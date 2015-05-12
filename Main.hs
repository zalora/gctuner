{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Attoparsec.Text.Lazy as AP
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.IO         as TIO
import qualified Data.Time                 as DT

import           Control.Applicative       ((<|>))
import           Data.Time.Format          (defaultTimeLocale, parseTimeM)
import           System.IO                 (hPutStrLn, stderr)


data LogEntry = LogEntry
    { leTimestamp        :: DT.ZonedTime
    , leSecondsFromStart :: Double
    , leNewSizeBefore    :: Integer
    , leNewSizeAfter     :: Integer
    , leNewSizeMax       :: Integer
    , leTotalSizeBefore  :: Integer
    , leTotalSizeAfter   :: Integer
    , leTotalSizeMax     :: Integer
    } deriving Show

type Log = [LogEntry]

data ReferenceType =
    SoftReference
  | WeakReference
  | FinalReference
  | PhantomReference
  | JniWeakReference
    deriving Show

data Reference = Reference
    { rType  :: ReferenceType
    , rCount :: Maybe Integer
    , rTime  :: Double
    } deriving Show

data Stats = Stats
    { sNewGenUsage :: Integer
    }


timeParser :: AP.Parser DT.ZonedTime
timeParser = do
    timeString <- AP.count (length example) AP.anyChar
    parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" timeString
    where
        example :: String
        example = "2015-05-08T22:39:43.843+0800"

referenceTypeParser :: AP.Parser ReferenceType
referenceTypeParser =
     ("FinalReference"     >> return FinalReference)
 <|> ("JNI Weak Reference" >> return JniWeakReference)
 <|> ("PhantomReference"   >> return PhantomReference)
 <|> ("SoftReference"      >> return SoftReference)
 <|> ("WeakReference"      >> return WeakReference)

referenceParser :: AP.Parser Reference
referenceParser = do
    _ <- AP.double <* ": ["
    type_ <- referenceTypeParser <* ", "
    count <- justCount <|> return Nothing
    time <- AP.double <* " secs]"
    return Reference
        { rType = type_
        , rCount = count
        , rTime = time
        }
    where
        justCount = do
            count <- AP.decimal <* " refs, "
            return $ Just count

logEntryParser :: AP.Parser LogEntry
logEntryParser = do
    timestamp <- timeParser <* ": "
    secondsFromStart <- AP.double <* ": [GC "
    -- FIXME: obviously this only supports ParNew for now
    _ <- AP.double <* ": [ParNew"
    _ <- AP.many' referenceParser <* ": "
    newSizeBefore <- AP.decimal <* "K->"
    newSizeAfter <- AP.decimal <* "K("
    newSizeMax <- AP.decimal <* "K), "
    _ <- AP.double <* " secs] "
    totalSizeBefore <- AP.decimal <* "K->"
    totalSizeAfter <- AP.decimal <* "K("
    totalSizeMax <- AP.decimal <* "K), "
    AP.skipWhile (not . AP.isEndOfLine)
    AP.option () AP.endOfLine
    return LogEntry
        { leTimestamp = timestamp
        , leSecondsFromStart = secondsFromStart
        , leNewSizeBefore = newSizeBefore
        , leNewSizeAfter = newSizeAfter
        , leNewSizeMax = newSizeMax
        , leTotalSizeBefore = totalSizeBefore
        , leTotalSizeAfter = totalSizeAfter
        , leTotalSizeMax = totalSizeMax
        }

logParser :: AP.Parser Log
logParser = AP.many' logEntryParser

headRoom :: Double
headRoom = 0.5

-- New Gen = Eden + Survivor
-- By default Eden / Survivor = 8
edenRatio :: Double
edenRatio = 8 / 9

analyse :: Log -> Stats
analyse entries = Stats $ maximum $ map leNewSizeAfter entries

recommend :: Stats -> IO ()
recommend stats = do
    hPutStrLn stderr $ "Maximum New Gen usage after GC: " ++ show (sNewGenUsage stats) ++ "K"
    putStrLn $ "-XX:NewSize=" ++ show roundedSize ++ "K"
    where
        ratio = (1 + headRoom) / edenRatio
        recommendedSize = (*) ratio $ realToFrac $ sNewGenUsage stats
        roundedSize = (^) 2 $ ceiling $ logBase 2 recommendedSize

main :: IO ()
main = do
    logString <- TIO.readFile "gc.log"
    either print process $ AP.parseOnly logParser $ T.toStrict $ filterLog logString
    where
        filterLog = T.unlines . filter (T.isInfixOf "[ParNew") . T.lines
        process = recommend . analyse
