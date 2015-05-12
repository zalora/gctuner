{ mkDerivation, attoparsec, base, stdenv, text, time }:
mkDerivation {
  pname = "gctuner";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ attoparsec base text time ];
  license = stdenv.lib.licenses.mpl20;
}
