{ mkDerivation, base, hpack, stdenv, typed-process }:
mkDerivation {
  pname = "emergence";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base typed-process ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base typed-process ];
  testHaskellDepends = [ base typed-process ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Emergence#readme";
  license = stdenv.lib.licenses.asl20;
}
