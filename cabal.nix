{ mkDerivation, base, hpack, runc, stdenv, typed-process }:
mkDerivation {
  pname = "emergence";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base typed-process ];
  libraryToolDepends = [ hpack runc ];
  executableHaskellDepends = [ base typed-process ];
  executableToolDepends = [ runc ];
  testHaskellDepends = [ base typed-process ];
  testToolDepends = [ runc ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Emergence#readme";
  license = stdenv.lib.licenses.asl20;
}
