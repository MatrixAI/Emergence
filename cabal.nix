{ mkDerivation, aeson, aeson-casing, base, containers, hpack, runc
, stdenv, typed-process
}:
mkDerivation {
  pname = "emergence";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base containers typed-process
  ];
  libraryToolDepends = [ hpack runc ];
  executableHaskellDepends = [
    aeson aeson-casing base containers typed-process
  ];
  executableToolDepends = [ runc ];
  testHaskellDepends = [
    aeson aeson-casing base containers typed-process
  ];
  testToolDepends = [ runc ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Emergence#readme";
  license = stdenv.lib.licenses.asl20;
}
