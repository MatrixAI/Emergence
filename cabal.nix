{ mkDerivation, aeson, base, containers, hpack, runc, stdenv
, typed-process
}:
mkDerivation {
  pname = "emergence";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base containers typed-process ];
  libraryToolDepends = [ hpack runc ];
  executableHaskellDepends = [ aeson base containers typed-process ];
  executableToolDepends = [ runc ];
  testHaskellDepends = [ aeson base containers typed-process ];
  testToolDepends = [ runc ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Emergence#readme";
  license = stdenv.lib.licenses.asl20;
}
