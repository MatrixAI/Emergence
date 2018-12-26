{ mkDerivation, aeson, aeson-casing, base, bytestring, containers
, crypto-multihash, cryptohash-sha256, directory, filepath, hpack
, libnix, runc, stdenv, text, typed-process
}:
mkDerivation {
  pname = "emergence";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring containers crypto-multihash
    cryptohash-sha256 directory filepath libnix text typed-process
  ];
  libraryToolDepends = [ hpack runc ];
  executableHaskellDepends = [
    aeson aeson-casing base bytestring containers crypto-multihash
    cryptohash-sha256 directory filepath libnix text typed-process
  ];
  executableToolDepends = [ runc ];
  testHaskellDepends = [
    aeson aeson-casing base bytestring containers crypto-multihash
    cryptohash-sha256 directory filepath libnix text typed-process
  ];
  testToolDepends = [ runc ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Emergence#readme";
  license = stdenv.lib.licenses.asl20;
}
