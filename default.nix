{ mkDerivation, aeson, attoparsec, base, bytestring, clock, dhall
, disk-free-space, either, hpack, http-client, http-client-tls
, http-types, mtl, safe, safe-exceptions, stdenv, stm, text, time
, transformers
}:
mkDerivation {
  pname = "statum-metrics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring clock dhall disk-free-space either
    http-client http-client-tls http-types mtl safe safe-exceptions stm
    text time transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring clock dhall disk-free-space either
    http-client http-client-tls http-types mtl safe safe-exceptions stm
    text time transformers
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring clock dhall disk-free-space either
    http-client http-client-tls http-types mtl safe safe-exceptions stm
    text time transformers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/prasmussen/statum-metrics#readme";
  license = stdenv.lib.licenses.bsd3;
}
