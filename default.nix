{ mkDerivation, attoparsec, base, clock, either, hpack, mtl, safe
, safe-exceptions, stdenv, stm, text, time, transformers
}:
mkDerivation {
  pname = "statum-metrics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base clock either mtl safe safe-exceptions stm text time
    transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    attoparsec base clock either mtl safe safe-exceptions stm text time
    transformers
  ];
  testHaskellDepends = [
    attoparsec base clock either mtl safe safe-exceptions stm text time
    transformers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/prasmussen/statum-metrics#readme";
  license = stdenv.lib.licenses.bsd3;
}
