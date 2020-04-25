{ mkDerivation, base, Cabal, cabal-doctest, containers, doctest
, ghc, ghc-tcplugins-extra, hspec, hspec-discover
, inspection-testing, polysemy, should-not-typecheck, stdenv, syb
, transformers
}:
mkDerivation {
  pname = "polysemy-plugin";
  version = "0.2.5.0";
  sha256 = "50cb4d72dad042e60b90e7dc34364bb9db5e4ad0b3afb99f4a941576eff0b9fa";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base containers ghc ghc-tcplugins-extra polysemy syb transformers
  ];
  testHaskellDepends = [
    base containers doctest ghc ghc-tcplugins-extra hspec
    inspection-testing polysemy should-not-typecheck syb transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/isovector/polysemy#readme";
  description = "Disambiguate obvious uses of effects";
  license = stdenv.lib.licenses.bsd3;
}
