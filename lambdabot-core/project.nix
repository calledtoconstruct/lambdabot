{ mkDerivation, base, binary, bytestring, containers, dependent-map
, dependent-sum, dependent-sum-template, directory, edit-distance
, filepath, haskeline, hslogger, HTTP, lifted-base, monad-control
, mtl, network, parsec, prim-uniq, random, random-fu, random-source
, regex-tdfa, SafeSemaphore, split, stdenv, syb, template-haskell
, time, transformers, transformers-base, unix, utf8-string, zlib
}:
mkDerivation {
  pname = "lambdabot-core";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers dependent-map dependent-sum
    dependent-sum-template directory edit-distance filepath haskeline
    hslogger HTTP lifted-base monad-control mtl network parsec
    prim-uniq random random-fu random-source regex-tdfa SafeSemaphore
    split syb template-haskell time transformers transformers-base unix
    utf8-string zlib
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Lambdabot core functionality";
  license = "GPL";
}
