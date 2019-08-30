{ mkDerivation, array, arrows, base, bytestring, containers
, data-memocombinators, directory, filepath
, haskell-src-exts-simple, hoogle, HTTP, IOSpec, lambdabot-core
, lambdabot-reference-plugins, lambdabot-trusted-plugins
, lifted-base, logict, MonadRandom, mtl, mueval, network, numbers
, oeis, parsec, pretty, process, QuickCheck, regex-tdfa, show
, split, stdenv, syb, transformers, utf8-string, vector-space
}:
mkDerivation {
  pname = "lambdabot-haskell-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    array arrows base bytestring containers data-memocombinators
    directory filepath haskell-src-exts-simple hoogle HTTP IOSpec
    lambdabot-core lambdabot-reference-plugins
    lambdabot-trusted-plugins lifted-base logict MonadRandom mtl mueval
    network numbers oeis parsec pretty process QuickCheck regex-tdfa
    show split syb transformers utf8-string vector-space
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Lambdabot Haskell plugins";
  license = "GPL";
}
