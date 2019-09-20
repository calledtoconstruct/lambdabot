{ mkDerivation, base, binary, bytestring, containers, dice
, directory, extra, lambdabot-core, lifted-base, misfortune
, monad-control, mtl, process, random, random-fu, regex-tdfa, split
, stdenv, universe-base
}:
mkDerivation {
  pname = "lambdabot-hangman-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers dice directory extra
    lambdabot-core lifted-base misfortune monad-control mtl process
    random random-fu regex-tdfa split universe-base
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Hangman plugins for Lambdabot";
  license = "GPL";
}
