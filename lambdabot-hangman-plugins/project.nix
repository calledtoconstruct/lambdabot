{ mkDerivation, base, binary, bytestring, containers, dice
, directory, lambdabot-core, misfortune, mtl, process, random-fu
, regex-tdfa, split, stdenv
}:
mkDerivation {
  pname = "lambdabot-hangman-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers dice directory lambdabot-core
    misfortune mtl process random-fu regex-tdfa split
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Hangman plugins for Lambdabot";
  license = "GPL";
}
