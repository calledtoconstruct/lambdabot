{ mkDerivation, base, bytestring, containers, directory, filepath
, lambdabot-core, lifted-base, mtl, network, SafeSemaphore, split
, stdenv, time
}:
mkDerivation {
  pname = "lambdabot-irc-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory filepath lambdabot-core
    lifted-base mtl network SafeSemaphore split time
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "IRC plugins for lambdabot";
  license = "GPL";
}
