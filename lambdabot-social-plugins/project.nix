{ mkDerivation, base, binary, bytestring, containers
, lambdabot-core, mtl, split, stdenv, time
}:
mkDerivation {
  pname = "lambdabot-social-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers lambdabot-core mtl split time
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Social plugins for Lambdabot";
  license = "GPL";
}
