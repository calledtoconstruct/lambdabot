{ mkDerivation, base, binary, bytestring, containers, dice
, directory, lambdabot-core, misfortune, mtl, process, random-fu
, regex-tdfa, split, stdenv, zlib
}:
mkDerivation {
  pname = "lambdabot-points-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers dice directory lambdabot-core
    misfortune mtl process random-fu regex-tdfa split zlib
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Points plugins for Lambdabot";
  license = "GPL";
}
