{ mkDerivation, base, binary, bytestring, containers, dice
, directory, lambdabot-core, misfortune, mtl, process, random-fu
, regex-tdfa, stdenv
}:
mkDerivation {
  pname = "lambdabot-suggest-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers dice directory lambdabot-core
    misfortune mtl process random-fu regex-tdfa
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Suggest plugins for Lambdabot";
  license = "GPL";
}
