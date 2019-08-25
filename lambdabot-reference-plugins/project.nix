{ mkDerivation, base, bytestring, containers, HTTP, lambdabot-core
, mtl, network, network-uri, oeis, process, regex-tdfa, split
, stdenv, tagsoup, utf8-string
}:
mkDerivation {
  pname = "lambdabot-reference-plugins";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers HTTP lambdabot-core mtl network
    network-uri oeis process regex-tdfa split tagsoup utf8-string
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Lambdabot reference plugins";
  license = "GPL";
}
