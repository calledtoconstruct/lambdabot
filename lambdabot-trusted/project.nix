{ mkDerivation, base, oeis, QuickCheck, QuickCheck-safe, stdenv }:
mkDerivation {
  pname = "lambdabot-trusted";
  version = "5.1.0.4";
  src = ./.;
  libraryHaskellDepends = [ base oeis QuickCheck QuickCheck-safe ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Lambdabot trusted code";
  license = "GPL";
}
