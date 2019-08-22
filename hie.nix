{
    nixpkgs   ? import ./nixpkgs.nix {}
,   version   ? "ghc864"
,   compiler  ? import ./compiler.nix { inherit nixpkgs version; }
}:
let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
    hie = (all-hies.selection { selector = p: { inherit (p) ghc864; }; });
}
  