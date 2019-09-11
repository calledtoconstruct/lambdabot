{  pkgs ? import ./nixpkgs.nix {} }:
with pkgs;

let 
  drv = import ./default.nix {};
  projectFolders =  lib.filterAttrs (k: v: v == "directory" && (lib.hasPrefix drv.pname k)) (builtins.readDir ./.);
  script = lib.concatStrings ( lib.mapAttrsToList (n: v: ''
  echo 'running cabal2nix into project.nix for ./${n}/'
  cd ./${n}
  ${pkgs.cabal2nix}/bin/cabal2nix . > project.nix
  cd ..
'') projectFolders);
in writeShellScriptBin "cabal2nix-all" script
