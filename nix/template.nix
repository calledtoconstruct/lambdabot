(import ./nixpkgs.nix {}).writeShellScriptBin "template-haskell" ''
  echo 'creating nix files based on the ${(import ./. {}).pname} project'
  cp -rf ${../nix} nix
  cp -f ${../default.nix} default.nix
  cp -f ${../shell.nix} shell.nix
''
