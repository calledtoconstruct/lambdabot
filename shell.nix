{ pkgs ? import ./nixpkgs.nix {} }:
let 
  drv = import ./.;
  hie = import ./hie.nix;

  hie-tools = with pkgs.haskellPackages; 
    [ cabal-install apply-refact hie hasktags hlint hoogle brittany ]; 
  
  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends =
      (drv'.buildDepends or []) ++
      [ (pkgs.haskell.packages.ghc865.hoogleLocal {
          packages =
            (drv'.libraryHaskellDepends or []) ++
            (drv'.executableHaskellDepends or []) ++
            (drv'.testHaskellDepends or []) ;
        })
        pkgs.cabal-install
        (import ./vscode.nix { inherit pkgs; })
      ];
    buildTools = (drv'.buildTools or []) ++ hie-tools;
  });
in shellDrv.env
