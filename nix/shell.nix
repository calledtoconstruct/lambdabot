let
  pkgs = import ./nixpkgs.nix {};
  compiler = import ./compiler.nix { inherit pkgs; };
  drv = import ./. { inherit compiler; };
  vscode = import ./vscode.nix { inherit pkgs; };

  tools-used-by-hie = with pkgs.haskellPackages; [
    Cabal_2_4_1_0
    cabal-install
    apply-refact
    hasktags
    hlint
    hoogle
    brittany
  ]; 
  
  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends = (drv'.buildDepends or []) ++ [
      (compiler.hoogleLocal {
        packages = (drv'.libraryHaskellDepends or []) ++
          (drv'.executableHaskellDepends or []) ++
          (drv'.testHaskellDepends or []) ;
      })
    ];
    buildTools = (drv'.buildTools or []) ++ tools-used-by-hie;
  });
in shellDrv.env.overrideAttrs ( shellEnv: {
  buildInputs = shellEnv.buildInputs ++ [
    pkgs.figlet
    vscode
    (import ./cabal2nix-all.nix {})
  ];
  shellHook = ''
    figlet ${drv.pname}
  '';
} )
