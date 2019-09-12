{ pkgs            ? import ./nixpkgs.nix{} 
, hie             ? import ./hie.nix 
, vscode-personal ? import ./vscode-personal.nix { inherit pkgs; }
}:
let extensionsFromVscodeMarketplace = [
  
  ] ++ vscode-personal.extensions;
  vscode = 
  pkgs.vscode-with-extensions.override 
  
  ({
    # When the extension is already available in the default extensions set.
    vscodeExtensions = with pkgs.vscode-extensions;
    [
      bbenoist.Nix
    ]
    ++
    # Concise version from the vscode market place when not available in the default set.
    pkgs.vscode-utils.extensionsFromVscodeMarketplace ([
      {
        name = "vscode-hie-server";
        publisher = "alanz";
        version = "0.0.28";
        sha256 = "1gfwnr5lgwdgm6hs12fs1fc962j9hirrz2am5rmhnfrwjgainkyr";
      }
      {
        name = "language-haskell";
        publisher = "justusadam";
        version = "2.6.0";
        sha256 = "1891pg4x5qkh151pylvn93c4plqw6vgasa4g40jbma5xzq8pygr4";
      }
      {
        name = "brittany";
        publisher = "MaxGabriel";
        version = "0.0.6";
        sha256 = "1v47an1bad5ss4j5sajxia94r1r4yfyvbim5wax4scr0d5bdgv54";
      }
    ] ++ vscode-personal.extensions);
  });
in pkgs.buildEnv { 
  name = "vscodeEnv"; 
  paths = [ vscode hie (vscode-personal.vscodeUpdateSettingsCmd pkgs) ]; 
}
