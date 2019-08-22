
{
  nixpkgs   ? import ./nixpkgs.nix {}
, version   ? "ghc864"
}:

with nixpkgs;

let
    inherit (nixpkgs)
      fetchFromGitHub
      writeShellScriptBin
      buildEnv
      runCommand
      makeWrapper
      lib
      glibcLocales
      figlet;

    vscode = vscode-with-extensions.override {
      # When the extension is already available in the default extensions set.
      vscodeExtensions = with vscode-extensions;
      [
        bbenoist.Nix
      ]
      ++
      # Concise version from the vscode market place when not available in the default set.
      vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "theme-dracula-at-night";
          publisher = "bceskavich";
          version = "2.5.0";
          sha256 = "03wxaizmhsq8k1mlfz246g8p8kkd21h2ajqvvcqfc78978cwvx3p";
        }
        {
          name = "vsc-material-theme";
          publisher = "equinusocio";
          version = "29.2.0";
          sha256 = "192x2rblyxgwilyk54anpplxqqwza9445rxsqw78v5ig64yv521j";
        }
        {
          name = "gitlens";
          publisher = "eamodio";
          version = "9.5.1";
          sha256 = "10s2g98wv8i0w6fr0pr5xyi8zmh229zn30jn1gg3m5szpaqi1v92";
        }
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
      ];
    };

    theCodeWrapper = runCommand "${vscode.name}" { nativeBuildInputs = [ makeWrapper ]; } ''
      mkdir -p $out/bin
      makeWrapper \
        ${vscode}/bin/code \
        $out/bin/code \
        --prefix PATH : ${lib.makeBinPath [ cabal-install ]}
    '';

in {
  name = "vscode-with-hie-${version}";
  codeWrapper = theCodeWrapper;
}
