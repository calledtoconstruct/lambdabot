{ pkgs ? import ./nixpkgs.nix }:
let 
  #VSCode Settings file
  vscodeSettingsFile = builtins.toString ../.vscode/settings.json;
  #uses jq to beautify the json coming out of builtins.toJSON
  vscodeUpdateSettingsCmd = _nixpkgs : with _nixpkgs;
  writeShellScriptBin "vscodeNixUpdateSettings" (lib.optionalString (lib.pathExists vscodeSettingsFile) ''
    echo '====================================='
    echo 'vscode-presonal.nix :  Updating ${vscodeSettingsFile}'
    echo '${updateVSCodeSettings _nixpkgs}' | ${jq}/bin/jq .  > ${vscodeSettingsFile}
  '');
  #this command should be passed to the shell enviorment and triggered from shellHook.
  updateVSCodeSettings =  _nixpkgs:
    builtins.toJSON (
      (builtins.fromJSON (builtins.readFile vscodeSettingsFile)) // 
      #Here you can overwrite vscode configuration in settings.json
      { 
        # "java.home" = "${_jdk}"; 
        # "maven.executable.path" = "${_sdk}/apache-maven-3.5.4/bin/mvn";
      }
    );
in
{
  inherit vscodeUpdateSettingsCmd;
  extensions = [
        {
          name = "theme-dracula-at-night";
          publisher = "bceskavich";
          version = "2.5.0";
          sha256 = "03wxaizmhsq8k1mlfz246g8p8kkd21h2ajqvvcqfc78978cwvx3p";
        }
        {
          name = "material-icon-theme";
          publisher = "pkief";
          version = "3.9.0";
          sha256 = "10sqdy42zdckji93rc1rxvnlwic6ykxpyjj785qgkncw8n4ysd5g";
        }
        {
          name = "gitlens";
          publisher = "eamodio";
          version = "9.5.1";
          sha256 = "10s2g98wv8i0w6fr0pr5xyi8zmh229zn30jn1gg3m5szpaqi1v92";
        }
      ];
    }
