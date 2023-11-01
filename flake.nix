{
  description = "A solution for 3D applicaions";

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
    };
  in 
  {

    devShells.${system}.default = pkgs.mkShell rec {
      name = "FCS";
      buildInputs = with pkgs; [
        nodejs_16
        gnome.gnome-terminal
        bashInteractive
        dotnet-sdk

        (vscode-with-extensions.override  {
          vscode = pkgs.vscode;
          vscodeExtensions = with pkgs.vscode-extensions; [
            jnoortheen.nix-ide
            mhutchie.git-graph
            ms-dotnettools.csharp
            ionide.ionide-fsharp
          ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "vscode-edit-csv";
              publisher = "janisdd";
              version = "0.8.2";
              sha256 = "sha256-DbAGQnizAzvpITtPwG4BHflUwBUrmOWCO7hRDOr/YWQ=";
            }
          ];
        })
      ];

      shellHook = ''
        export PS1+="${name}> "
        echo "Welcome to the Fallen Character Sheet Shell"
      '';
    };
  }; 

}

