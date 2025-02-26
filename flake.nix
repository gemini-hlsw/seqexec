{
  inputs = {
    typelevel-nix.url = "github:typelevel/typelevel-nix";
    nixpkgs.follows = "typelevel-nix/nixpkgs";
    flake-utils.follows = "typelevel-nix/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, typelevel-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs-x86_64 = import nixpkgs {
            system = "x86_64-darwin";
        };
        scala-cli-overlay = final: prev: {
            scala-cli = pkgs-x86_64.scala-cli;
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ typelevel-nix.overlays.default scala-cli-overlay];
        };
        pkgs2 = import (fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/6babc092caf5ed6744d5eb49f7d233dbb3c4f1ef.tar.gz";
          sha256 = "1rnijpph8r8xmc2xi1lvkwqmzjc2fgzndg6f7zr95hi1zchjgj9g";
        }) {inherit system;};
      in
      {
        devShell = pkgs.devshell.mkShell {
          imports = [ typelevel-nix.typelevelShell ];
          packages = [
            pkgs.nodePackages.vscode-langservers-extracted
            pkgs.nodePackages.prettier
            pkgs.nodePackages.node-gyp
            pkgs.nodePackages.yarn
          ];
          typelevelShell = {
            nodejs.enable = true;
            nodejs.package = pkgs2.nodejs-16_x;
            jdk.package = pkgs.jdk17;
          };
          env = [
            {
              "name" = "NODE_OPTIONS";
              "value" = "--openssl-legacy-provider --max-old-space-size=4096";
            }
          ];
        };
      }

    );
}
