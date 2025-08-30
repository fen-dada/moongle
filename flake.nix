{
  description = "My flake template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = inputs @ {
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        devPkgs = with pkgs; [
          cmake
          ninja
          gnumake
          zlib
          postgresql_17
          pkg-config

          haskell.compiler.ghc912
          haskell.packages.ghc912.haskell-language-server
          ormolu
          cabal-install

          elmPackages.elm
          elmPackages.elm-format
          elmPackages.elm-language-server
          elmPackages.elm-test
          elmPackages.elm-review
          elmPackages.elm-live
          elmPackages.nodejs
        ];

        hsPkgs = pkgs.haskell.packages.ghc912;
        moongle-backend = hsPkgs.callCabal2nix "moongle-backend" ./. {};
        moongle-frontend = pkgs.buildNpmPackage {
          pname = "moongle-frontend";
          version = "0.1.0";
          src = ./web;

          npmDepsHash = "sha256-ak/d2p20p+bT2iDl4JEbdLlZuo03iwLJeXFOT0//Sy0=";

          nativeBuildInputs = with pkgs; [
            tree-sitter
          ];

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -r dist/* $out/
            runHook postInstall
          '';
        };

        devEnv = {
          # NODE_OPTIONS = "--openssl-legacy-provider";
          # CHOKIDAR_USEPOLLING = 1;
        };
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          config = {};
        };

        packages = {
          inherit moongle-backend moongle-frontend;
          default = moongle-backend;
        };

        devShells = {
          default = pkgs.mkShell {
            env = devEnv;
            packages = devPkgs;
          };

          clang = pkgs.mkShell.override {stdenv = pkgs.clangStdenv;} {
            env = devEnv;
            packages = devPkgs;
          };
        };
      };
    };
}
