{
  description = "Moongle - Moonbit API Search Engine";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = inputs @ {flake-parts, ...}:
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

        hsOverlay = final: prev: {
          haskellPackages = prev.haskell.packages.ghc912.override {
            overrides = hself: hsuper: {
              language-moonbit = hself.callCabal2nix "language-moonbit" ./vendor/language-moonbit {};
              servant-effectful = hself.callCabal2nix "servant-effectful" ./vendor/servant-effectful {};

              postgresql-simple = final.haskell.lib.doJailbreak hsuper.postgresql-simple;
              wreq-effectful = final.haskell.lib.doJailbreak hsuper.wreq-effectful;
            };
          };
        };

        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [hsOverlay];
          config = {};
        };

        hp = pkgs.haskell.packages.ghc912;
        moongleRaw = hp.callCabal2nix "moongle" ./. {};
        moongle =
          pkgs.haskell.lib.dontCheck
          (pkgs.haskell.lib.dontHaddock moongleRaw);

        devEnv = {
          # NODE_OPTIONS = "--openssl-legacy-provider";
          # CHOKIDAR_USEPOLLING = 1;
        };
      in {
        _module.args.pkgs = pkgs;

        packages = {
          inherit moongle;
          default = moongle;
        };

        apps.moongle = {
          type = "app";
          program = "${moongle}/bin/moongle";
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
