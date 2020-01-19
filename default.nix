{ system ? builtins.currentSystem
, compiler ? "ghc865"
}:

let
  onlyBuild = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
    doBenchmark = false;
    doCheck = false;
    doCoverage = false;
    doHaddock = false;
  });

  src = pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./.;

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskell.packages.${compiler}.override {
          overrides = new: old: rec {
            converge = new.callCabal2nix "converge" src {};
            fused-effects =
              onlyBuild (new.callPackage ./nix/fused-effects.nix {});
            relude = onlyBuild (new.callPackage ./nix/relude.nix {});
          };
        };
    };
  };

  pkgs = import ./nix/nixpkgs.nix { inherit config; inherit system; };

  dockerImage =
    # docker load --input $(nix-build --no-link -A dockerImage)
    # docker run --interactive --tty --rm converge:latest
    let
      linuxPkgs =
        import ./nix/nixpkgs.nix { inherit config; system = "x86_64-linux"; };
    in
      linuxPkgs.dockerTools.buildImage {
        name = "converge";
        tag = "latest";
        contents =
          linuxPkgs.haskell.lib.justStaticExecutables
            linuxPkgs.haskellPackages.converge;
        config = {
          Entrypoint = "/bin/converge";
          ExposedPorts = { "8080" = {}; };
        };
      };

  shell = pkgs.haskellPackages.converge.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
      hlint
    ];
  });
in
  rec {
    converge = pkgs.haskellPackages.converge;
    convergeBin = pkgs.haskell.lib.justStaticExecutables converge;
    inherit dockerImage;
    inherit shell;
  }
