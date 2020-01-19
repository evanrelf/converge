let
  pkgs = import ./nix/nixpkgs.nix { config = {}; };

  onlyBuild = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
    doBenchmark = false;
    doCheck = false;
    doCoverage = false;
    doHaddock = false;
  });

  haskellPackages = pkgs.haskellPackages.override {
    overrides = new: old: {
      fused-effects = onlyBuild (new.callPackage ./nix/fused-effects.nix {});
      relude = onlyBuild (new.callPackage ./nix/relude.nix {});
    };
  };

  converge =
    let
      src = pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./.;
    in
      haskellPackages.callCabal2nix "converge" src {};

  dockerImage =
    # TODO:
    # 1. Figure out the idiomatic way of specifying a build or host platform
    # 2. Make sure both the Docker image and the contents are built on Linux
    # 3. Figure out how to prevent subsequent overrides from clobbering
    #    eachother (see https://github.com/NixOS/nixpkgs/issues/26561)
    #
    # docker load -i (nix-build --no-link -A dockerImage) && docker run -it --rm converge:latest
    let
      linuxPkgs = pkgs.forceSystem "x86_64-linux" "x86_64";
    in
      linuxPkgs.dockerTools.buildImage {
        name = "converge";
        tag = "latest";
        contents =
          linuxPkgs.haskell.lib.justStaticExecutables converge;
        config = {
          Cmd = "/bin/converge";
          ExposedPorts = {
            "8080/tcp" = {};
          };
        };
      };

  shell = converge.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
      hlint
    ];
  });
in
  { inherit converge;
    inherit dockerImage;
    inherit shell;
  }
