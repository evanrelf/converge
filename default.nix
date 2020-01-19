let
  src = pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./.;

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: rec {
          relude = new.callPackage ./nix/relude.nix {};
          converge = new.callCabal2nix "converge" src {};
        };
      };
    };
  };

  pkgs = import ./nix/nixpkgs.nix { inherit config; };

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
          linuxPkgs.haskell.lib.justStaticExecutables (linuxPkgs.haskellPackages.callCabal2nix "converge" src {});
        config = {
          Cmd = "/bin/converge";
          ExposedPorts = {
            "8080/tcp" = {};
          };
        };
      };
in
  rec {
    converge = pkgs.haskellPackages.converge;

    inherit dockerImage;

    shell = converge.env.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs ++ [
        cabal-install
        ghcid
        hlint
      ];
    });
  }
