let
  onlyBuild = drv:
    pkgs.haskell.lib.overrideCabal drv (drv: {
      doBenchmark = false;
      doCheck = false;
      doCoverage = false;
      doHaddock = false;
    });

  src =
    pkgs.nix-gitignore.gitignoreSource [
      ".git/"
      "/default.nix"
      "/shell.nix"
    ] ./.;

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          extension = haskellPackagesNew: haskellPackagesOld: {
            converge =
              haskellPackagesNew.callCabal2nix "converge" src {};
            fused-effects =
              onlyBuild (haskellPackagesNew.callPackage ./nix/haskell-packages/fused-effects.nix {});
            relude =
              onlyBuild (haskellPackagesNew.callPackage ./nix/haskell-packages/relude.nix {});
          };
        in
          pkgsNew.lib.composeExtensions
            (old.overrides or (_: _: {}))
            extension;
    });
  };

  pkgs =
    import ./nix/nixpkgs.nix {
      overlays = [ overlay ];
      config = {};
    };

  linuxPkgs =
    import ./nix/nixpkgs.nix {
      system = "x86_64-linux";
      overlays = [ overlay ];
      config = {};
    };

  converge = pkgs.haskellPackages.converge;

  executable = pkgs.haskell.lib.justStaticExecutables converge;

  # To load into Docker and run as a container:
  # docker load --input $(nix-build --no-link -A dockerImage)
  # docker run --interactive --tty --publish "8080:8080" --rm converge:latest
  dockerImage =
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

  shell =
    pkgs.haskellPackages.converge.env.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs ++ [
        cabal-install
        ghcid
        hlint
      ];
    });
in
  { inherit
      converge
      executable
      dockerImage
      shell
    ;
  }
