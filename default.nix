let
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
              pkgsNew.haskell.lib.dontCheck
                (haskellPackagesNew.callPackage ./nix/haskell-packages/fused-effects.nix {});
            github =
              haskellPackagesNew.callPackage ./nix/haskell-packages/github.nix {};
            relude = with pkgsNew.haskell.lib;
              dontCheck
                (appendPatches
                  (haskellPackagesNew.callPackage ./nix/haskell-packages/relude.nix {})
                  [ ./nix/haskell-packages/relude.patch ]);
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
  # docker load --input $(nix-build --no-link --attr dockerImage)
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
    converge.env.overrideAttrs (old: {
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
