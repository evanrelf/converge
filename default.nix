let
  haskellPackagesOverlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          extension =
            (pkgsNew.haskell.lib.packagesFromDirectory {
              directory = ./nix/haskell-packages;
            });
        in
          pkgsNew.lib.composeExtensions
            (old.overrides or (_: _: {}))
            extension;
    });
  };


  pkgs =
    import ./nix/nixpkgs.nix {
      overlays = [ haskellPackagesOverlay ];
      config = {};
    };


  converge = pkgs.haskellPackages.converge;


  executable = pkgs.haskell.lib.justStaticExecutables converge;


  # To load into Docker and run as a container:
  # docker load --input $(nix-build --no-link --attr dockerImage)
  # docker run --interactive --tty --publish "8080:8080" --rm converge:latest
  dockerImage =
    let
      linuxPkgs =
        import ./nix/nixpkgs.nix {
          system = "x86_64-linux";
          overlays = [ haskellPackagesOverlay ];
          config = {};
        };
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
