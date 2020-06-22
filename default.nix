let
  haskellPackagesOverlay = import ./nix/haskell-packages;


  pkgs =
    import ./nix/nixpkgs.nix {
      overlays = [ haskellPackagesOverlay ];
    };


  converge = pkgs.haskellPackages.converge;


  executable = pkgs.haskell.lib.justStaticExecutables converge;


  # To load into Docker and run as a container:
  # docker load --input $(nix-build --no-link --attr dockerImage)
  # docker run --interactive --tty --publish "7777:7777" --rm converge:latest
  dockerImage =
    let
      linuxPkgs =
        import ./nix/nixpkgs.nix {
          system = "x86_64-linux";
          overlays = [ haskellPackagesOverlay ];
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
          ExposedPorts = { "7777" = {}; };
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
