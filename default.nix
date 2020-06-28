let
  haskellPackagesOverlay =
    import ./nix/override-haskell-packages.nix {
      packages = {
        "converge" = pkgs.nix-gitignore.gitignoreSource [ ./.nixignore ] ./.;
        "generic-lens-core" = "2.0.0.0";
        "generic-optics" = "2.0.0.0";
        "github-webhooks" = "0.13.0";
        "polysemy" = "1.3.0.0";
        "polysemy-plugin" = "0.2.5.0";
        "relude" = "0.6.0.0";
        "servant-github-webhook" = "0.4.2.0";
        "string-interpolate" = "0.2.0.0";
      };
      overrides = {
        "relude" = oldCabal: {
          patches = (oldCabal.patches or []) ++ [ ./nix/patches/relude.patch ];
        };
        "servant-github-webhook" = oldCabal: {
          src = builtins.fetchTarball {
            url = "https://github.com/tsani/servant-github-webhook/archive/89574d30a3bce1411a03d7f8e21606125f4859cb.tar.gz";
            sha256 = "1bqj3p5wrzb5lmcsbyy6qpb2m6yw7jd4zd7ndgb5h0madl0j2l81";
          };
          patches = (oldCabal.patches or []) ++ [ ./nix/patches/servant-github-webhook.patch ];
        };
      };
      hackage = {
        rev = "76457479e5fa2c98f2b1b12d5213ae82524973d2";
        sha256 = "1c05r6cnn987da3n8fr7ny9h0abqdbwmn075y7vyag7166wx02l6";
      };
    };


  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };


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
