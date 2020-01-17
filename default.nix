let
  src = pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./.;

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: rec {
          converge = new.callCabal2nix "converge" src {};
        };
      };
    };
  };

  pkgs = import ./nix/nixpkgs.nix { inherit config; };
in
  rec {
    converge = pkgs.haskellPackages.converge;

    shell = converge.env.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs ++ [
        cabal-install
        ghcid
        hlint
      ];
    });
  }
