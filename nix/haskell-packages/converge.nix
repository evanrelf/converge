{ callCabal2nix, pkgs, ... }:

let
  src =
    pkgs.nix-gitignore.gitignoreSource [
      ".git/"
      "/nix/"
      "/*.nix"
    ] ../../.;
in
  callCabal2nix "converge" src {}
