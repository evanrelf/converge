{ callCabal2nix, nix-gitignore, ... }:

let
  src =
    nix-gitignore.gitignoreSource [
      ".git/"
      "/nix/"
      "/*.nix"
    ] ../../.;
in
  callCabal2nix "converge" src {}
