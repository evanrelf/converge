{ callCabal2nix, nix-gitignore, ... }:

let
  src = nix-gitignore.gitignoreSource [ ../../.nixignore ] ../../.;
in
  callCabal2nix "converge" src {}
