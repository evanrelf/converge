let
  # nixos-19.09 on 2020-01-11
  rev = "9f453eb97ffe261ff93136757cd08b522fac83b7";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "16wdsazc7g09ibcxlqsa3kblzhbbpdpb6s29llliybw73cp37b9s";
  };
in
  import nixpkgs
