{ pkgs ? import (builtins.fetchTarball {
      name = "nixos-23.11_2024-07-09";
      url = "https://github.com/nixos/nixpkgs/archive/205fd4226592cc83fd4c0885a3e4c9c400efabb5.tar.gz";
      sha256 = "sha256:1f5d2g1p6nfwycpmrnnmc2xmcszp804adp16knjvdkj8nz36y1fg";
    }) {}
}:
let compilers = [ "ghc884" "ghc8107"];
in pkgs.lib.genAttrs compilers (c: pkgs.haskell.packages."${c}".callCabal2nix "haveibeenpwned" ./. {})
