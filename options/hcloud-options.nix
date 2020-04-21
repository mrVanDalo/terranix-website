{ pkgs, ... }:
let
  hcloud-modules = pkgs.fetchgit {
    url = "https://github.com/mrVanDalo/terranix-hcloud.git";
    rev = "c3571f76664e1813f90d97b8c194a1e0149e895e";
    sha256 = "0plld74wincyy3c5gdfqh78pzrqibxh6r839dm0c717fajr9imwb";
  };
in { imports = [ (toString hcloud-modules) ]; }
