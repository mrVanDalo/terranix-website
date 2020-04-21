{pkgs, ... }:
let
  terranix = pkgs.fetchgit {
    url = "https://github.com/mrVanDalo/terranix.git";
    rev = "dfbf4d1fae08da8052ff880c5d02b2eb5857d54c";
    sha256 = "1qilbvldlq7ybxa3yx99hb8vbmj0sk5x9qqxa4f1czpzj2mja0fn";
  };
in {
  imports = [
    "${terranix}/modules"
    "${terranix}/core/terraform-options.nix"
  ];
}
