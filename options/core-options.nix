{ pkgs, ... }:
let
  terranix = pkgs.fetchgit {
    url = "https://github.com/mrVanDalo/terranix.git";
    rev = "2.2.3";
    sha256 = "0r7n0c1m81rz22x2bc3kkw63xs3cf8jbfpr73vplnc3yyngkrjxp";
  };
in {
  imports = [ "${terranix}/modules" "${terranix}/core/terraform-options.nix" ];
}
