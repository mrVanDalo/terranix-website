{ pkgs, ... }:
let
  hcloud-modules = pkgs.fetchgit {
    url = "https://github.com/mrVanDalo/terranix-hcloud.git";
    rev = "5fa359a482892cd973dcc6ecfc607f4709f24495";
    sha256 = "0smgmdiklj98y71fmcdjsqjq8l41i66hs8msc7k4m9dpkphqk86p";
  };
in { imports = [ (toString hcloud-modules) ]; }
