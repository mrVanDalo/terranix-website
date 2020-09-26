* [hcloud-options](#hcloud-options)
  * [with terraform version 0.11](#with-terraform-version-0.11)
  * [with terraform version 0.12](#with-terraform-version-0.12)
* [options](#options)

An opinionated 
[hetzner cloud](https://www.terraform.io/docs/providers/hcloud/index.html)
module.
Read more at 
[https://github.com/mrvandalo/terranix-hcloud](https://github.com/mrvandalo/terranix-hcloud)

## with terraform version 0.11

To use this module with terranix and terraform version 0.11

```nix
{ pkgs, ...}:
let
  hcloud-modules = pkgs.fetchgit{
    url = "https://github.com/mrVanDalo/terranix-hcloud.git";
    rev = "2ba5c46ebf48668691148a1458fa71925cff5316";
    sha256 = "0z9d28lp2wn56b9l80878vv1f28b0lxzdi870drd4mvmn37ib814";
  };
in
{ imports = [ (toString hcloud-modules) ]; }
```

## with terraform version 0.12

To use this module with terranix and terraform version 0.12

```nix
{ pkgs, ...}:
let
  hcloud-modules = pkgs.fetchgit{
    url = "https://github.com/mrVanDalo/terranix-hcloud.git";
    rev = "5fa359a482892cd973dcc6ecfc607f4709f24495";
    sha256 = "0smgmdiklj98y71fmcdjsqjq8l41i66hs8msc7k4m9dpkphqk86p";
  };
in
{ imports = [ (toString hcloud-modules) ]; }
```

# options

These options are available by the module
