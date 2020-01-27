* [hcloud-options](#hcloud-options)
  * [with terraform version 0.11](#with-terraform-version-0.11)
  * [with terraform version 0.12](#with-terraform-version-0.12)
* [options](#options)

<div class="alert alert-error">This page is under construction!</div>

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
    rev = "a020c6df7d5b301d561d1612737a4905a18dbdd4";
    sha256 = "0a3j9s5hljz1065gqrj304p6xj6grklrl8k7jblawx8c34kibd6y";
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
    rev = "c3571f76664e1813f90d97b8c194a1e0149e895e";
    sha256 = "0plld74wincyy3c5gdfqh78pzrqibxh6r839dm0c717fajr9imwb";
  };
in
{ imports = [ (toString hcloud-modules) ]; }
```

# options

These options are available by the module

<div class="alert alert-error">Links will be fixed soon</div>
