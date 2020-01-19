---
title: Getting Started
date: 2020-11-11
draft: false
info: A quick introduction on how to start.
---

# Getting Started

Let's have a quick overview on how you would use Terranix.

If you search for working examples have a look at the
[ examples folder at github ](https://github.com/mrVanDalo/terranix/tree/master/examples).


## How to Install

You can install terranix via an [overlay](https://nixos.wiki/wiki/Overlays) like this:

```nix
terranix = callPackage (super.fetchgit {
    url = "https://github.com/mrVanDalo/terranix.git";
    rev = "6097722f3a94972a92d810f3a707351cd425a4be";
    sha256 = "1d8w82mvgflmscvq133pz9ynr79cgd5qjggng85byk8axj6fg6jw";
  }) { };
```

## How to use


### config.nix 

create a `config.nix` for example

```nix
{ ... }:
{
  resource.hcloud_server.nginx = {
    name = "terranix.nginx";
    image  = "debian-10";
    server_type = "cx11";
    backups = false;
  };
  resource.hcloud_server.test = {
    name = "terranix.test";
    image  = "debian-9";
    server_type = "cx11";
    backups = true;
  };
}
```

### Create a Server

To apply your configuration, we will generate a json file in terraform json format.
After that we run terraform to apply theses changes.

```shell
terranix > config.tf.json && terraform init && terraform apply
```

### Destroy a Server

```shell
terraform destroy
```

# Links

* [ shell.nix template to start ](https://github.com/mrVanDalo/nix-shell-mix/blob/master/terraform/shell.nix)
* [ terraform provider documentation ](https://www.terraform.io/docs/providers/index.html)
* [ nixpkgs function documentation ]( https://storage.googleapis.com/files.tazj.in/nixdoc/manual.html#sec-functions-library )
