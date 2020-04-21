---
title: Getting Started
order: 1
draft: false
info: A quick introduction on how to start.
letter: g
preview: true
---

Let's have a quick overview on how you would use Terranix.

If you search for working examples have a look at the
[ examples folder at github ](https://github.com/mrVanDalo/terranix/tree/master/examples).


## How to Setup

A convenient way is to create a shell.nix
which holds you terranix and terraform setup.

```nix
{ pkgs ? import <nixpkgs> { } }:
let

  hcloud_api_token = "`${pkgs.pass}/bin/pass development/hetzner.com/api-token`";

  terranix = pkgs.callPackage (pkgs.fetchgit {
    url = "https://github.com/mrVanDalo/terranix.git";
    rev = "2.2.3";
    sha256 = "0r7n0c1m81rz22x2bc3kkw63xs3cf8jbfpr73vplnc3yyngkrjxp";
  }) { };

  terraform = pkgs.writers.writeBashBin "terraform" ''
    export TF_VAR_hcloud_api_token=${hcloud_api_token}
    ${pkgs.terraform_0_11}/bin/terraform "$@"
  '';

in pkgs.mkShell {
  buildInputs = [ terranix terraform ];
}
```

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

Next lets generate a json file in terraform json format
and run terraform apply
to let terraform do it's magic.

```shell
terranix > config.tf.json && terraform init && terraform apply
```

### Destroy a Server

cleaning everything up is the job of terraform, just run : 

```shell
terraform destroy
```

# Links

* [ shell.nix template to start ](https://github.com/mrVanDalo/nix-shell-mix/blob/master/terraform/shell.nix)
* [ terraform provider documentation ](https://www.terraform.io/docs/providers/index.html)
* [ nixpkgs function documentation ]( https://storage.googleapis.com/files.tazj.in/nixdoc/manual.html#sec-functions-library )
