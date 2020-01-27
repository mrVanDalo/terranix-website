---
title: terranix.org
---

A NixOS way to create [terraform json](https://www.terraform.io/docs/configuration/syntax-json.html) files.

## Features

* Using terranix is very similar to use terraform, you can use the
  [terraform reference material](https://www.terraform.io/docs/providers/index.html) without much hassle.
* The full power of the nix language (map, filter, reduce, ... )
* The full power of the module system of NixOS
* The full power of all the tooling in `pkgs` of NixOS (fetchgit,fetchurl,writers, ...)
* Documentation generation out of a `config.nix` as json or man page.

# What is Terraform?

Terraform a tool to interact with APIs via declarative files.
Instead of write imperative scripts, you
define the setup you like to have and terraform will make it happen.

## What is config.tf.json?

`config.tf.json` or `config.tf` is the file that contains the
setup you which to realize behind one or multiple APIs.
The majority of your work will be to create theses files.

## What are Providers?

Providers are the *thing* that enables terraform to talk to APIs.
There is a huge list of providers available on 
[the Terraform website](https://www.terraform.io/docs/providers/index.html).

## What is Terraform State?

Terraform is not capable of seeing the state behind APIs,
because APIs never share all information.
This is why terraform creates a state file
on every run to provide information for the next runs.

It is not always clear what ends up in this state file,
and is the reason why secrets have to be handled with care!


# What is terranix?

terranix is a tool that enables you to render the `config.tf.json` file.
It uses the NixOS module system and gives you tools like `terranix-doc-man` and `terranix-doc-json`
to generate documentation of terranix modules.

## News

<div class="media"> <div class="media-left"> <a href="https://github.com/mrVanDalo/terranix/releases/tag/2.2.0"> <div class="avatarholder">r</div> </a> </div> <div class="media-body"> <div class="media-heading"><a href="https://github.com/mrVanDalo/terranix/releases/tag/2.2.0"> version 2.2.0 released </a> </div> <div class="media-content"> I'm happy to announce that version 2.2.0 is ready. You are now able to render documentation similar to NixOS. </div> </div> </div>

# Documentation

$partial("templates/post-list.html")$
