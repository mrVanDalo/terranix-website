---
title: Differences between terranix and HCL
order: 2
draft: false
info: Syntax differences between terranix and HCL
letter: t
---

## declarations

In **HCL** you would do something like this:

```hcl
resource "aws_instance" "web" {
  ami           = "${data.aws_ami.ubuntu.id}"
  instance_type = "t2.micro"
  tags = {
    Name = "HelloWorld"
  }
}
```

Which is the equivalent for the following in **terranix**:

```nix
resource."aws_instance"."web" = {
  ami = "\${data.aws_ami.ubuntu.id}";
  instance_type = "t2.micro";
  tags = {
    Name = "HelloWorld";
  };
}
```

## references

In **HCL** you can only reference variables outputs. 
But in terranix, because it is nix, you can basically reference everything.

For example have a resource and want to reuse the parameters:
```nix
resoure.hcloud_server.myserver = {
  name = "node1";
  image = "debian-9";
  server_type = "cx11";
};
```

You can reference parameters the terraform way:

```nix
resoure.hcloud_server.myotherserver = {
  name = "node2";
  image = "\${ hcloud_server.myserver.image }";
  server_type = "\${ hcloud_server.myserver.server_type }";
};
```

Or the terranix way:

```nix
resoure.hcloud_server.myotherotherserver = {
  name = "node3";
  image = config.resource.hcloud_server.myserver.image;
  server_type = config.resource.hcloud_server.myserver.server_type;
};
```

Or the terranix pro way:

```nix
resoure.hcloud_server.myotherotherotherserver = {
  name = "node4";
  inherit (config.resource.hlcoud_server) image server_type;
};
```

The difference is that terranix and terraform references are evaluated differently.
terranix references are evaluated when generating the json file, and terraform references are calculated
during the process.

