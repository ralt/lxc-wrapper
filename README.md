# lxc-wrapper

[![Build Status](https://travis-ci.org/Ralt/lxc-wrapper.svg?branch=master)](https://travis-ci.org/Ralt/lxc-wrapper)

An opinionated LXC wrapper.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [LX-What?](#lx-what)
- [Motivation](#motivation)
- [What it does](#what-it-does)
- [Example session](#example-session)
- [Why not docker?!](#why-not-docker)
- [Installation](#installation)
- [CLI Usage](#cli-usage)
- [Requirements](#requirements)
- [Limitations](#limitations)
- [Development](#development)
- [API](#api)
  - [Functions](#functions)
    - [`create`](#create)
    - [`destroy`](#destroy)
    - [`start`](#start)
    - [`stop`](#stop)
    - [`ls`](#ls)
    - [`package`](#package)
    - [`deploy`](#deploy)
  - [Variables](#variables)
    - [`*lxc-default-folder*`](#lxc-default-folder)
    - [`*lxc-rootfs*`](#lxc-rootfs)
    - [`*lxc-folder`*`](#lxc-folder)
    - [`*lxc-host-extension*`](#lxc-host-extension)
    - [`*lxc-gateway*`](#lxc-gateway)
    - [`*default-dns-nameserver*`](#default-dns-nameserver)
    - [`*hosts-file*`](#hosts-file)
    - [`*lxc-network*`](#lxc-network)
    - [`*ip-regex*`](#ip-regex)
    - [`*lxc-interfaces-file*`](#lxc-interfaces-file)
    - [`*default-shell*`](#default-shell)
    - [`*lxc-package-extension*`](#lxc-package-extension)
    - [`*lxc-config*`](#lxc-config)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## LX-What?

LXC, aka Linux Containers. In case you don't know what they are, it's
basically virtual machines without emulating the hardware. In
technical details, it's chroot on steroids, because the filesystem,
processes and networking are separated from the host. More information
[here][0].

This technology is getting a lot of traction since ~2 years, because
it allows people to create isolated environments very quickly, very
cheaply. Fedora 21, for example, will have each application run in a
different container. Deploying containers on AWS lets you build
multi-tier architectures for cheap, etc etc. There are many
applications.

I personally use them as "VMs" for my projects (each project gets a
VM). And lxc-wrapper is there to help me with that. I think this is a
common usage though, so I thought it was worth sharing.

## Motivation

I use LXC in a very opinionated way, and has some manual maintenance to
do every time I do something with LXCs. So I created this tool to
automate what I do with them.

## What it does

- I always forget the `--name` option. lxc-wrapper assumes that the
  argument is the name of the container.
- Writing `--fancy` when I want an ls should be the default. No
  argument needed.
- When creating a container, it assigns a static IP to it, adds an
  entry to the hosts file so that the container is reachable, and adds
  a symbolic link to the rootfs in a defined folder.
- Destroying a container cleans up behind itself.

## Example session

Starting a new project "bar" based on the "foo" stack.

    $ sudo lxc-wrapper --base foo create bar
    Created container bar as copy of foo
    $ sudo lxc-wrapper ls
    NAME      STATE    IPV4      IPV6  GROUPS  AUTOSTART
    ----------------------------------------------------
    bar       STOPPED  -         -     -       NO
    foo       STOPPED  -         -     -       NO
    $ ls ~/lxc/
    foo bar
    $ ls ~/lxc/bar
    bin  boot  dev  etc  home  lib  lib64  media  mnt  opt  proc  root  run  sbin  srv  sys  tmp  usr  var
    $ sudo lxc-wrapper start bar
    $ sudo lxc-wrapper ls
    NAME      STATE    IPV4      IPV6  GROUPS  AUTOSTART
    ----------------------------------------------------
    bar       STARTED  10.0.3.4  -     -       NO
    foo       STOPPED  -         -     -       NO
    $ ssh ubuntu@bar.lxc

When done with the project:

    $ sudo lxc-wrapper stop bar
    $ sudo lxc-wrapper destroy bar
    $ sudo lxc-wrapper ls
    NAME      STATE    IPV4      IPV6  GROUPS  AUTOSTART
    ----------------------------------------------------
    foo       STOPPED  -         -     -       NO

## Why not docker?!

Good question.

Several reasons:

- Docker is based on an overlayfs system all the time. My usage is
  simply having long-term projects, so docker's containers don't make
  sense.
- Docker's networking doesn't allow me to assign a static IP to a
  container. It makes it inconvenient, especially for long-term
  containers, to connect to them via ssh.
- Docker's containers' filesystems are hidden when they're not
  started. I don't want to start a container just to get some random
  file in it.
- Docker CLI are not a very beautiful API. lxc-wrapper is supposed to
  be **simple** to use.

## Installation

You can:

- Download the sources and run `make && make install`. Only `sbcl` is
  needed.
- Download and install the [rpm
  file](https://github.com/Ralt/lxc-wrapper/releases/download/1.0.3/lxc-wrapper-1.0.3-1.x86_64.rpm)
  ([pgp signature](https://github.com/Ralt/lxc-wrapper/releases/download/1.0.3/lxc-wrapper_1.0.3-rpm.sig))
  if you're on Fedora/CentOS/Red Hat (x86~64~ only)
- Download and install the [deb
  file](https://github.com/Ralt/lxc-wrapper/releases/download/1.0.3/lxc-wrapper_1.0.3_amd64.deb)
  ([pgp signature](https://github.com/Ralt/lxc-wrapper/releases/download/1.0.3/lxc-wrapper_1.0.3-deb.sig))
  if you're on Ubuntu/Debian (amd64 only)
- Download and install the
  [PKGBUILD](https://aur.archlinux.org/packages/lxc-wrapper/) if
  you're on ArchLinux

## CLI Usage

```
$ lxc-wrapper help
Usage: lxc-wrapper [OPTIONS] [COMMAND]
Wrapper around lxc for an opinionated workflow.

Commands:

	help
		Shows this help

	create NAME
		Creates a container named NAME

		Options (must be BEFORE the command):
			--base=BASE
				Clones the BASE container
			--template=TEMPLATE
				Uses the TEMPLATE lxc template

		Overridable variables and default values (must be BEFORE the command):
			--lxc-default-folder=/var/lib/lxc/
			--lxc-rootfs=rootfs/
			--lxc-folder=~/lxc/
			--lxc-host-extension=.lxc
			--default-dns-nameserver=8.8.8.8
			--hosts-file=/etc/hosts
			--lxc-interfaces-file=etc/network/interfaces

	start NAME
		Starts the container named NAME

	stop NAME
		Stops the container named NAME

	ls
		Lists the containers

	destroy NAME
		Destroys the container named NAME

		Overridable variables and default values (must be BEFORE the command):
			--lxc-folder=~/lxc/
			--lxc-host-extension=.lxc
			--hosts-file=/etc/hosts

	package NAME
		Packages the container named NAME

		Options (must be BEFORE the command):
			--archive-path=PATH
				the path of the archive

		Overridable variables and default values (must be BEFORE the command):
			--lxc-package-extension=.tar.gz
			--lxc-default-folder=/var/lib/lxc/

	deploy --archive ARCHIVE NAME
		Deploys the ARCHIVE archive in a container named NAME

		Overridable variables and default values (must be BEFORE the command):
			--lxc-default-folder=/var/lib/lxc/
			--lxc-config=config
			--hosts-file=/etc/hosts

	autostart NAME
		Toggles the autostart status of the container named NAME

		Overridable variables and default values (must be BEFORE the command):
			--lxc-default-folder=/var/lib/lxc/
			--lxc-config=config

	Overridable variables and default values for all commands (must be BEFORE the c
ommand):
		--default-shell=/bin/bash

```

## Requirements

Linux only.

If you just want to use the distributed package, that's all you need.

If you want to compile yourself, you need:

- sbcl
- lxc

And run:

```
$ git clone https://github.com/Ralt/lxc-wrapper
$ cd lxc-wrapper
$ make
$ make install
```

Eventually using `sudo` for the `make install`.

Tested on SBCL only. There is a requirement on `sb-posix` to get the
version number.

The swank server or the CLI utility needs to be ran as root. (Ideally
with sudo, so that `~` matches your user folder)

## Limitations

Known limitations:

- Only /24 subnetworks supported. Which means you can only make 254
  containers **with lxc-wrapper** on one host.
- Autostart management not supported yet.

## Development

You need:

- SBCL
- QuickLisp

To create a CLI utility, you need:

- buildapp

The Makefile supports the following tasks:

- `all`: builds the `./dist/usr/bin/lxc-wrapper` binary, along with
  downloading dependencies in a local quicklisp environment
- `clean`: deletes the dependencies and the binary
- `install`: copies the `./dist/usr/binlxc-wrapper` binary to `DESTDIR`
  which is `/usr/bin` by default
- `test`: runs tests; requires a functional lisp environment

## API

### Functions

#### `create`

```lisp
(defcommand create (name args)
  "Creates an LXC"
  (destructuring-bind (&key base template)
      args
```

Creates an LXC.

If a base LXC is provided, then it makes a clone of it.

If a template is provided, then it creates a new LXC based on this
template.

The opinionated part of lxc-wrapper comes here. For every new LXC:

- It gives it a static IP
- It adds the static IP to the host's /etc/hosts
- It makes a symlink to the rootfs

#### `destroy`

```lisp
(defcommand destroy (name args)
  "Destroys an LXC and its leftovers"
  (declare (ignore args))
```

Destroys an LXC.

The opinionated part of lxc-wrapper comes here too. When an LXC is
destroyed:

- It destroys the entry in the host's /etc/hosts
- It deletes the symlink to the rootfs

#### `start`

```lisp
(defcommand start (name args)
  "Starts an LXC"
  (declare (ignore args))
```

Starts an LXC. The argument can be a string or a symbol.

#### `stop`

```lisp
(defcommand stop (name args)
  "Stops an LXC"
  (declare (ignore args))
```

Stops an LXC. The argument can be a string or a symbol.

#### `ls`

```lisp
(defcommand ls (name args)
  "Lists all the LXC"
  (declare (ignore args))
```

Returns the fancy output of the list of LXCs.

#### `package`

```lisp
(defcommand package (name args)
  "Packages an LXC"
```

Packages an LXC into an shareable archive file.

#### `deploy`

```lisp
(defcommand deploy (name args)
  "Deploys an archive created by lxc-wrapper"
  (destructuring-bind (&key archive)
    args
```

Deploys an archive created by `lxc-wrapper package`.

#### `autostart`

```lisp
(defcommand autostart (name args)
  "Toggles the autostart setting of a container"
```

Toggles the autostart setting of a container.

### Variables

Variables are used throughout the code to be able to customize them
through dynamic scoping.

#### `*lxc-default-folder*`

Used by: `create`

Default value: `/var/lib/lxc/`

The folder where LXC stores its containers.

#### `*lxc-rootfs*`

Used by: `create`

Default value: `rootfs`

The folder where the filesystem of the container lives.

#### `*lxc-folder`*`

Used by: `create`, `destroy`

Default value: `~/lxc`

The folder where symbolic links to the containers' filesystems are made.

#### `*lxc-host-extension*`

Used by: `create`, `destroy`

Default value: `.lxc`

The TLD of the container hostname.

#### `*lxc-gateway*`

Used by: `create`

Default value: `10.0.3.1`

The gateway that the container uses.

#### `*default-dns-nameserver*`

Used by: `create`

Default value: `8.8.8.8`

The DNS nameserver that the container uses.

#### `*hosts-file*`

Used by: `create`, `destroy`

Default value: `/etc/hosts`

The host's hosts file.

#### `*lxc-network*`

Used by: `create`, `destroy`

Default value: `'(10 0 3 0)`

The network of the container. Only /24 supported.

#### `*ip-regex*`

Used by: `create`

Default value: `^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)`

The regex used to find IPs in the hosts file.

#### `*lxc-interfaces-file*`

Used by: `create`

Default value: `etc/network/interfaces`

The file where interfaces are written in the container.

#### `*default-shell*`

Used by: `create`, `destroy`, `start`, `stop`, `ls`

Default value: `/bin/bash`

The shell used by the commands.

#### `*lxc-package-extension*`

Used by: `package`

Default value: `.tar.gz`

The extension to give to archives created by `package`.

#### `*lxc-config*`

Used by: `deploy`

Default value: `#p"config"`

The name of the configuration file of the containers.

## License

MIT License.


  [0]: http://en.wikipedia.org/wiki/LXC
