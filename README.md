# lxc-wrapper

[![Build Status](https://travis-ci.org/Ralt/lxc-wrapper.svg?branch=master)](https://travis-ci.org/Ralt/lxc-wrapper)

An opinionated LXC wrapper.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

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
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Motivation

I use LXC in a very opinionated way, and has some manual maintenance to
do every time I do something with LXCs. So I created this tool to
automate what I do with them.

## What it does

-   I always forget the `--name` option. lxc-wrapper assumes that the
    argument is the name of the container.
-   Writing `--fancy` when I want an ls should be the default. No
    argument needed.
-   When creating a container, it assigns a static IP to it, adds an
    entry to the hosts file so that the container is reachable, and adds
    a symbolic link to the rootfs in a defined folder.
-   Destroying a container cleans up behind itself.

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

-   Docker is based on an overlayfs system all the time. My usage is
    simply having long-term projects, so docker's containers don't make
    sense.
-   Docker's networking doesn't allow me to assign a static IP to a
    container. It makes it inconvenient, especially for long-term
    containers, to connect to them via ssh.
-   Docker's containers' filesystems are hidden when they're not
    started. I don't want to start a container just to get some random
    file in it.
-   Docker CLI are not a very beautiful API. lxc-wrapper is supposed to
    be **simple** to use.

## Installation

You can:

-   Download the sources and run `make && make install`. Only `sbcl` is
    needed.
-   Download and install the [rpm
    file](https://github.com/Ralt/lxc-wrapper/releases/download/1.0.1/lxc-wrapper-1.0.1-1.x86_64.rpm)
    if you're on Fedora/CentOS/Red Hat (x86~64~ only)
-   Download and install the [deb
    file](https://github.com/Ralt/lxc-wrapper/releases/download/1.0.1/lxc-wrapper_1.0.1_amd64.deb)
    if you're on Ubuntu/Debian (amd64 only)
-   Download and install the
    [PKGBUILD](https://aur.archlinux.org/packages/lxc-wrapper/) if
    you're on ArchLinux

## CLI Usage

```
$ lxc-wrapper # or lxc-wrapper help
Usage: lxc-wrapper [OPTIONS] [COMMAND]
Wrapper around lxc for an opinionated workflow.

Commands:

 create NAME
  creates a container named NAME

  Options (must be BEFORE the command):
   --base=BASE
    clone BASE
   --template=TEMPLATE
    use the TEMPLATE lxc template
   --lxc-default-folder, --lxc-rootfs, --lxc-folder, --lxc-extension, --lxc-gateway, --default-dns-nameserver, --hosts-file, --lxc-interfaces-file

 start NAME
  starts the container named NAME

 stop NAME
  stops the container named NAME

 ls
  lists the containers

 destroy NAME
  destroys the container named NAME

  Options (must be BEFORE the command):
   --lxc-folder, --lxc-host-extension, --hosts-file

 Options for all commands (must be BEFORE the command):
  --default-shell
```

## Requirements

Linux only.

Tested on SBCL only, but nothing specific is used. Should work on other
platforms.

The swank server or the CLI utility needs to be ran as root. (Ideally
with sudo, so that `~` matches your user folder)

## Limitations

Known limitations:

-   Only /24 subnetworks supported. Which means you can only make 254
    containers **with lxc-wrapper** on one host.
-   Autostart management not supported yet.

## Development

You need:

-   SBCL
-   QuickLisp

To create a CLI utility, you need:

-   buildapp

The Makefile supports the following tasks:

-   `all`: builds the `./dist/usr/bin/lxc-wrapper` binary, along with
    downloading dependencies in a local quicklisp environment
-   `clean`: deletes the dependencies and the binary
-   `install`: copies the `./dist/usr/binlxc-wrapper` binary to `DESTDIR`
    which is `/usr/bin` by default
-   `test`: runs tests; requires a functional lisp environment

## API

### Functions

#### `create`

```lisp
(defcommand create (name args)
  "Creates an LXC"
```

Creates an LXC.

If a base LXC is provided, then it makes a clone of it.

If a template is provided, then it creates a new LXC based on this
template.

The opinionated part of lxc-wrapper comes here. For every new LXC:

-   It gives it a static IP
-   It adds the static IP to the host's /etc/hosts
-   It makes a symlink to the rootfs

#### `destroy`

```lisp
(defcommand destroy (name)
  "Destroys an LXC and its leftovers"
```

Destroys an LXC.

The opinionated part of lxc-wrapper comes here too. When an LXC is
destroyed:

-   It destroys the entry in the host's /etc/hosts
-   It deletes the symlink to the rootfs

#### `start`

```lisp
(defcommand start (name)
  "Starts an LXC"
```

Starts an LXC. The argument can be a string or a symbol.

#### `stop`

```lisp
(defcommand stop (name)
  "Stops an LXC"
```

Stops an LXC. The argument can be a string or a symbol.

#### `ls`

```lisp
(defcommand ls ()
  "Lists all the LXC"
```

Returns the fancy output of the list of LXCs.

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

## License

MIT License.
