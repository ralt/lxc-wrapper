# lxc-wrapper

My personal LXC wrapper.

## Motivation

I use LXC in a very opinionated way, and has some manual maintenance
to do every time I do something with LXCs. So I created this tool to
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

## Requirements

Tested on SBCL only, but nothing specific is used. Should work on
other platforms.

The swank server needs to be ran as root. (Ideally with sudo, so that
`~` matches your user folder)

## API

### Functions

#### `create`

```lisp
(defun create (name &key base template)
  "Creates an LXC"
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
(defun destroy (name)
  "Destroys an LXC and its leftovers"
```

Destroys an LXC.

The opinionated part of lxc-wrapper comes here too. When an LXC is
destroyed:

- It destroys the entry in the host's /etc/hosts
- It deletes the symlink to the rootfs

#### `start`

```lisp
(defun start (name)
  "Starts an LXC"
```

Starts an LXC. The argument can be a string or a symbol.

#### `stop`

```lisp
(defun stop (name)
  "Stops an LXC"
```

Stops an LXC. The argument can be a string or a symbol.

#### `ls`

```lisp
(defun ls ()
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

#### `*lxc-folder*`

Used by: `create`, `destroy`

Default value: `~/lxc`

The folder where symbolic links to the containers' filesystems are
made.

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
