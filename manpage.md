% LXC-WRAPPER(1) lxc-wrapper man page
% Florian Margaine <florian@margaine.com>
% February 2, 2015

# NAME

lxc-wrapper - Wrapper around lxc for an opinionated workflow.

# SYNOPSIS

lxc-wrapper [OPTIONS] [COMMAND]

# DESCRIPTION

lxc-wrapper lets you manage your containers with a simple API, in an opinionated workflow.
Notably, it automatically takes care of networking.

	help
		Shows the help

	version
		Shows the version of lxc-wrapper

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

	Overridable variables and default values for all commands (must be BEFORE the command):
		--default-shell=/bin/bash

# BUGS

If you find any bug, please send your reports to lxc-wrapper@googlegroups.com
