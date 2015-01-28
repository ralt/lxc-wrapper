;;;; lxc-wrapper.lisp

(in-package #:lxc-wrapper)

;;; "lxc-wrapper" goes here. Hacks and glory await!

(defmacro run (&body command)
  "Runs a command using sudo. LXC requires sudo.
To avoid having an awkward API (i.e. passing a list),
defining this as a macro."
  (let ((stream (gensym)))
    `(with-output-to-string (,stream)
       (external-program:run
	 "sudo"
	 (list ,@command)
	 :output ,stream
	 :environment '(("$PATH" . "/usr/bin")))
       ,stream)))

(defun init-lxc (name)
  "Initializes the LXC after creating it. It means:
- Giving it a static IP
- Adding the static IP to the host's /etc/hosts
- Making a symlink to the rootfs somewhere")

(defun create (name &key base template)
  "Creates an LXC"
  (if base
      (create-clone base name)
      (create-base name template)))

(defun create-clone (base name)
  "Creates a clone of another LXC"
  (let ((cli-base (adapt-arg base))
	(cli-name (adapt-arg name)))
    (run
      "lxc-clone"
      "--orig" cli-base
      "--new" cli-name)
    (init-lxc cli-name)))

(defun create-base (name template)
  "Creates an LXC from no base"
  (let ((cli-name (adapt-arg name))
	(cli-template (adapt-arg template)))
    (run
      "lxc-create"
      "--name" cli-name
      "-t" cli-template)
    (init-lxc cli-name)))

(defun start (name)
  "Starts an LXC"
  (let ((cli-name (adapt-arg name)))
    (run
      "lxc-start"
      "--name" cli-name)))

(defun stop (name)
  "Stops an LXC"
  (let ((cli-name (adapt-arg name)))
    (run
      "lxc-stop"
      "--name" cli-name)))

(defun ls ()
  "Lists all the LXC"
  (run
   "lxc-ls"
   "--fancy"))

(defun adapt-arg (name)
  "Adapts an argument to string"
  (when (symbolp name)
    (string-downcase (symbol-name name)))
  (when (stringp name)
    (string-downcase name)))
