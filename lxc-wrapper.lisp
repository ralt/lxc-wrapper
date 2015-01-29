(in-package #:lxc-wrapper)

(defvar *default-shell* #p"/bin/bash")

(defmacro run (&body command)
  "Runs a command. To avoid having an awkward API
 (i.e. passing a list), defining this as a macro."
  `(external-program:run
     (car (list ,@command))
     (cdr (list ,@command))
     :output *standard-output*
     ;; see man environ
     :environment (list (cons "SHELL" *default-shell*))))

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

(defun destroy (name)
  "Destroys an LXC and its leftovers"
  (let ((cli-name (adapt-arg name)))
    (run
      "lxc-destroy"
      "--name" cli-name)
    (remove-lxc-leftovers cli-name)))

(defun adapt-arg (name)
  "Adapts an argument to string"
  (when (symbolp name)
    (return-from adapt-arg (string-downcase (symbol-name name))))
  (when (stringp name)
    (return-from adapt-arg (string-downcase name))))
