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

(defcommand create (name args)
  "Creates an LXC"
  (if (getf args :base)
      (create-clone (getf args :base) name)
      (create-base name (getf args :template))))

(defun create-clone (base name)
  "Creates a clone of another LXC"
  (let ((cli-base (adapt-arg base))
	(cli-name (adapt-arg name)))
    (run
      "lxc-clone"
      "--orig" cli-base
      "--new" cli-name)
    (init-lxc cli-name *hosts-file*)))

(defun create-base (name template)
  "Creates an LXC from no base"
  (let ((cli-name (adapt-arg name))
	(cli-template (adapt-arg template)))
    (run
      "lxc-create"
      "--name" cli-name
      "-t" cli-template)
    (init-lxc cli-name *hosts-file*)))

(defcommand start (name &rest args)
  "Starts an LXC"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (run
      "lxc-start"
      "--name" cli-name)))

(defcommand stop (name &rest args)
  "Stops an LXC"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (run
      "lxc-stop"
      "--name" cli-name)))

(defcommand ls (&rest args)
  "Lists all the LXC"
  (declare (ignore args))
  (run
   "lxc-ls"
   "--fancy"))

(defcommand destroy (name &rest args)
  "Destroys an LXC and its leftovers"
  (declare (ignore args))
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
