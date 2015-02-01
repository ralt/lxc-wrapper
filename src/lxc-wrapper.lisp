(in-package #:lxc-wrapper)

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
  (destructuring-bind (&key base template)
      (car args)
    (if base
	(create-clone base name)
	(create-base name template))))

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

(defcommand package (name &rest args)
  "Packages an LXC"
  (let* ((cli-name (adapt-arg name))
	 (archive (concatenate 'string cli-name *lxc-package-extension*)))
    (when (not (equal args '(nil)))
      (destructuring-bind (&key archive-path)
	  (car args)
	(setf archive archive-path)))
    (format t "Packaging ~A...~%" cli-name)
    (run
      "tar"
      "-C" (merge-pathnames cli-name *lxc-default-folder*)
      "-czf" archive
      ".")
    (format t "Created ~A~%" archive)))

(defun adapt-arg (name)
  "Adapts an argument to string"
  (when (symbolp name)
    (return-from adapt-arg (string-downcase (symbol-name name))))
  (when (stringp name)
    (return-from adapt-arg (string-downcase name))))
