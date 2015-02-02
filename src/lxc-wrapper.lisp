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
      args
    (if base
	(create-clone base name)
	(create-base name template))))

(defun create-clone (base name)
  "Creates a clone of another LXC"
  (let ((cli-base (adapt-arg base))
	(cli-name (adapt-arg name)))
    (format t "Cloning ~A..." cli-base)
    (run
      "lxc-clone"
      "--orig" cli-base
      "--new" cli-name)
    (format t " done.~%")
    (init-lxc cli-name *hosts-file*)))

(defun create-base (name template)
  "Creates an LXC from no base"
  (let ((cli-name (adapt-arg name))
	(cli-template (adapt-arg template)))
    (format t "Creating ~A..." cli-name)
    (run
      "lxc-create"
      "--name" cli-name
      "-t" cli-template)
    (format t " done.~%")
    (init-lxc cli-name *hosts-file*)))

(defcommand start (name args)
  "Starts an LXC"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (format t "Starting ~A..." cli-name)
    (run
      "lxc-start"
      "--name" cli-name)
    (format t " done.~%")))

(defcommand stop (name args)
  "Stops an LXC"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (format t "Stopping ~A..." cli-name)
    (run
      "lxc-stop"
      "--name" cli-name)
    (format t " done.~%")))

(defcommand ls (name args)
  "Lists all the LXC"
  (declare (ignore args))
  (declare (ignore args))
  (run
   "lxc-ls"
   "--fancy"))

(defcommand destroy (name args)
  "Destroys an LXC and its leftovers"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (format t "Destroying ~A..." cli-name)
    (run
      "lxc-destroy"
      "--name" cli-name)
    (format t " done.~%")
    (format t "Removing leftovers...")
    (remove-lxc-leftovers cli-name)
    (format t " done.~%")))

(defcommand package (name args)
  "Packages an LXC"
  (let* ((cli-name (adapt-arg name))
	 (archive (concatenate 'string cli-name *lxc-package-extension*)))
    (when args
      (destructuring-bind (&key archive-path)
	  args
	(setf archive archive-path)))
    (format t "Packaging ~A...~%" cli-name)
    (run
      "tar"
      "-C" (merge-pathnames cli-name *lxc-default-folder*)
      "-czf" archive
      ".")
    (format t "Created ~A~%" archive)))

(defcommand deploy (name args)
  "Deploys an archive created by lxc-wrapper"
  (destructuring-bind (&key archive)
      args
    (let* ((cli-name (adapt-arg name))
	   (lxc-path (merge-pathnames (concatenate 'string cli-name "/")
				      *lxc-default-folder*)))
      (run
	"mkdir" "-p" lxc-path)
      (format t "Deploying ~A..." cli-name)
      (run
	"tar"
	"xf" archive
	"-C" lxc-path)
      (fix-lxc-config cli-name lxc-path *lxc-config*)
      (format t " done.~%")
      (init-lxc cli-name *hosts-file*))))

(defun adapt-arg (name)
  "Adapts an argument to string"
  (when (symbolp name)
    (return-from adapt-arg (string-downcase (symbol-name name))))
  (when (stringp name)
    (return-from adapt-arg (string-downcase name))))
