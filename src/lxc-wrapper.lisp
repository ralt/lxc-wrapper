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
  "create NAME
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
		--lxc-interfaces-file=etc/network/interfaces"
  (destructuring-bind (&key base template)
      args
    (if base
	(create-clone base name)
	(create-base name template))))

(defcommand start (name args)
  "start NAME
	Starts the container named NAME"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (format t "Starting ~A..." cli-name)
    (run
      "lxc-start"
      "--name" cli-name)
    (format t " done.~%")))

(defcommand stop (name args)
  "stop NAME
	Stops the container named NAME"
  (declare (ignore args))
  (let ((cli-name (adapt-arg name)))
    (format t "Stopping ~A..." cli-name)
    (run
      "lxc-stop"
      "--name" cli-name)
    (format t " done.~%")))

(defcommand ls (name args)
  "ls
	Lists the containers"
  (declare (ignore name))
  (declare (ignore args))
  (run
   "lxc-ls"
   "--fancy"))

(defcommand destroy (name args)
  "destroy NAME
	Destroys the container named NAME

	Overridable variables and default values (must be BEFORE the command):
		--lxc-folder=~/lxc/
		--lxc-host-extension=.lxc
		--hosts-file=/etc/hosts"
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
  "package NAME
	Packages the container named NAME

	Options (must be BEFORE the command):
		--archive-path=PATH
			the path of the archive

	Overridable variables and default values (must be BEFORE the command):
		--lxc-package-extension=.tar.gz
		--lxc-default-folder=/var/lib/lxc/"
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
  "deploy --archive ARCHIVE NAME
	Deploys the ARCHIVE archive in a container named NAME

	Overridable variables and default values (must be BEFORE the command):
		--lxc-default-folder=/var/lib/lxc/
		--lxc-config=config
		--hosts-file=/etc/hosts"
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

(defcommand autostart (name args)
  "autostart NAME
	Toggles the autostart status of the container named NAME

	Overridable variables and default values (must be BEFORE the command):
		--lxc-default-folder=/var/lib/lxc/
		--lxc-config=config"
  (declare (ignore args))
  (let* ((cli-name (adapt-arg name))
	 (lxc-path (merge-pathnames *lxc-config*
				    (merge-pathnames
				     (concatenate 'string cli-name "/")
				     *lxc-default-folder*)))
	 (config-content (alexandria:read-file-into-string
			  lxc-path)))
    (if (lxc-config-has-autostart config-content)
	(toggle-autostart-value lxc-path config-content)
	(add-autostart-line lxc-path))))

(defun adapt-arg (name)
  "Adapts an argument to string"
  (when (symbolp name)
    (return-from adapt-arg (string-downcase (symbol-name name))))
  (when (stringp name)
    (return-from adapt-arg (string-downcase name))))
