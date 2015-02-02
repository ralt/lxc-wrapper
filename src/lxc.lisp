(in-package #:lxc-wrapper)

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

(defun init-lxc (name file)
  "Initializes the LXC after creating it. It means:
- Giving it a static IP
- Adding the static IP to the host's /etc/hosts
- Making a symlink to the rootfs somewhere
- Making the container directory readable by all"
  (format t "Initializing ~A..." name)
  (let ((ip (next-ip file))
	(lxc-path (merge-pathnames (concatenate 'string name "/")
				   *lxc-default-folder*)))
    (assign-static-ip name ip *lxc-gateway* *default-dns-nameserver*)
    (add-ip *hosts-file* ip (concatenate 'string name *lxc-host-extension*))
    (make-lxc-symlink (merge-pathnames *lxc-rootfs* lxc-path)
		      (merge-pathnames name *lxc-folder*))
    (fix-permissions lxc-path))
  (format t " done.~%"))

(defun fix-permissions (path)
  "Makes the folder readable by all"
  (run "chmod" "o+x" path))

(defun remove-lxc-leftovers (name)
  "Removes the leftovers such as:
- The IP in /etc/hosts
- The symbolic link to the now-missing rootfs"
  (remove-ip *hosts-file* name)
  (lxc-delete-file (merge-pathnames name *lxc-folder*)))

(defun make-lxc-symlink (base end)
  "Makes a symlink from end to base"
  (run
    "ln"
    "-s" base end))

(defun lxc-delete-file (file)
  "Deletes a file if it exists"
  (when (probe-file file)
    (delete-file file)))

(defun fix-lxc-config (name lxc-path config)
  "Fixes the config of a newly deployed container"
  (let* ((config-path (merge-pathnames config lxc-path))
	 (config-string (alexandria:read-file-into-string config-path))
	 (base-name (get-base-lxc-name config-string)))
    (alexandria:write-string-into-file
     (cl-ppcre:regex-replace-all base-name
				 config-string
				 name)
     config-path
     :if-exists :overwrite)))

(defun get-base-lxc-name (config)
  "Gets the name of the base lxc"
  (multiple-value-bind (match name)
      (cl-ppcre:scan-to-strings "\\n\\s*lxc\\.utsname\\s*=\\s*(\\w+)" config)
    (declare (ignore match))
    (elt name 0)))
