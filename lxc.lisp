(in-package #:lxc-wrapper)

(defun init-lxc (name file)
  "Initializes the LXC after creating it. It means:
- Giving it a static IP
- Adding the static IP to the host's /etc/hosts
- Making a symlink to the rootfs somewhere
- Making the container directory readable by all"
  (let ((ip (next-ip file))
	(lxc-path (merge-pathnames (concatenate 'string name "/")
				   *lxc-default-folder*)))
    (assign-static-ip name ip *lxc-gateway* *default-dns-nameserver*)
    (add-ip *hosts-file* ip (concatenate 'string name *lxc-host-extension*))
    (make-lxc-symlink (merge-pathnames *lxc-rootfs* lxc-path)
		      (merge-pathnames name *lxc-folder*))
    (fix-permissions lxc-path)))

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
