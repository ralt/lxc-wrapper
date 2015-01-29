(in-package #:lxc-wrapper)

(defvar *lxc-default-folder* #p"/var/lib/lxc/")
(defvar *lxc-rootfs* #p"rootfs")
(defvar *lxc-folder* (merge-pathnames #p"lxc/" (user-homedir-pathname)))
(defvar *lxc-host-extension* ".lxc")
(defvar *lxc-gateway* "10.0.3.1")
(defvar *default-dns-nameserver* "8.8.8.8")

(defun init-lxc (name file)
  "Initializes the LXC after creating it. It means:
- Giving it a static IP
- Adding the static IP to the host's /etc/hosts
- Making a symlink to the rootfs somewhere"
  (let ((ip (next-ip file)))
    (assign-static-ip name ip *lxc-gateway* *default-dns-nameserver*)
    (add-ip *hosts-file* ip (concatenate 'string name "." *lxc-host-extension*))
    (make-lxc-symlink
     (merge-pathnames (merge-pathnames *lxc-rootfs* name) *lxc-default-folder*)
     (merge-pathnames name *lxc-folder*))))

(defun remove-lxc-leftovers (name)
  "Removes the leftovers such as:
- The IP in /etc/hosts
- The symbolic link to the now-missing rootfs"
  (remove-ip *hosts-file* name)
  (delete-file (merge-pathnames name *lxc-folder*)))

(defun make-lxc-symlink (base end)
  "Makes a symlink from end to base"
  (run
    "ln"
    "-s" base end))
