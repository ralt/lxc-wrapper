;;;; package.lisp

(defpackage #:lxc-wrapper
  (:use #:cl)
  (:export
   :main
   ;; Functions
   :create :start :stop :ls :destroy
   ;; Variables
   :*lxc-default-folder* :*lxc-rootfs* :*lxc-folder*
   :*lxc-host-extension* :*lxc-gateway* :*default-dns-nameserver*
   :*hosts-file* :*lxc-network* :*ip-regex* :*lxc-interfaces-file
   :*default-shell*))
