;;;; package.lisp

(defpackage #:lxc-wrapper
  (:use #:cl)
  (:export :create :start :stop :ls :destroy))
