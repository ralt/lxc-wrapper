(defpackage #:lxc-wrapper-test
  (:use :cl))

(in-package #:lxc-wrapper-test)

;; If the debugger is fired, it means something went wrong.
(setf fiveam:*debug-on-error* t
      fiveam:*debug-on-failure* t)
(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1)))

(5am:def-suite cli)
(5am:def-suite lxc-wrapper)
(5am:def-suite ip)

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :lxc-wrapper-test))))
  (5am:run! 'cli)
  (5am:run! 'lxc-wrapper)
  (5am:run! 'ip))
