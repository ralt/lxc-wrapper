(in-package #:lxc-wrapper-test)

(5am:in-suite cli)

(5am:test test-clean-stars
  (5am:is-true (string= "foo" (lxc-wrapper::clean-stars "*foo*"))))

(5am:test test-help
  (5am:is-true (eq (type-of #'lxc-wrapper::help) 'function))
  (multiple-value-bind (fn present)
      (gethash "HELP" lxc-wrapper::*commands*)
    (declare (ignore fn))
    (5am:is-true present)))
