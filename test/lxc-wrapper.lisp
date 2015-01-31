(in-package #:lxc-wrapper-test)

(5am:in-suite lxc-wrapper)

(5am:test test-adapt-arg
  (5am:is-true (string= "foo" (lxc-wrapper::adapt-arg "foo")))
  (5am:is-true (string= "foo" (lxc-wrapper::adapt-arg 'foo)))
  (5am:is-true (string= "foo" (lxc-wrapper::adapt-arg :foo))))

(5am:test test-commands
  (loop for command in (list (list "CREATE" #'lxc-wrapper::create)
			     (list "START" #'lxc-wrapper::start)
			     (list "STOP" #'lxc-wrapper::stop)
			     (list "LS" #'lxc-wrapper::ls)
			     (list "DESTROY" #'lxc-wrapper::destroy))
     do (progn
	  (5am:is-true (eq (type-of (car (cdr command))) 'function))
	  (multiple-value-bind (fn present)
	      (gethash (car command) lxc-wrapper::*commands*)
	    (declare (ignore fn))
	    (5am:is-true present)))))
