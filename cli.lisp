(in-package #:lxc-wrapper)

;; Default behavior is to get in the debugger,
;; change that.
(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1)))

(defvar *commands* (make-hash-table :test #'equal))

(defmacro defcommand (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (gethash (symbol-name ',name) *commands*) #',name)))

(defun main (args)
  "CLI entry point"
  (handler-case
      (let* ((parsed-args (apply-argv:parse-argv (cdr args)))
	     (command (caar parsed-args))
	     (name (cadar parsed-args)))
	(if command
	    (funcall (gethash (string-upcase command) *commands*) name (cdr parsed-args))
	    (help)))
    (error () (format *error-output* "An internal error occured. Are you sure the options are before the command?~%"))))

(defcommand help (&rest args)
  "Help output"
  (declare (ignore args))
  (format
   t
   "Usage: lxc-wrapper [OPTIONS] [COMMAND]
Wrapper around lxc for an opinionated workflow.

Commands:

~Tcreate NAME
~T~Tcreates a container named NAME

~T~TOptions (must be BEFORE the command):
~T~T~T--base=BASE
~T~T~T~Tclone BASE
~T~T~T--template=TEMPLATE
~T~T~T~Tuse the TEMPLATE lxc template

~Tstart NAME
~T~Tstarts the container named NAME

~Tstop NAME
~T~Tstops the container named NAME

~Tls
~T~Tlists the containers

~Tdestroy NAME
~T~Tdestroys the container named NAME

"))
