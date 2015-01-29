(in-package #:lxc-wrapper)

(defvar *commands* (make-hash-table :test #'equal))

(defmacro defcommand (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (gethash (symbol-name ',name) *commands*) #',name)))

(defun main (args)
  "CLI entry point"
  ;; @todo
  (let* ((parsed-args (apply-argv:parse-argv (cdr args)))
	 (command (caar parsed-args))
	 (name (cadar parsed-args)))
    (if command
	(funcall (gethash (string-upcase command) *commands*) name parsed-args)
	(help))))

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
~T~TArguments:
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
