(in-package #:lxc-wrapper)

;; Default behavior is to get in the debugger,
;; change that.
(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1)))

(defvar *commands* (make-hash-table :test #'equal))
(defvar *lxc-default-folder* #p"/var/lib/lxc/")
(defvar *lxc-rootfs* #p"rootfs/")
(defvar *lxc-folder* (merge-pathnames #p"lxc/" (user-homedir-pathname)))
(defvar *lxc-host-extension* ".lxc")
(defvar *lxc-gateway* "10.0.3.1")
(defvar *default-dns-nameserver* "8.8.8.8")
(defvar *default-shell* #p"/bin/bash")
(defvar *hosts-file* #p"/etc/hosts")
(defvar *lxc-network* '(10 0 3 0))
(defvar *ip-regex* "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)")
(defvar *lxc-interfaces-file* #p"etc/network/interfaces")

(defmacro defcommand (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (gethash (symbol-name ',name) *commands*) #',name)))

(defmacro default-variables-let (vars &body body)
  `(let (,@(loop for var in vars
	      collect `(,var (or
			      (getf (cdr parsed-args)
				    (intern
				     (clean-stars (symbol-name ',var))
				     "KEYWORD"))
			      ,var))))
     ,@body))

(defun clean-stars (var)
  "Removes the stars from a string"
  (cl-ppcre:regex-replace-all "\\*" var ""))

(defun main (args)
  "CLI entry point"
  (handler-case
      (let* ((parsed-args (apply-argv:parse-argv (cdr args)))
	     (command (caar parsed-args))
	     (name (cadar parsed-args)))
	;; *lxc-network* and *ip-regex* are voluntarily not available
	(default-variables-let (*lxc-default-folder*
				*lxc-rootfs*
				*lxc-folder*
				*lxc-host-extension*
				*lxc-gateway*
				*default-dns-nameserver*
				*hosts-file*
				*lxc-interfaces-file*
				*default-shell*)
	  (if command
	      (funcall (gethash (string-upcase command) *commands*) name (cdr parsed-args))
	      (help))))
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
~T~T~T--lxc-default-folder, --lxc-rootfs, --lxc-folder, --lxc-extension, --lxc-gateway, --default-dns-nameserver, --hosts-file, --lxc-interfaces-file

~Tstart NAME
~T~Tstarts the container named NAME

~Tstop NAME
~T~Tstops the container named NAME

~Tls
~T~Tlists the containers

~Tdestroy NAME
~T~Tdestroys the container named NAME

~T~TOptions (must be BEFORE the command):
~T~T~T--lxc-folder, --lxc-host-extension, --hosts-file

~TOptions for all commands (must be BEFORE the command):
~T~T--default-shell

"))
