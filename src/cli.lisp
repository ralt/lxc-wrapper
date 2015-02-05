(in-package #:lxc-wrapper)

;; Default behavior is to get in the debugger,
;; change that.
(defvar *debug* nil)
(defvar *original-hook* *debugger-hook*)
(setf *debugger-hook*
      (lambda (c h)
	(if *debug*
	    (funcall *original-hook* c h)
	    (progn
	      (format t "An internal error occured. Are you sure the options are before the command?~%")
	      (uiop:quit -1)))))

(defvar *commands* (make-hash-table :test #'equal))
(defvar *doc-strings* (make-hash-table :test #'equal))
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
(defvar *lxc-package-extension* ".tar.gz")
(defvar *lxc-config* #p"config")

(defmacro defcommand (name args doc-string &body body)
  ;; Using this to be able to use (check-type)
  (let ((doc (gensym)))
    `(progn
       (let ((,doc ,doc-string))
	 (check-type ,doc string)
	 (defun ,name ,args
	   ,@body)
	 (setf (gethash (symbol-name ',name) *commands*) #',name)
	 (setf (gethash (symbol-name ',name) *doc-strings*) ,doc)))))

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
	(default-variables-let (*debug*
				*lxc-default-folder*
				*lxc-rootfs*
				*lxc-folder*
				*lxc-host-extension*
				*lxc-gateway*
				*default-dns-nameserver*
				*hosts-file*
				*lxc-interfaces-file*
				*lxc-package-extension*
				*lxc-config*
				*default-shell*)
	  (if (and command (gethash (string-upcase command) *commands*))
	      (funcall (gethash (string-upcase command) *commands*) name (cdr parsed-args))
	      (help nil nil))))))

(defcommand help (name args)
  "help
	Shows this help"
  (declare (ignore name))
  (declare (ignore args))
  (format t
	  "Usage: lxc-wrapper [OPTIONS] [COMMAND]
Wrapper around lxc for an opinionated workflow.

Commands:
")
  (maphash #'(lambda (name doc-string)
	       (declare (ignore name))
	       (format t
		       "~%~{	~A~%~}"
		       (cl-ppcre:split "\\n" doc-string)))
	   *doc-strings*)
  (format t "
	Overridable variables and default values for all commands (must be BEFORE the command):
		--default-shell=/bin/bash

"))

(defcommand version (name args)
  "version
	Shows the version of lxc-wrapper"
  (declare (ignore name))
  (declare (ignore args))
  ;; load-time-value because it's built with buildapp
  (format t "~A~%" (load-time-value (sb-posix:getenv "VERSION"))))
