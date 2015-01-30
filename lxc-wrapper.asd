(asdf:defsystem #:lxc-wrapper
  :description "My personal LXC wrapper"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on ("external-program" "cl-ppcre" "alexandria" "apply-argv")
  :components ((:file "package")
	       (:file "lxc-wrapper")
	       (:file "cli")
	       (:file "ip")
	       (:file "lxc")))
