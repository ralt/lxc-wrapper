;;;; lxc-wrapper.asd

(asdf:defsystem #:lxc-wrapper
  :description "My personal LXC wrapper"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on ("external-program")
  :components ((:file "package")
	       (:file "ip")
               (:file "lxc-wrapper")))
