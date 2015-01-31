;; -*- mode: common-lisp -*-
(asdf:defsystem :lxc-wrapper
  :description "An opinionated LXC wrapper"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on ("external-program" "cl-ppcre" "alexandria" "apply-argv")
  :in-order-to ((asdf:test-op (asdf:test-op :lxc-wrapper-test)))
  :components ((:file "package")
	       (:file "cli")
	       (:file "lxc-wrapper")
	       (:file "ip")
	       (:file "lxc")))
