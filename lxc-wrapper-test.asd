;; -*- mode: common-lisp -*-
(asdf:defsystem :lxc-wrapper-test
  :description "Test package for lxc-wrapper"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on ("lxc-wrapper" "fiveam")
  :components ((:module "test"
			:components
			((:file "test-suites")
			 (:file "cli")
			 (:file "lxc-wrapper")
			 (:file "ip")))))
