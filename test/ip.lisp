(in-package #:lxc-wrapper-test)

(5am:in-suite ip)

(5am:test test-line-matches-ip
  (5am:is-true (lxc-wrapper::line-matches-ip "10.0.3.4 foo"))
  (5am:is-false (lxc-wrapper::line-matches-ip "10.10.3.4 bar"))
  (5am:is-false (lxc-wrapper::line-matches-ip "foo bar")))

(5am:test test-vector-to-list
  (5am:is-true (eq (type-of (lxc-wrapper::vector-to-list #(1 2 3))) 'cons)))

(5am:test test-line-get-ip
  (5am:is-true (equal '(10 0 3 1) (lxc-wrapper::line-get-ip "10.0.3.1 foobar"))))

(5am:test test-generate-next-ip
  (5am:is-true (equal '(10 0 3 3) (lxc-wrapper::generate-next-ip
				   '((10 0 3 1) (10 0 3 2))))))

(5am:test test-new-ip
  (5am:is-true (equal '(10 0 3 3) (lxc-wrapper::new-ip '(10 0 3 2) 3 3))))

(5am:test test-path-lxc-interfaces
  (5am:is-true (equal #p"/var/lib/lxc/foo/rootfs/etc/network/interfaces"
		      (lxc-wrapper::path-lxc-interfaces "foo"))))
