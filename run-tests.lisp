(in-package #:cl-user)


(ql:quickload #:lxc-wrapper)
(ql:quickload #:lxc-wrapper-test)

;; If the debugger is fired, it means something went wrong.
(setf fiveam:*debug-on-error* t
      fiveam:*debug-on-failure* t)
(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1)))

;; Run tests
(in-package #:lxc-wrapper-test)

(5am:run! 'cli)
(5am:run! 'lxc-wrapper)
(5am:run! 'ip)

;; Everything went fine
(uiop:quit 0)
