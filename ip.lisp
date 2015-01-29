(in-package #:lxc-wrapper)

(defvar *hosts-file* #p"/etc/hosts")
;;; IP in list form, appended with the mask
(defvar *lxc-network* '(10 0 3 0 24))
(defvar *ip-regex* "(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)")

(defun next-ip ()
  "Finds the next available IP for the LXC"
  (generate-next-ip
   (with-open-file (file *hosts-file*)
     (loop for line = (read-line file nil)
	while line
	when (line-matches-ip line)
	collect (line-get-ip line)))))

(defun add-ip (file ip extension)
  "Adds the ip:extension pair to the hosts file"
  ;; @todo
  )

(defun line-matches-p (line)
  "Finds if the line matches an IP. Which means that
the line must starts with an IP address."
  (multiple-value-bind (match numbers)
      (cl-ppcre:scan-to-strings *ip-regex* (car (cl-ppcre:split " " line)))
    (declare (ignore match))
    (let ((int-numbers (mapcar #'parse-integer (vector-to-list numbers))))
      (loop for i from 0 upto (- (round (/ (elt *lxc-network* 4) 8)) 1)
	 when (/= (elt *lxc-network* i) (elt int-numbers i))
	 do (return-from line-matches-p nil))
      t)))

(defun vector-to-list (vector)
  (loop for el across vector collect el))

(defun line-get-ip (line)
  "Gets the IP of a line."
  (cl-ppcre:scan-to-strings *ip-regex* (car (cl-ppcre:split " " line))))

(defun generate-next-ip (ips)
  "Generates the next IP from a list of IPs."
  ;; @todo
  )
