(in-package #:lxc-wrapper)

(defvar *hosts-file* #p"/etc/hosts")
(defvar *lxc-network* '(10 0 3 0))
(defvar *ip-regex* "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)")

(defun next-ip ()
  "Finds the next available IP for the LXC"
  (format
   nil
   "~{~D~^.~}"
   (generate-next-ip
    (with-open-file (file *hosts-file*)
      (loop for line = (read-line file nil)
	 while line
	 when (line-matches-ip line)
	 collect (line-get-ip line))))))

(defun add-ip (file ip host)
  "Adds the ip:extension pair to the hosts file"
  (with-open-file (f file :direction :output :if-exists :append)
    (format f "~%~A ~A~%" ip host)))

(defun line-matches-ip (line)
  "Finds if the line matches an IP. Which means that
the line must starts with an IP address."
  (multiple-value-bind (match numbers)
      (cl-ppcre:scan-to-strings *ip-regex* (car (cl-ppcre:split " " line)))
    (declare (ignore match))
    (unless numbers
      (return-from line-matches-ip))
    (let ((int-numbers (mapcar #'parse-integer (vector-to-list numbers))))
      (loop for i from 0 upto 2 ;; only /24 supported
	 unless (= (elt *lxc-network* i) (elt int-numbers i))
	 do (return-from line-matches-ip nil))
      t)))

(defun vector-to-list (vector)
  (loop for el across vector collect el))

(defun line-get-ip (line)
  "Gets the IP of a line."
  (multiple-value-bind (matches vector)
      (cl-ppcre:scan-to-strings *ip-regex* (car (cl-ppcre:split " " line)))
    (declare (ignore matches))
    (mapcar #'parse-integer (vector-to-list vector))))

(defun generate-next-ip (ips)
  "Generates the next IP from a list of IPs."
  ;; Just try to make sequential IPs using the netmask until one
  ;; doesn't exist.
  ;; We can't just sort the list of IPs and add one, because the IPs
  ;; may not be sequential.
  (let ((numbers (loop for i in ips collect (elt i 3))))
    (loop for i from 2 upto 254
       unless (member i numbers)
       do (return-from generate-next-ip (new-ip *lxc-network* i 3)))))

(defun new-ip (ip number place)
  "Gets a new IP from a full IP and its last number."
  (let ((counter -1))
    (loop for i in ip
       collect (progn
		 (incf counter)
		 (if (= counter place)
		     number
		     i)))))
