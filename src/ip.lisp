(in-package #:lxc-wrapper)

(defun next-ip (file)
  "Finds the next available IP for the LXC"
  (format
   nil
   "~{~D~^.~}"
   (generate-next-ip
    (with-open-file (f file)
      (loop for line = (read-line f nil)
	 while line
	 when (line-matches-ip line)
	 collect (line-get-ip line))))))

(defun add-ip (file ip host)
  "Adds the ip:extension pair to the hosts file"
  (with-open-file (f file :direction :output :if-exists :append)
    (format f "~A ~A~%" ip host)))

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

(defun assign-static-ip (name ip gateway dns)
  "Assigns a static IP to an LXC"
  ;; @todo
  (let ((path (path-lxc-interfaces name)))
    (lxc-delete-file path)
    (with-open-file (file path :if-does-not-exist :create :direction :output)
      (format file "
auto lo
iface lo inet loopback

auto eth0
iface eth0 inet static
	address ~A
	gateway ~A
	dns-nameserver ~A~%" ip gateway dns))))

(defun path-lxc-interfaces (name)
  "Returns the path to the LXC interfaces file"
  (merge-pathnames
   *lxc-interfaces-file*
   (merge-pathnames
    *lxc-rootfs*
    (merge-pathnames (concatenate 'string name "/") *lxc-default-folder*))))

(defun remove-ip (file name)
  "Removes a line from the hosts file"
  ;; @todo
  (let ((hosts (alexandria:read-file-into-string file)))
    (lxc-delete-file file)
    (alexandria:write-string-into-file
     (cl-ppcre:regex-replace (concatenate
			      'string
			      "\\d+\\.\\d+\\.\\d+\\.\\d+ "
			      name
			      "\\" *lxc-host-extension*
			      "\\n")
			     hosts
			     "")
     file)))
