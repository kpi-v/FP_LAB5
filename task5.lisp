;-----------------------------------------------------
; adittional functions
;-----------------------------------------------------
(defun split-string (string delimiter)
  (let ((start 0)
        (result '()))
    (loop for pos = (position delimiter string :start start)
          while pos do
            (push (subseq string start pos) result)
            (setf start (1+ pos)))
    (push (subseq string start) result)
    (reverse result)))

(defun parse-value (value)
  (cond ((string= value "false") nil)
        ((parse-integer value :junk-allowed t))
        (t value)))

;-----------------------------------------------------
; main functions
;-----------------------------------------------------

(defun read-csv-to-hash (file-path)
  (with-open-file (stream file-path :direction :input)
    (let* ((header (split-string (read-line stream) #\,))
           (records '()))
      (loop for line = (read-line stream nil nil)
            while line do
              (let* ((values (mapcar #'parse-value (split-string line #\,)))
                     (record (make-hash-table :test 'equal)))
                (loop for key in header
                      for value in values
                      do (setf (gethash key record) value))
                (push record records)))
      (reverse records))))

(defun select (file-path)
  (let ((records (read-csv-to-hash file-path)))
    (lambda (&key (filter nil))
      (if filter
          (remove-if-not (lambda (record)
                           (every (lambda (pair)
                                    (equal (gethash (car pair) record) (cdr pair)))
                                  filter))
                         records)
          records))))

(defun write-hashes-to-csv (records file-path)
  (when records
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (let* ((header (loop for k being the hash-keys of (first records) collect k)))
        (format stream "~{~A~^,~}~%" header)
        (dolist (record records)
          (format stream "~{~A~^,~}~%"
                  (loop for key in header
                        collect (gethash key record ""))))))))

(defun hash-to-alist (hash)
  (let ((alist '()))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             hash)
    alist))

(defun print-records (records)
  (dolist (record records)
    (maphash (lambda (k v)
               (format t "~A: ~A~%" k v))
             record)))

;-----------------------------------------------------
; test functions
;-----------------------------------------------------

(defun test-utils ()
  (format t "--- Tests ---~%")
  (let ((file "drones.csv"))
    (format t "~%Read csv to hash test: ~%")
    (let ((records (read-csv-to-hash file)))
      (print-records records)))

  (let ((file "drones.csv"))
    (format t "~%Select test: ~%")
    (let ((selector (select file)))
      (let ((all-records (funcall selector)))
        (format t "All records: ~%")
        (print-records all-records))
      (let ((filtered-records (funcall selector :filter '(("Model" . "X1")))))
        (format t "Filtred records (Model=X1): ~%")
        (print-records filtered-records))))

  (let ((output-file "output.csv"))
    (format t "~%Write to csv test: ~%")
    (let ((records (read-csv-to-hash "drones.csv")))
      (write-hashes-to-csv records output-file)
      (format t "Records saved to ~A~%" output-file)))

  (format t "~%Convert hash to align test: ~%")
  (let ((hash (make-hash-table :test 'equal)))
    (setf (gethash "id" hash) "123"
          (gethash "name" hash) "Drone A"
          (gethash "price" hash) "1000")
    (let ((expected '(("id" . "123") ("name" . "Drone A") ("price" . "1000")))
          (actual (hash-to-alist hash)))
      (if (equalp actual expected)
          (format t "Test passed: ~A~%" actual)
          (format t "Test failed: Expected ~A, but got ~A~%" expected actual)))))

(test-utils)


