(in-package :cl-readr)

(defun quote-field (field &key (delim #\,) (quote #\") (escape #\\))
  "Properly quotes a field if necessary."
  ;; Convert to string if not
  (let ((s (format nil "~a" field)))
    (if (or (find delim s) (find quote s) (find #\Newline s) (find #\Return s))
        (with-output-to-string (out)
          (write-char quote out)
          (loop for char across s do
            (when (char= char quote)
              (write-char escape out)) ;; Usually escape for quote is doubling quote in CSV
            (write-char char out))
          (write-char quote out))
        s)))

(defun write-delim (x file &key (delim #\,) (na "NA") (append nil) (col-names t))
  "Writes a tibble X to FILE in delimited format."
  (let ((direction (if append :append :output))
        (if-exists (if append :append :supersede)))
    
    (with-open-file (stream file :direction :output :if-exists if-exists :if-does-not-exist :create)
      (let* ((names (coerce (cl-tibble:tbl-names x) 'list))
             (ncols (length names))
             (nrows (cl-tibble:tbl-nrows x)))
        
        ;; Header
        (when col-names
          (loop for name in names
                for i from 0
                do (write-string (quote-field name :delim delim) stream)
                   (unless (= i (1- ncols)) (write-char delim stream)))
          (write-char #\Newline stream))
        
        ;; Rows
        (let ((cols (loop for name in names collect (cl-tibble:tbl-col x name))))
          (loop for i from 0 below nrows do
            (loop for col in cols
                  for j from 0
                  do
                  (let ((val (aref col i)))
                    (if (null val)
                        (write-string na stream)
                        (write-string (quote-field val :delim delim) stream)))
                  (unless (= j (1- ncols)) (write-char delim stream)))
            (write-char #\Newline stream)))))))

(defun write-csv (x file &rest args)
  "Writes a tibble to a CSV file."
  (apply #'write-delim x file :delim #\, args))

(defun write-tsv (x file &rest args)
  "Writes a tibble to a TSV file."
  (apply #'write-delim x file :delim #\Tab args))

(defun write-excel (x file &key (sheet "Sheet1"))
  "Writes a tibble X to an Excel FILE.
   Uses CL-EXCEL:WRITE-XLSX."
   ;; Convert tibble to list of lists (header + rows)
   (let* ((names (coerce (cl-tibble:tbl-names x) 'list))
          (pnames (mapcar #'string-capitalize names)) ;; Excel headers usually nice? Or just raw.
          (nrows (cl-tibble:tbl-nrows x))
          (cols (loop for name in names collect (cl-tibble:tbl-col x name)))
          (data '()))
     
     ;; Collect rows
     (loop for i from 0 below nrows do
       (push (loop for col in cols collect (aref col i)) data))
     (setf data (nreverse data))
     
     ;; Add header
     (push pnames data)
     
     ;; Write
     (cl-excel:write-xlsx data file)))
