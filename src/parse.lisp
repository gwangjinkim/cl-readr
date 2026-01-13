(in-package :cl-readr)

;;; Constants
(defparameter *default-na* '("NA" "") "Default strings treated as NA")

;;; Helper to check for NA
(defun na-p (val &optional (na *default-na*))
  (if (stringp val)
      (member val na :test #'string=)
      (null val)))

;;; Parsers aimed at singular values (strings or already parsed)
(defun parse-integer-safe (val &optional (jna *default-na*))
  (cond
    ((integerp val) val)
    ((stringp val)
     (if (na-p val jna)
         nil
         (ignore-errors (parse-integer val))))
    (t nil)))

(defun parse-double-safe (val &optional (jna *default-na*))
  (cond
    ((numberp val) (float val 1.0d0))
    ((stringp val)
     (if (na-p val jna)
         nil
         (ignore-errors 
           (let ((*read-eval* nil))
             (read-from-string val)))))
    (t nil)))

(defun true-double-p (val)
  "Checks if val effectively looks like a double."
  (cond
    ((numberp val) t) ;; Already a number
    ((stringp val)
     (let ((v (ignore-errors (read-from-string val))))
       (and (numberp v) (not (complexp v)))))
    (t nil)))

(defun parse-logical-safe (val &optional (jna *default-na*))
  (if (na-p val jna)
      nil
      (cond
        ((eq val t) t)
        ((eq val nil) nil)
        ((stringp val)
         (cond
           ((member val '("T" "TRUE" "True" "true") :test #'string=) t)
           ((member val '("F" "FALSE" "False" "false") :test #'string=) nil)
           (t :error)))
        (t :error))))

;;; Type Guessing
(defun guess-column-type (vals &key (na *default-na*))
  "Guesses the type of a list of values (column).
Order of preference: Logical > Integer > Double > Character."
  (let ((is-logical t)
        (is-integer t)
        (is-double t))
    
    (dolist (s vals)
      (unless (na-p s na)
        ;; Check Logical
        (when is-logical
          (cond
            ((typep s 'boolean) t) 
            ((stringp s)
             (unless (member s '("T" "TRUE" "True" "true" "F" "FALSE" "False" "false") :test #'string=)
               (setf is-logical nil)))
            (t (setf is-logical nil))))
        
        ;; Check Integer
        (when is-integer
          (cond
            ((integerp s) t)
            ((stringp s)
             (multiple-value-bind (int pos) (parse-integer s :junk-allowed t)
               (unless (and int (= pos (length s)))
                  (setf is-integer nil))))
            (t (setf is-integer nil))))
               
        ;; Check Double
        (when is-double
           (unless (true-double-p s)
             (setf is-double nil)))))
             
    (cond
      (is-logical :logical)
      (is-integer :integer)
      (is-double :double)
      (t :character))))

(defun parse-column (vals type &key (na *default-na*))
  "Parses a list of values into a vector of the specified type."
  (ecase type
    (:integer 
     (map 'vector (lambda (s) (parse-integer-safe s na)) vals))
    (:double
     (map 'vector (lambda (s) 
                    (let ((val (parse-double-safe s na)))
                      (if (numberp val) (float val 1.0d0) nil))) 
          vals))
    (:logical
     (map 'vector (lambda (s) (parse-logical-safe s na)) vals))
    (:character
     (map 'vector (lambda (s) 
                    (if (na-p s na) 
                        nil 
                        (if (stringp s) s (format nil "~a" s)))) 
          vals))))
