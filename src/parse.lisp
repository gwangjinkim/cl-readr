(in-package :cl-readr)

;;; Constants
(defparameter *default-na* '("NA" "") "Default strings treated as NA")

;;; Helper to check for NA
(defun na-p (str &optional (na *default-na*))
  (member str na :test #'string=))

;;; Parsers aimed at singular strings
(defun parse-integer-safe (str &optional (jna *default-na*))
  (if (na-p str jna)
      nil
      (ignore-errors (parse-integer str))))

(defun parse-double-safe (str &optional (jna *default-na*))
  (if (na-p str jna)
      nil
      (ignore-errors 
        (let ((*read-eval* nil))
          (read-from-string str))))) ;; READ-FROM-STRING is dangerous on untrusted input, but standard Common Lisp approach for floats. 
          ;; Ideally we implement a regex based float parser or use a library, but simplest for now.
          ;; WARNING: '1/2' reads as ratio. 'SYMBOL' reads as symbol. We need to check type.

(defun true-double-p (str)
  "Checks if string effectively looks like a double."
  ;; Basic check: contains digits, maybe ., e, E, +, -
  ;; And READ-FROM-STRING returns a number (float or integer we cast to float).
  ;; But we only want to promote if it's strictly creating a float or int.
  (let ((val (ignore-errors (read-from-string str))))
    (and (numberp val) (not (complexp val))))) 
    ;; R readr is strict about formats. We'll start loose.

(defun parse-logical-safe (str &optional (jna *default-na*))
  (if (na-p str jna)
      nil
      (cond
        ((member str '("T" "TRUE" "True" "true") :test #'string=) t)
        ((member str '("F" "FALSE" "False" "false") :test #'string=) nil)
        (t :error))))

;;; Type Guessing
(defun guess-column-type (strings &key (na *default-na*))
  "Guesses the type of a list of strings (column).
Order of preference: Logical > Integer > Double > Character."
  ;; Heuristic:
  ;; 1. If all non-NA are logicals -> Logical
  ;; 2. If all non-NA are integers -> Integer
  ;; 3. If all non-NA are doubles -> Double
  ;; 4. Else -> Character
  
  (let ((is-logical t)
        (is-integer t)
        (is-double t))
    
    (dolist (s strings)
      (unless (na-p s na)
        ;; Check Logical
        (when is-logical
          (unless (member s '("T" "TRUE" "True" "true" "F" "FALSE" "False" "false") :test #'string=)
            (setf is-logical nil)))
        
        ;; Check Integer
        (when is-integer
          (multiple-value-bind (int pos) (parse-integer s :junk-allowed t)
            (unless (and int (= pos (length s)))
               (setf is-integer nil))))
               
        ;; Check Double
        (when is-double
           ;; Allow integers to be doubles
           ;; Check if it parses as number
           (unless (true-double-p s)
             (setf is-double nil)))))
             
    (cond
      (is-logical :logical)
      (is-integer :integer)
      (is-double :double)
      (t :character))))

(defun parse-column (strings type &key (na *default-na*))
  "Parses a list of strings into a vector of the specified type."
  (ecase type
    (:integer 
     (map 'vector (lambda (s) (parse-integer-safe s na)) strings))
    (:double
     (map 'vector (lambda (s) 
                    (let ((val (parse-double-safe s na)))
                      (if (numberp val) (float val 1.0d0) nil))) 
          strings))
    (:logical
     (map 'vector (lambda (s) (parse-logical-safe s na)) strings))
    (:character
     (map 'vector (lambda (s) (if (na-p s na) nil s)) strings))))
