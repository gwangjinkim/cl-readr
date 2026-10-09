(in-package :cl-readr)

;;; Constants
(defparameter *default-na* '("NA" "") "Default strings treated as NA")

;;; Missing values are nil everywhere. NIL is reserved for
;;; logical FALSE, so a missing logical can no longer be confused with FALSE.

;;; Helper to check for NA
(defun na-p (val &optional (na *default-na*))
  (if (stringp val)
      (member val na :test #'string=)
      (null val)))

;;; Number syntax. Cells are never handed to the Lisp reader unless they
;;; are a plain decimal number: reading arbitrary cells would intern
;;; symbols and, without *read-eval* bound, evaluate #.(...) forms
;;; embedded in a data file.

(defun %trim (s)
  (string-trim '(#\Space #\Tab) s))

(defun %digit-run-end (s start)
  "Index after the run of decimal digits starting at START."
  (or (position-if-not #'digit-char-p s :start start) (length s)))

(defun %integer-syntax-p (s)
  "True if S is [+-]?digits (surrounding blanks allowed)."
  (let* ((s (%trim s))
         (n (length s))
         (i (if (and (> n 0) (find (char s 0) "+-")) 1 0)))
    (and (< i n) (= (%digit-run-end s i) n))))

(defun %real-syntax-p (s)
  "True if S is [+-]?(digits[.digits]|.digits)([eEdD][+-]?digits)?
(surrounding blanks allowed)."
  (let* ((s (%trim s))
         (n (length s))
         (i (if (and (> n 0) (find (char s 0) "+-")) 1 0))
         (int-end (%digit-run-end s i))
         (mantissa-digits (- int-end i))
         (j int-end))
    (when (and (< j n) (char= (char s j) #\.))
      (let ((frac-end (%digit-run-end s (1+ j))))
        (incf mantissa-digits (- frac-end (1+ j)))
        (setf j frac-end)))
    (when (zerop mantissa-digits)
      (return-from %real-syntax-p nil))
    (when (and (< j n) (find (char s j) "eEdD"))
      (let* ((k (1+ j))
             (k (if (and (< k n) (find (char s k) "+-")) (1+ k) k))
             (exp-end (%digit-run-end s k)))
        (when (= exp-end k)
          (return-from %real-syntax-p nil))
        (setf j exp-end)))
    (= j n)))

(defun %read-real (s)
  "Read a string that passed %REAL-SYNTAX-P as a DOUBLE-FLOAT."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*read-default-float-format* 'double-float))
      (float (read-from-string (%trim s)) 1.0d0))))

;;; Parsers aimed at singular values (strings or already parsed)
(defun parse-integer-safe (val &optional (jna *default-na*))
  "Parse VAL as an integer; NA strings and unparseable values give NIL."
  (cond
    ((integerp val) val)
    ((and (stringp val) (not (na-p val jna)) (%integer-syntax-p val))
     (parse-integer (%trim val)))
    (t nil)))

(defun parse-double-safe (val &optional (jna *default-na*))
  "Parse VAL as a double-float; NA strings and unparseable values give NIL."
  (cond
    ((realp val) (float val 1.0d0))
    ((and (stringp val) (not (na-p val jna)) (%real-syntax-p val))
     (%read-real val))
    (t nil)))

(defun true-double-p (val)
  "Checks if val effectively looks like a double."
  (cond
    ((realp val) t)
    ((stringp val) (%real-syntax-p val))
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
             (unless (%integer-syntax-p s)
               (setf is-integer nil)))
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
     (map 'vector (lambda (s) (parse-double-safe s na)) vals))
    (:logical
     (map 'vector (lambda (s) (parse-logical-safe s na)) vals))
    (:character
     (map 'vector (lambda (s)
                    (if (na-p s na)
                        nil
                        (if (stringp s) s (format nil "~a" s))))
          vals))))
