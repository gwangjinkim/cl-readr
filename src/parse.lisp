(in-package :cl-readr)

;;; Constants
(defparameter *default-na* '("NA" "") "Default strings treated as NA")

;;; Missing values are cl-vctrs-lite:*na* everywhere. NIL is reserved for
;;; logical FALSE, so a missing logical can no longer be confused with FALSE.

;;; Helper to check for NA
(defun na-p (val &optional (na *default-na*))
  (cond ((stringp val) (member val na :test #'string=))
        ((cl-vctrs-lite:na-p val) t)
        (t (null val))))

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
  "Parse VAL as an integer; NA strings and unparseable values give *NA*."
  (cond
    ((integerp val) val)
    ((and (stringp val) (not (na-p val jna)) (%integer-syntax-p val))
     (parse-integer (%trim val)))
    (t cl-vctrs-lite:*na*)))

(defun parse-double-safe (val &optional (jna *default-na*))
  "Parse VAL as a double-float; NA strings and unparseable values give *NA*."
  (cond
    ((realp val) (float val 1.0d0))
    ((and (stringp val) (not (na-p val jna)) (%real-syntax-p val))
     (%read-real val))
    (t cl-vctrs-lite:*na*)))

(defun true-double-p (val)
  "Checks if val effectively looks like a double."
  (cond
    ((realp val) t)
    ((stringp val) (%real-syntax-p val))
    (t nil)))

(defun parse-logical-safe (val &optional (jna *default-na*))
  (if (na-p val jna)
      cl-vctrs-lite:*na*
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

(define-condition parse-failure (warning)
  ((count :initarg :count :reader parse-failure-count)
   (type :initarg :type :reader parse-failure-type)
   (inputs :initarg :inputs :reader parse-failure-inputs
           :documentation "The cells that failed, in order (at most 10)."))
  (:report (lambda (c stream)
             (format stream "~d parsing failure~:p (expected ~(~a~))~@[: ~{~s~^, ~}~]."
                     (parse-failure-count c) (parse-failure-type c)
                     (parse-failure-inputs c))))
  (:documentation "Signalled by PARSE-COLUMN when non-missing cells cannot
be parsed as the requested type; they become *NA* (like R's readr)."))

(defun %parse-column-values (vals type na)
  (ecase type
    (:integer
     (map 'vector (lambda (s) (parse-integer-safe s na)) vals))
    (:double
     (map 'vector (lambda (s) (parse-double-safe s na)) vals))
    (:logical
     (map 'vector (lambda (s)
                    (let ((v (parse-logical-safe s na)))
                      (if (eq v :error) cl-vctrs-lite:*na* v)))
          vals))
    (:character
     (map 'vector (lambda (s)
                    (if (na-p s na)
                        cl-vctrs-lite:*na*
                        (if (stringp s) s (format nil "~a" s))))
          vals))))

(defun parse-column (vals type &key (na *default-na*))
  "Parses a list of values into a vector of the specified type.
Missing values become cl-vctrs-lite:*na*. Non-missing values that cannot be
parsed also become *na*, and one PARSE-FAILURE warning is signalled for the
column (as R's readr does)."
  (let* ((out (%parse-column-values vals type na))
         (failed (loop for raw in (coerce vals 'list)
                       for v across out
                       when (and (cl-vctrs-lite:na-p v) (not (na-p raw na)))
                         collect raw)))
    (when failed
      (warn 'parse-failure :count (length failed) :type type
                           :inputs (subseq failed 0 (min 10 (length failed)))))
    out))
