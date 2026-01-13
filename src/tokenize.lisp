(in-package :cl-readr)

(defun tokenize-line (line &key (delim #\,) (quote #\") (escape #\\))
  "Splits a line into tokens handling delimiters, quotes, and escapes."
  (let ((tokens '())
        (current-token (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (in-quote nil)
        (len (length line))
        (i 0))
    (loop while (< i len) do
      (let ((char (char line i)))
        (cond
          ;; Case 1: Escape Char Logic
          ;; If we are in a quote, and we see an escape char...
          ((and in-quote escape (char= char escape))
             (cond
               ;; Subcase 1a: Escape == Quote (CSV style "").
               ;; We only treat it as escape if the NEXT char is also quote.
               ((char= escape quote)
                (if (and (< (+ i 1) len)
                         (char= (char line (+ i 1)) quote))
                    ;; It IS a double quote escapement. Consume both, emit one quote.
                    (progn
                      (vector-push-extend quote current-token)
                      (incf i)) ;; Skip the second quote
                    ;; Else, it is NOT an escape. It's just a closing quote.
                    ;; We let it fall through to the Quote Toggle logic? 
                    ;; But we are in the first branch of the main COND.
                    ;; We must handle it here.
                    (setf in-quote (not in-quote))))
               
               ;; Subcase 1b: Backslash escape (or other distinct escape char).
               ;; We always consume the next char literally.
               (t
                (if (< (+ i 1) len)
                    (progn
                      (vector-push-extend (char line (+ i 1)) current-token)
                      (incf i))
                    ;; Trailing escape at EOF? Ignore.
                    nil))))

          ;; Case 2: Quote Toggle
          ((and quote (char= char quote))
           (setf in-quote (not in-quote)))
          
          ;; Case 3: Delimiter (only if not in quote)
          ((and (char= char delim) (not in-quote))
           (push (copy-seq current-token) tokens)
           (setf (fill-pointer current-token) 0))
          
          ;; Case 4: Normal char
          (t
           (vector-push-extend char current-token))))
      (incf i))
    (push (copy-seq current-token) tokens)
    (nreverse tokens)))
