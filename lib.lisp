
;-------------------------------------------------------------------------------
; lib
; library for making common lisp more scheme-like
;-------------------------------------------------------------------------------

; controls how symbols are displayed
; options: upcase, downcase, capitalize
(setq *print-case* :downcase)


(defmacro define (nameargs &body body)
  "Define a function, as in scheme/newlisp. Because it's more lispy. 
  eg (define (foo x) (+ x 5))"
  `(defun ,(first nameargs) ,(rest nameargs) ,@body))

(defmacro define-macro (nameargs &body body)
  "Define a macro, as in newlisp. Because it's more lispy. 
  eg (define-macro (foo x) (+ x 5))"
  `(defmacro ,(first nameargs) ,(rest nameargs) ,@body))

(define (equal-epsilon x y &optional (epsilon 1e-8))
  "Same as equal but if args are numeric will check if they are within epsilon of each other"
  (if (and (numberp x) (numberp y))
      (< (abs (- x y)) epsilon)
      (equal x y)))

(define-macro (test function-call expected-form &optional (epsilon 1e-8))
  "Tests a function call against the expected value, eg (test (c-to-f 25) 77).
  If they're both numbers, will compare them with epsilon."
  (let ((fncallstr (format nil "~a" function-call))
         ; (expectedstr (format nil "~a" expected-form))
         (actual-value (eval function-call)) ; evaluate the function call to get the actual result
         ; and have to evaluate the expected argument as well, in case it's something like (cons 3 4). 
         ; a simple number will eval to itself. 
         (expected-value (eval expected-form))) 
    (if (equal-epsilon actual-value expected-value epsilon)
        (format t "[OK] ~a => ~a~%" fncallstr expected-value) 
        (format t "[FAILED] ~a => ~a [expected ~a]~%" fncallstr actual-value expected-value))))

(define (eof stm &optional (skip-whitespace nil))
  "Returns true if the given stream is at the end.
  eg (until (eof stm) (println (read-line stm)))
  eg (until (eof stm t) (println (read stm)))"
  ; If peek-type [ie skip-whitespace] is t, then peek-char skips over whitespace characters, but not comments, 
  ; and then performs the peeking operation on the next character.
  (null (peek-char skip-whitespace stm nil nil))) 

(define (string-to-integer s)
  (parse-integer s :junk-allowed t))

(define (other-to-string o)
  (format nil "~a" o))

(define (string-to-other s to)
  (case to
    ('integer (string-to-integer s))
    (otherwise s)))

(define (left s nchars)
  (subseq s 0 (min (length s) nchars)))

(define (consif x lst)
  "this is useful when merging lists together - sometimes want to drop out elements." 
  (if x (cons x lst) lst))

(define (line)
  (println (dup "-" 80)))

(define (symbol< x y)
  (not (null (string-lessp (symbol-name x) (symbol-name y)))))

(define (symbol= x y)
  (not (null (string-equal (symbol-name x) (symbol-name y)))))

; generic operators
; =  string-equal
; <  string<  [case-sensitive]
; <  string-lessp  [case-insensitive]
; in newlisp, (< "A" "a") is true (case-sensitive)

(define (generic< x y)
  (cond ((stringp x) (string-lessp x y))
          ((symbolp x) (symbol< x y))
          (t (< x y))))

(define (generic= x y)
  (cond ((stringp x) (string-equal x y))
          ((symbolp x) (symbol= x y))
          (t (= x y))))

(define-macro (compare x y less equ more)
  "Compares x with y and returns one of the three given results,
  for x<y, x=y, or x>y.
  Currently just uses generic< function. "
    `(if (generic< ,x ,y)
        ,less
        (if (generic< ,y ,x)
            ,more
            ,equ)))

(define (lastv vec)
  (elt vec (- (length vec) 1)))

(define-macro (when-run &body body)
  "Wrap test code for a file in this macro, so it will only run when you're
  actually running the file directly, instead of loading it from another file."
  ; need namestring to convert pathname to a string
  `(when (ends-with (namestring *load-pathname*) (lastv (argv))) ,@body))

(define (filter predicate lst)
  (remove-if-not predicate lst))

(define (starts-with str substr)
  ; if search finds substr at position 0, then str starts with substr
  ; (= 0 (search substr str))) ; bombs if nil!
  (let (pos pos-expected)
    (setq pos (search substr str))
    (setq pos-expected 0)
    (and (not (null pos)) (= pos-expected pos))))

(define (ends-with str substr)
  (let (pos pos-expected)
    (setq pos (search substr str :from-end t))
    (setq pos-expected (- (length str) (length substr)))
    (and (not (null pos)) (= pos-expected pos))))

(define (empty? lst)
  (null lst))

(define (top stack)
  (first stack))

(define (concat &rest args)
  (format nil "~{~A~}" args))

(define (docstring fn)
  "Returns the documentation string associated with the given function or macro"
  (documentation fn 'function))

(define (newline)
  (format nil "~%"))

(define (inc x)
  (1+ x))

(define (dec x)
  (1- x))
  
(define (maxlist lst)
  (apply #'max lst))

(define (println &rest args) 
  "Print a string followed by a newline. If no arguments given will print a blank line."
  (dolist (expr args) 
    (format t "~a" expr))
  (terpri))

(define-macro (while condn . body)
  "Your basic while loop"
  `(do nil ((not ,condn)) ,@body)) 

(define-macro (until condn . body)
  "Your basic until loop"
  `(do nil (,condn) ,@body))

(define-macro (show &body body)
  "Show some expressions and what they evaluate to"
  (dolist (expr body) 
    (if (equal "" expr)
        (println)
        (println expr " => " (eval expr)))))

(define (unfold start stop step-fn)
  "fold and unfold are duals of each other"
  (if (equal start stop)
      (list stop)
      (cons start (unfold (funcall step-fn start) stop step-fn))))

(define (sequence-num num-start num-end)
  (unfold num-start num-end #'1+))

(define-macro (alias short long)
  "from paul graham's on lisp (renamed from abbrev)" 
  `(defmacro ,short (&rest args)
    `(,',long ,@args)))

(alias mvbind multiple-value-bind)

(define (int val)
  "Convert the given value to an integer"
  (coerce val 'integer))

(define (double val)
  "Convert the given value to a double float"
  (coerce val 'double-float))

(define (degrees-to-radians theta)
  (/ (* theta pi) 180))

(define (dup expr &optional (n 2))
  (make-string n :initial-element (character expr)))

(define-macro (with-unlock &body body)
  `(progn 
     (unlock) 
     ,@body 
     (lock)))

