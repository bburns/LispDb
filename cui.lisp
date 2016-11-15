
;-------------------------------------------------------------------------------
; cui.lisp
; this is the console ui
; it implements the generic ui functions for interacting with the console
;-------------------------------------------------------------------------------

(load "lib.lisp")


(define (output str)
  (format t str))

(define (outputln &rest args)
  ; (format t "~a~%" str))
  (dolist (expr args) 
    ; (write expr)) ; lisp: write puts quotes around strings!
    (format t "~a" expr))
  (terpri))

(define (input prompt)
  "let user enter a string at a prompt"
  (output prompt)
  (read-line))

; (define (input-char prompt)
  ; "let user enter a single character at a prompt"
    ; (setq c (read)) ; this treats the input as a symbol. so enter q and it gives you the symbol Q. 
    ; (setq c (read-line stream-in)) ; must hit enter
    ; (setq c (read-char)) ; no need to hit enter

(define (input-choose lst prompt)
  "let user choose from a list of options.
  lst should be an association list of keys and menu strings.
  returns the key of the choice."
  (let (i imax line choice key)
    (setq imax (length lst))
    (until choice
      (setq i 1)
      (dolist (item lst)
        (outputln "[" i "] " (rest item))
        (incf i))
      ; (outputln "[0] Escape")
      (outputln)
      (setq line (input prompt))
      (setq choice (string-to-integer line))
      (if (or (null choice) (<= choice 0) (> choice imax))
        (progn (outputln "Invalid selection.") (setq choice nil))))
    (setq key (first (nth (- choice 1) lst)))
    key))


; tests

(when-run

  (output "hello!")
  (output " there!")
  (outputln "")
  (outputln "argh!")

  (show
   (input-choose '((a . hi) (b . foo) (c . bar) (d . zookr)) "choice: ")
  )
    
)


