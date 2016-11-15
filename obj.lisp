
;-------------------------------------------------------------------------------
; obj.lisp
; a really simple object system
;-------------------------------------------------------------------------------

(load "lib.lisp")


; specify position of fields
(defconstant fields '("id" "type" "key" "name" "started" "location"))
(defconstant field-types '(integer string string string string string))


(define (get-field-type fieldname)
  (nth (position fieldname fields :test #'equal) field-types))

(define (make-object &rest args)
  args)

(define (get-prop fieldname obj)
  (nth (position fieldname fields :test #'equal) obj))

(define (get-id obj)
  (first obj))

(define (set-id obj id)
  (setf (first obj) id))

; so can say (db-list (where "location" "MIT"))
(define (where fieldname value)
  (lambda (obj) (generic= value (get-prop fieldname obj))))

(define (sortby fieldname)
  (lambda (obj) (get-prop fieldname obj)))


; tests

(when-run

  (setq obj (make-object nil 'lisp 'lisp1.5 "Lisp 1.5" "1960" "MIT"))
  (test (get-prop "name" obj) "Lisp 1.5")
  (test (get-prop "location" obj) "MIT")
  (test (get-id obj) nil)
  (test (set-id obj 1) 1)
  (test (get-id obj) 1)
  (test (get-field-type "id") 'integer)
  (test (get-field-type "name") 'string)
  
  (show
  
   (where "location" "MIT") ;=> #<function lambda (obj) (equal value (get-prop fieldname obj))>
   (sortby "started") ;=> #<function lambda (obj) (get-prop fieldname obj)>

   (funcall (where "location" "MIT") obj) ;=> t
   (funcall (where "id" 1) obj) ;=> t
   (funcall (where "id" 0) obj) ;=> nil
   
   (load "db.lisp")
   (db-load "lisps.db")
   (db-list (where "location" "MIT") (sortby "name"))
  
  )
)


