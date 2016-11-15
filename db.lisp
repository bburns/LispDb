
;-------------------------------------------------------------------------------
; db.lisp
; this database stores things in an association list, 
; lets you read/write it all to a file, etc.
;-------------------------------------------------------------------------------

; in an alist, the first element of each item is the key. 
; here we're using integer ids. 

; this is basically the lisp ui (lui).
; no i/o here - just return lists. 

;-------------------------------------------------------------------------------

(load "lib.lisp")


; hide the database variables in a closure (rather than in global vars)
(let ((db-alist '()) ;db is just an association list for now
      (db-next-id 1)
      (db-filename nil))

  (defun db-load (filename)
    "load the given file as a database. 
    it should be a text file with a lisp association list in it.
    get-id is a function that gets the id for an element in the db."
    (with-open-file (stm filename)
      (setq db-alist (read stm)) ; this reads in the whole association list as ONE sexpr!
      (setq db-filename filename) ; save the filename
      (if db-alist
        (setq db-next-id (inc (maxlist (mapcar #'first db-alist)))) ; get the max id used
        (setq db-next-id 1)) ; in case list is empty!
      )
    (concat (db-size) " objects loaded from " db-filename "."))

  (defun db-save ()
    "save the database to the current file."
    (with-open-file (stm db-filename  
                          :direction :output  
                          :if-exists :supersede)
        (print db-alist stm) ; write the whole association list as ONE sexpr
        )
    (concat (db-size) " objects saved to " db-filename "."))

  (defun db-save-as (filename)
    "save the database to a new filename."
    (setq db-filename filename)
    (db-save))

  (defun db-add (obj)
    "add a new object to the database.
    the object's first element should be an id of nil.
    returns the new object's id."
    (setq id db-next-id)
    (push (cons id (rest obj)) db-alist) ;destructive! modifies db-alist
    (incf db-next-id)
    id)
    
  (defun db-get (id)
    "retrieve the given object from the database.
    returns nil if the given id is not found"
    (rest (assoc id db-alist)))

  (defun db-remove (id)
    "remove the given object from the database."
    (if (assoc id db-alist)
        (progn
          (setq db-alist (remove (assoc id db-alist) db-alist))
          (concat "Object " id " removed from database."))
        "No such object in database."))
  
  (defun db-list (&optional (where-fn #'(lambda (obj) t)) (sortby-fn #'(lambda (obj) (first obj))))
    "get a list of the objects in the database, matching the optional where predicate,
    and sorted by the field selected with the given sortby fn.
    default where-fn is true (ie all objects).
    default sortby-fn picks out the object id."
    (let (tbl)
      (setq tbl (filter where-fn db-alist))
      (setq tbl (stable-sort tbl #'generic< :key sortby-fn))
      tbl))

  ; no need for search now - just use list, eg (db-list (where "location" "MIT")) !

  (defun db-size ()
    "returns the number of elements in the database"
    (length db-alist))
    
  )


; tests

(when-run

  (test (db-add '(nil bleargh)) 1)
  (test (db-add '(nil foobarzoo)) 2)
  (test (db-size) 2)
  (test (db-get 1) '(bleargh))
  (test (db-get 22) nil)

  (println)

  (show

  (db-load "lisps.db")
  (db-size)
  (db-list)
  (db-get 2)
  (db-remove 4)
  (db-list)
  (db-remove 22)
  (db-list #'(lambda (obj) (equal "MIT" (sixth obj))))

  ; (setq obj (make-object 'lisp 'lisp1 "Lisp 1" "1958" "MIT"))
  ; (setq id (db-add obj))
  ; (db-add (make-object 'lisp 'lisp15 "Lisp 1.5" "1960" "MIT"))
  ; (db-add (make-object 'lisp 'maclisp "MacLisp" "1966" "MIT"))
  ; (db-add (make-object 'lisp 'bbnlisp "BBNLisp" "1964" "BBN"))
  ; (db-add (make-object 'lisp 'muddle "Muddle" "1971" "MIT"))
  ; (db-add (make-object 'lisp 'gcl "GCL" "1994" "UT Austin"))
  ; (db-get id)
  ; (db-list)
  ; (db-delete id) ; works!
  ; (db-list)
  ; obj
  ; (setq obj (db-get 3))
  ; fields
  ; (position "name" fields :test #'equal)
  ; (get-prop "name" obj)
  ; (get-prop "location" obj)
  ; (db-search "location" "MIT") ;works!
  ; (db-search "started" "1971") ;works!
  ; (db-save-as "lisps.txt") ;works!
  );show

);when

