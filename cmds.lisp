
;-------------------------------------------------------------------------------
; cmds.lisp
; defines the available commands.
;-------------------------------------------------------------------------------
; this is the user-interactive interface to the db.
; ie each function prompts the user for information.
; each function is called with NO parameters.
;-------------------------------------------------------------------------------

(load "lib.lisp")
(load "db.lisp")
(load "format.lisp")
(load "obj.lisp")
(load "cui.lisp")


(define (cmd-load-db)
  "Load a database"
  (let (filename)
    ; (outputln "A database file is just a text file with an association ")
    ; (outputln "list stored in it, in normal Lisp format.")
    (setq filename (input "Enter the filename to load (eg lisps.txt): "))
    ; (db-load filename)
    (outputln (db-load filename))
    ; (outputln (db-status)) ;?
    ))


(define (cmd-save-db)
  "Save the database"
  ; (outputln (db-save)))
  (let (filename)
    (setq filename (input "Enter the filename to save as (eg lisps.txt): "))
    (outputln (db-save-as filename))))


(define (cmd-add-item)
  "Add a new item to the database"
  ; iterate over avail fields, let user enter values, make object, add to db, print status
  (let (obj value)
    (setq obj '())
    (outputln "Fields: " fields)
    (dolist (fieldname (rest fields)) ; skip id
      (setq value (input (concat fieldname ": ")))
      (setq obj (cons value obj))) ; build up the object list
    (setq obj (reverse obj)) ; reverse the object list
    (setq obj (cons nil obj)) ; add id
    ; (outputln obj)
    (db-add obj)
    (outputln "Object added to database.")
    ; (outputln (db-add obj))
    ))


(define (cmd-remove-item)
  "Remove an item from the database"
  (let (id)
    (setq id (string-to-integer (input "Enter id of object to remove: ")))
    (outputln (db-remove id))))


(define (cmd-list-items)
  "List all items in the database"
  (let (tbl)
    (setq tbl (db-list))
    (outputln (format-table tbl fields))
    (outputln (length tbl) " objects.")))

(defvar fieldmenu (mapcar #'(lambda (field) (cons field field)) fields))
; (show fieldmenu)

(define (cmd-search-items)
  "Search for items in the database"
  (let (fieldname value tbl)
    (setq fieldname (input-choose fieldmenu "Choose the field to search on: "))
    ; (outputln "Possible fields: " fields)
    ; (setq fieldname (input "Enter the fieldname to search on: "))
    (setq value (input "Enter the value to search on: "))
    ; need to convert value to same type as fieldvalue!
    ; (setq value (coerce value (get-field-type fieldname))) ; won't do string to integer. feh.
    ; (setq value (convert value 'string (get-field-type fieldname)))
    (setq value (string-to-other value (get-field-type fieldname)))
    (setq tbl (db-list (where fieldname value)))
    (outputln (format-table tbl fields))
    (outputln (length tbl) " objects.")))


(define (cmd-quit-program)
  "Quit the program"
  (println "Bye..."))


; test
(when-run
  (cmd-load-db)
  (db-load "lisps.db")
  (cmd-list-items)
  (cmd-add-item)
  (cmd-search-items)
  (cmd-remove-item)
  (cmd-list-items)
  (cmd-quit-program)
)

