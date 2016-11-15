
; -------------------------------------------------------------------------------
; lispdb
; Main program
; -------------------------------------------------------------------------------

(load "lib.lisp")
(load "cmds.lisp")
(load "db.lisp")
(load "obj.lisp")
(load "format.lisp")
(load "cui.lisp")


(defconstant commands '(
  cmd-load-db 
  cmd-list-items 
  cmd-search-items 
  cmd-add-item 
  cmd-remove-item 
  cmd-save-db 
  cmd-quit-program
  ))

; define the main menu by extracting docstrings from the command functions.
(defvar menu (mapcar #'(lambda (cmd) (cons cmd (docstring cmd))) commands))

(define (display-welcome)
  (outputln "Welcome to the database."))

(define (display-status)
  (outputln (db-size) " objects in database."))

(define (main-loop)
  "get cmd, execute it, display results and status message. repeat."
  (outputln)
  (display-welcome)
  (display-status)
  (let (cmd)
    (until (equal cmd 'cmd-quit-program)
      (outputln)
      (setq cmd (input-choose menu "Command: "))
      (funcall cmd))))


(main-loop)

