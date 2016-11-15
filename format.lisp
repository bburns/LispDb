
;-------------------------------------------------------------------------------
; format.lisp
; format a list of lists as a table.
;-------------------------------------------------------------------------------

(defconstant maxcolwidth 20)
(defconstant colgap "  ")

(defun format-row (row widths)
  (let (s gaps cellformats)
    (setq s "")
    ; note: "~12a" pads to 12 chars, but doesn't truncate!
    (setq cellformats (mapcar #'(lambda (w) (concat "~" w "a")) widths)) ; eg "~12a" 
    (setq row (mapcar #'other-to-string row))
    (setq row (mapcar #'left row widths))
    (setq row (mapcar #'(lambda (cell cellformat) (format nil cellformat cell)) row cellformats))
    (setq row (mapcar #'(lambda (cell) (concat cell colgap)) row))
    (setq s (concat s (apply #'concat row)))
    (setq s (concat s (newline)))
    s))

(defun format-table (tbl field-list)
  "returns a string representing the list of lists as a table"
  (let (s widths)
    (setq widths (get-max-col-widths tbl field-list))
    (setq s "")
    (setq s (concat s (format-row field-list widths))) ; column headers
    (setq s (concat s (dup "-" (+ (apply #'+ widths) (* (length colgap) (length widths))))))
    (dolist (row tbl)
      (setq s (concat s (format-row row widths))))
    s))

(defun get-max-col-widths (tbl field-list)
  (let (widths)
    (setq widths (mapcar #'length field-list)) ; get width of col headers
    (dolist (row tbl)
      (map-into widths #'max widths (mapcar #'(lambda (cell) (length (other-to-string cell))) row)))
    ; (setq widths (mapcar #'(lambda (w) (+ w colgap)) widths)) ; add gap between columns
    (setq widths (mapcar #'(lambda (w) (min w maxcolwidth)) widths)) ; limit to maxcolwidth
    widths))


; tests

(when-run

  (setq fields2 '("id" "type" "key" "name" "started" "location"))
  (setq tbl '(
              (1 lisp lisp1 "Lisp 1" 1958 "MIT") 
              (2 lisp lisp1.5 "Lisp 1.5" 1960 "MIT") 
              (3 lisp maclisp "MacLisp" 1966 "MIT") 
              (4 lisp bbnlisp "BBNLisp" 1964 "BBN")
              (5 lisp muddle "Muddle" 1971 "MIT") 
              (6 lisp gcl "GCL" 1994 "UT Austin")
              (7 lisp kcl "Kyoto Common Lisp (KCL)" 1984 "Kyoto Japan")
              ))

  (show
   (get-max-col-widths tbl fields2)
   (format-table tbl fields2)
  )

)

