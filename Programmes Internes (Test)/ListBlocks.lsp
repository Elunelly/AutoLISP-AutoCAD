(defun c:ListBlocks (/ flt_tbl str filename file)
  (defun flt_tbl (tag search / lst value)
    (setq value (cdr (assoc 2 (tblnext tag t))))
    (while (/= value nil)
      (if (= (wcmatch value search) t)
        (setq lst (cons value lst))
      )
      (setq value (cdr (assoc 2 (tblnext tag))))
    )
    lst
  )
  
  (if
    (and
      (setq str (getvar "DWGNAME"))
      (setq str (substr str 1 (- (strlen str) 4)))
      (setq filename (getfiled "File creation" (strcat "Z:\\BUREAU D'ETUDES\\TOOLKIT\\AUTOCAD\\LISP\\Aide - Utilisateurs\\" str) "csv" 1))
      (setq file (open filename "W"))
    )
    (mapcar
      '(lambda (x) (write-line x file))
      (vl-sort
        (vl-remove-if '(lambda (x) (wcmatch x "`**")) (flt_tbl "BLOCK" "*"))
        '(lambda (e1 e2) (< (strcase e1 T) (strcase e2 T)))
      )
    )
  )
  (close file)
  filename
)