(defun c:RESCALEBLOCK (/ make-a-list-properties lst2str BlockDef BlockRef i block name lst)
  ;; Definition of local functions
  (defun make-a-list-properties (lst k-lst value fun flag / key search)
    (if (null (cdr k-lst))
      (if (setq search (assoc (setq key (car k-lst)) lst))
        (subst (cons key (apply fun (if flag (list value (cdr search)) (list (cdr search) value)))) search lst)
        (append lst (list (cons key value)))
      )
      (if (setq search (assoc (setq key (car k-lst)) lst))
        (subst (cons key (make-a-list-properties (cdr search) (cdr k-lst) value fun)) search lst)
        (append lst (list (cons key (make-a-list-properties (cdr search) (cdr k-lst) value fun))))
      )
    )
  )
  (defun lst2str (lst sep)
    (if lst
      (vl-string-left-trim sep (apply 'strcat (mapcar '(lambda (x) (strcat sep (vl-princ-to-string x))) lst)))
    )
  )
  ;; End of the definition of local functions
  (vl-load-com)
  (and
    (setq BlockDef (vla-get-Blocks (vla-get-activedocument (vlax-get-acad-object))))
    (setq BlockRef (ssget '((0 . "INSERT") (66 . 1))))
    (repeat (setq i (sslength BlockRef))
      (setq block (ssname BlockRef (setq i (1- i))))
      (setq name (vla-get-effectivename (vlax-ename->vla-object block)))
      (setq lst
        (make-a-list-properties
          lst
          (list name)
          (/ (+ (cdr (assoc 41 (entget block))) (cdr (assoc 42 (entget block))) (cdr (assoc 43 (entget block)))) 3.0)
          '(lambda (o n) (/ (+ o n) 2.0))
          nil
        )
      )
    )
    (vlax-for block BlockDef
      (setq name (vla-get-name block))
      (if (assoc name lst)
        (progn
          (vlax-put-property block "units" (getvar "INSUNITS"))
          (command "_-BEDIT" name)
          (command "_SCALE" "_All" "" "0,0,0" (cdr (assoc name lst)))
          (command "_BCLOSE" "_Save")
          T
        )
        T
      )
    )
    (repeat (setq i (sslength BlockRef))
      (setq block (vlax-ename->vla-object (ssname BlockRef (setq i (1- i)))))
      (vlax-put-property block "XScaleFactor" 1.0)
      (vlax-put-property block "YScaleFactor" 1.0)
      (vlax-put-property block "ZScaleFactor" 1.0)
      T
    )
    (null (command "_ATTSYNC" "_Name" (lst2str (mapcar 'car lst) ",")))
  )
)