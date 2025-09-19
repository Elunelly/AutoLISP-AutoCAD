(defun c:MAC (/ SetVarList)
  (defun SetVarList (lst)
    (mapcar
      '(lambda (x / var sym val)
        (setq
          var (car x)
          sym (cadr x)
          val (caddr x)
        )
        (set sym (getvar var))
        (if val
          (setq val (setvar var val))
          (setq val (vl-symbol-value sym))
        )
        (list var sym val)
      )
      lst
    )
  )
  
  (SetVarList (list (list "CTAB" '$layout $layout)))
  (princ
    (strcat
      "\nLes présentations \""
      $layout
      "\" et \""
      (getvar "CTAB")
      "\" sont désormais liées..."
    )
  )
  (princ)
)