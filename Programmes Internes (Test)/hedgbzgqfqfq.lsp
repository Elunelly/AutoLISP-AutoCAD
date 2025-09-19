(defun ListBoxWilly95 (lst / *error* str2lst filename file DCL_ID rslt)
  (defun *error* (msg)
    (if file (close file))
    (if filename (vl-file-delete filename))
    (princ msg)
  )
  (defun str2lst (str sep / pos)
    (if (setq pos (vl-string-search sep str))
      (cons
        (substr str 1 pos)
        (str2lst (substr str (+ (strlen sep) pos 1)) sep)
      )
      (list str)
    )
  )
  (setq
    filename (vl-filename-mktemp "Willy95.dcl")
    file (open filename "W")
  )
  (write-line
    (strcat
      "ListBoxM:dialog {"
      "   label = \"1, 2 ou 3 choix\" ;"
      "   :list_box {"
      "     label = \"Choisir :\" ;"
      "     key = \"key\" ;"
      "     multiple_select = true ;"
      "   }"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog filename))
  (if (not (new_dialog "ListBoxM" DCL_ID))
    (exit)
  )
  (start_list "key")
  (mapcar 'add_list lst)
  (end_list)
  (action_tile "accept" "(foreach n (str2lst (get_tile \"key\") \" \") (setq rslt (cons (nth (atoi n) lst) rslt))) (done_dialog)")
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete filename)
  (reverse rslt)
)



(defun c:RBx (/ r jsel i name entl rot)
  (if (null (setq r (getreal "\nValeur ajoutée pour la rotation <200> : ")))
    (setq r 200)
  )
  (and
    (setq jsel (ssget '((0 . "INSERT"))))
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        entl (entget name)
        rot (cdr (assoc 50 entl))
      )
      (entmod (subst (cons 50 (+ rot (angtof (vl-princ-to-string r) (getvar "AUNITS")))) (assoc 50 entl) entl))
    )
  )
  (princ)
)

(defun angconv (ang from to / lst)
  (setq lst
    (list
      (cons 0 180.0)
      (cons 2 200.0)
      (cons 3 pi)
    )
  )
)