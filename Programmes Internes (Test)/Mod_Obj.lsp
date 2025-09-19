(defun c:MOD_OBJ (/ *error* cmdecho lst2str pplst filter choice ppname ptype ppvalue ent)
  (defun lst2str (lst sep)
    (vl-string-left-trim sep (apply 'strcat (mapcar '(lambda (l) (strcat sep (vl-princ-to-string l))) lst)))
  )
  (defun *error* (msg)
    (if (/= (getvar "CMDECHO") cmdecho)
      (setvar "CMDECHO" cmdecho)
    )
    (princ msg)
  )
  (setq
    cmdecho (getvar "CMDECHO")
    pplst
      (list
        (cons "ecHelle" (list "LineTypeScale" (list 7) (list 'getreal)))
        (cons "ePaisseur" (list 'LineWeight (list 5 "DuCalque 0 5 9 13 15 18 20 25 30 35 40 50 53 60 70 80 90 100 106 120 140 158 200 211") (list 'getreal)))
        (cons "Calque" (list 0 (list 1 (getvar 'CLAYER)) (list 'getstring T)))
        ; ...
      )
    filter
      (cond
        (
          (cons
            "\nSélectionner une ligne"
            "LINE,AcDbLine"
          )
        )
        (
          (cons
            (getstring T "\nDéfinir un message de sélection : ")
            (getstring T "\nDéfinir un pattern pour le filtre de sélection : ")
          )
        )
      )
  )
  (while
    (progn
      (initget (lst2str (mapcar 'car pplst) " "))
      (setq choice
        (cond
          ((getkword
            (strcat
              "\nVeuillez choisir la propriété à modifier ["
              (lst2str (mapcar 'car pplst) "/")
              "] <eXit> : "
            )
           )
          )
        )
      )
    )
    (setq ppname (cadr (assoc choice pplst))
          ptype (type ppname)
          n 0
    )
    (apply 'initget (caddr (assoc choice pplst)))
    (setq ppvalue
      (apply
        (car (cadddr (assoc choice pplst)))
        (vl-remove nil
          (list
            (cadr (cadddr (assoc choice pplst)))
            (strcat
              "\nSpécifiez la nouvelle valeur de la propriété "
              (strcase choice)
              (if (= (type (last (caddr (assoc choice pplst)))) 'STR)
                (strcat " [" (vl-string-translate " " "/" (last (caddr (assoc choice pplst)))) "]")
                ""
              )
              " : "
            )
          )
        )
      )
    )
    (setvar "CMDECHO" 0)
    (command "_UNDO" "_Mark")
    (while (/= (progn (initget "Quitter annUler") (setq ent (entsel (strcat (car filter) " [annUler/Quitter] : ")))) "Quitter")
      (cond
        ((and (= ent "annUler") (> n 0))
          (command "_UNDO" "_Back")
          (setq n (1+ n))
        )
        ((or (null ent) (= ent "annUler")))
        ((SetAnyProperty (car ent) (cdr filter) (cond ((= ptype 'INT) 0) ((= ptype 'STR) 1) ((= ptype 'SYM) 2)) ppname ppvalue)
          (princ
            (strcat
              "\nLa propriété \""
              (vl-prin1-to-string ppname)
              "\" possède désormais la valeur \""
              (vl-prin1-to-string ppvalue)
              "\" pour l'objet \""
              (cdr (assoc 0 (entget (car ent))))
              "\"."
            )
          )
        )
        (T
          (princ
            (strcat
              "\nEchec lors de la modification de la propriété \""
              (vl-prin1-to-string ppname)
              "\" pour l'objet \""
              (cdr (assoc 0 (entget (car ent))))
              "\"."
            )
          )
        )
      )
      (command "_UNDO" "_Mark")
    )
  )
  (setvar "CMDECHO" cmdecho)
  (princ)
)