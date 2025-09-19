(defun c:CopyAtt (/ getAtts selAtt source target o)
  (defun getAtts (ent / elst atts)
    (while (= "ATTRIB" (cdr (assoc 0 (setq elst (entget (setq ent (entnext ent)))))))
      (setq atts (cons (cons (strcase (cdr (assoc 2 elst))) elst) atts))
    )
  )
  (defun selAtt (msg / ent att)
    (while
      (and
        (setq ent (getpoint msg))
        (not
          (and
            (setq ent (nentselp ent))
            (cond
              ( (= "ATTRIB" (cdr (assoc 0 (entget (car ent)))))
                (setq ent (entget (car ent)))
              )
              ( (and
                  (setq ent (car (last ent)))
                  (= 'ENAME (type ent))
                  (setq ent (entget ent))
                  (= "INSERT" (cdr (assoc 0 ent)))
                  (setq att (getAtts (cdr (assoc -1 ent))))
                  (cond
                    ( (= 1 (length att)) (setq ent (cdar att)))
                    ( (progn
                        (initget 1 (apply 'strcat (mapcar '(lambda (a) (strcat (car a) " ")) att)))
                        (setq ent
                          (getkword
                            (strcat
                              "\nSélectionner l'attribut souhaité ["
                              (substr
                                (apply
                                  'strcat
                                  (mapcar '(lambda (a) (strcat "/" (car a))) att)
                                )
                                2
                              )
                              "] : "
                            )
                          )
                        )
                        (setq ent (cdr (assoc ent att)))
                      )
                    )
                  )
                )
              )
            )
            (setq att (cdr (assoc 2 ent)))
          )
        )
      )
      (princ "\nL'objet sélectionné n'est pas un attribut...")
    )
    (if (and ent att) (cons att ent))
  )
  (setq o (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (if
    (and
      (setq source (selAtt "\nSélectionner l'attribut source : "))
      (setq target (selAtt "\nSélectionner l'attribut cible : "))
    )
    (entmod
      (subst
        (assoc 1 (cdr source))
        (assoc 1 (cdr target))
        (cdr target)
      )
    )
  )
  (setvar "OSMODE" o)
  (princ)
)