(defun c:CopyAtt (/ getAtts selAtt source target)
  (defun getAtts (ent elst atts)
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
            (setq ent (car ent))
            (setq ent (entget ent))
            (or
              (= "ATTRIB" (cdr (assoc 0 ent)))
              (and
                (= "INSERT" (cdr (assoc 0 ent)))
                (setq att (getAtts (cdr (assoc -1 ent))))
                (cond
                  ( (= 1 (length att)) (setq ent (cdar att)))
                  ( (progn
                      (initget 1 (mapcar 'car att))
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
            (setq att (cdr (assoc 2 ent)))
          )
        )
      )
      (princ "\nL'objet sélectionné n'est pas un attribut...")
    )
    (cons att ent)
  )
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
  (princ)
)