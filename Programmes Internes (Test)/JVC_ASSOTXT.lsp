(defun c:JVC_ASSOTXT (/ *error* foo e fuzz layer cmdecho jsel i name lst tmp err)
  ;; PARAMETRAGE UTILISATEUR !!!
  (setq
    layer "TOU_EU_ALTI_FE"  ;; --> Calque défini pour le filtre de sélection (si plusieurs, séparer par une virgule sans espace!)
    e 0.952993              ;; --> Distance à prendre en compte entre les 2 textes à associer
    fuzz 1E-4               ;; --> Précision sur l'écart entre la distance à considérer pour associer les textes et la distance réelle entre 2 textes
  )
  ;; FIN DU PARAMETRAGE UTILISATEUR !!!
  (defun *error* (msg)
    (setvar "CMDECHO" cmdecho)
    (princ msg)
  )
  (defun foo (lst / name pt tmp ent)
    (if
      (and
        (setq
          name (car lst)
          pt (cdr (assoc 10 (entget name)))
          tmp (cdr lst)
        )
        (not
          (while (not (equal (distance pt (cdr (assoc 10 (entget (car tmp))))) e fuzz))
            (setq tmp (cdr tmp))
          )
        )
        (setq ent (car tmp))
      )
      (progn
        (setq lst (cdr lst))
        (setq lst (vl-remove ent lst))
        (setq tmp (ssadd name))
        (ssadd ent tmp)
        (command "TXT2MTXT" tmp "")
        (setq name (entlast))
        (entmod (subst '(41 . 3) (assoc 41 (entget name)) (entget name)))
        lst
      )
      name
    )
  )
  (sssetfirst)
  (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
  (and
    (setq cmdecho (getvar "CMDECHO"))
    (setvar "CMDECHO" 0)
    (setq err (ssadd))
    (setq jsel (ssget "_X" (list '(0 . "TEXT") (cons 8 layer))))
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        lst (cons name lst)
      )
    )
    (not
      (while lst
        (if (not (listp (setq tmp (foo lst))))
          (progn
            (ssadd tmp err)
            (setq lst (cdr lst))
          )
          (setq lst tmp)
        )
      )
    )
    (sssetfirst nil err)
    (setvar "CMDECHO" cmdecho)
    (princ
      (strcat
        "\nUn total de " (itoa (sslength jsel))
        " blocs ont été traités."
        (if (< 0 (sslength err))
          (strcat "\n /!\\ " (itoa (sslength err)) "/" (itoa (sslength jsel)) " blocs ont échoués dans le traitement...")
          ""
        )
      )
    )
  )
  (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
)