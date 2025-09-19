(defun c:Inter2Layers (/ LM:intersectionbetweensets LM:intersections Object2Layer layer1 layer2 ss ss1 ss2 lst tmp)
  (defun LM:intersectionsbetweensets ( ss1 ss2 / id1 id2 ob1 ob2 rtn )
    (repeat (setq id1 (sslength ss1))
      (setq ob1 (vlax-ename->vla-object (ssname ss1 (setq id1 (1- id1)))))
      (repeat (setq id2 (sslength ss2))
        (setq
          ob2 (vlax-ename->vla-object (ssname ss2 (setq id2 (1- id2))))
          rtn (cons (LM:intersections ob1 ob2 acextendnone) rtn)
        )
      )
    )
    (apply 'append (reverse rtn))
  )

  (defun LM:intersections ( ob1 ob2 mod / lst rtn )
    (if
      (and
        (vlax-method-applicable-p ob1 'intersectwith)
        (vlax-method-applicable-p ob2 'intersectwith)
        (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
      )
      (repeat (/ (length lst) 3)
        (setq
          rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
          lst (cdddr lst)
        )
      )
    )
    (reverse rtn)
  )
  
  (defun Object2Layer (msg / ent)
    (while (not (setq ent (entsel msg)))
    )
  )

  (setq param (list "Calque1" "Calque2")) ; Remplacer le nom des calques par défaut ici !
  (setq mode 1) ; Définir la valeur de mode sur 0 (= entrer le nom des calques à la main) ou 1 (= sélectionner une entité pour récupérer son calque)
  (and
    (cond
      ( (= 0 mode)
        (and
          (or
            (not (= "" (setq layer1 (getstring T (strcat "\nSpécifier le nom du 1er calque <" (car param) "> : ")))))
            (setq layer1 (car param))
          )
          (or
            (not (= "" (setq layer2 (getstring T (strcat "\nSpécifier le nom du 2nd calque <" (cadr param) "> : ")))))
            (setq layer2 (cadr param))
          )
        )
      )
      ( (= 1 mode)
        (and
          (or
            (and
              (setq layer1 (entsel (strcat "\nSélectionner une entité du 1er calque <" (car param) "> : ")))
              (setq layer1 (cdr (assoc 8 (entget (car layer1)))))
            )
            (setq layer1 (car param))
          )
          (or
            (and
              (setq layer2 (entsel (strcat "\nSélectionner une entité du 2nd calque <" (cadr param) "> : ")))
              (setq layer2 (cdr (assoc 8 (entget (car layer2)))))
            )
            (setq layer2 (cadr param))
          )
        )
      )
    )
    (null (sssetfirst))
    (setq ss (ssadd))
    (setq ss1 (ssget "_X" (list (cons 8 layer1) (cons 410 (getvar "CTAB")))))
    (setq ss2 (ssget "_X" (list (cons 8 layer2) (cons 410 (getvar "CTAB")))))
    (setq lst (LM:IntersectionsBetweenSets ss1 ss2))
    (setq tmp (vl-remove nil (mapcar '(lambda (p) (entmakex (list '(0 . "POINT") (cons 10 p) '(62 . 1)))) lst)))
    (mapcar '(lambda (e) (ssadd e ss)) tmp)
    (sssetfirst nil ss)
    (princ
      (strcat
        "\nUn total de " (itoa (length tmp)) " / " (itoa (length lst))
        " points d'intersections ont été créé avec succès, avec les calques :"
        "\n " layer1 " : " (itoa (sslength ss1)) " objets"
        "\n " layer2 " : " (itoa (sslength ss2)) " objets"
      )
    )
  )
  (princ)
)