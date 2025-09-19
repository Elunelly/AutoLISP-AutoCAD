(defun c:ATTROT (/ name)

   (while (and (setq name (car (nentsel "\nVeuillez cliquer sur un attribut : ")))
               (= (cdr (assoc 0 (entget name))) "ATTRIB")
          )
       (entmod (subst (cons 50 (+ (cdr (assoc 50 (entget name))) (/ pi 2.0))) (assoc 50 (entget name)) (entget name)))
   )
   (if name
       (prompt "\nErreur : L'objet sélectionné n'est pas un attribut")
       (prompt "\nFin de la commande")
   )
   (princ)

)