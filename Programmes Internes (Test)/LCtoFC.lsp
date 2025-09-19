(defun c:LCtoFC (/ color# ent-s ent-t layer color62 color420 color430 jsel i)

	(defun color# (color key name)

		(cond
			( (and color
			       (assoc key (entget name))
			  )
				(entmod (subst color (assoc key (entget name)) (entget name)))
			)
			( (and color
			       (not (assoc key (entget name)))
			  )
				(entmod (append (entget name) (list color)))
			)
			( (and (not color)
			       (assoc key (entget name))
			  )
				(entmod (vl-remove (assoc key (entget name)) (entget name)))
			)
		)

	)

	(while (and
		(setq ent-s (car (entsel "\nSélectionner l'objet source : ")))
		(if (not (assoc 62 (entget ent-s)))
			(setq layer (entget (tblobjname "LAYER" (cdr (assoc 8 (entget ent-s)))))
			      color420 (assoc 420 layer)
			      color430 (assoc 430 layer)
			      color62 (assoc 62 layer)
			)
			(setq color420 (assoc 420 (entget ent-s))
			      color430 (assoc 430 (entget ent-s))
			      color62 (assoc 62 (entget ent-s))
			)
		)
	       )
		(if (progn
		    	(princ "\nSélectionner le(s) objet(s) cible(s) : ")
			(setq jsel (ssget))
		    )
			(repeat (setq i (sslength jsel))
				(setq ent-t (ssname jsel (setq i (1- i))))
				(color# color62 62 ent-t)
				(color# color420 420 ent-t)
				(color# color430 430 ent-t)
			)
		)
	)
	(princ)

)