
(defun c:POLYEXTRACT (/ jsel i name pt-list n get-pt-list pt)

	(defun Get-pt-list (name / ent-list pt-list)

		(if (= (type name) 'ENAME)
			(progn
				(setq ent-list (entget name))
				(while (setq ent-list (member (assoc 10 ent-list) ent-list))
					(setq pt-list (cons (cdr (assoc 10 ent-list)) pt-list)
					      ent-list (cdr ent-list)
					)
				)
			)
			(prompt "\nL'argument spécifié n'est pas un nom d'entité.\n")
		)
		(if pt-list
			(cond
				((= (cdr (assoc 0 (entget name))) "HATCH")
					(setq pt-list (cdr (reverse (cdr pt-list))))
				)
				((= (cdr (assoc 0 (entget name))) "ARC")
					(setq pt-list (append pt-list
							      (list
								(polar (car pt-list) (cdr (assoc 50 (entget name))) (cdr (assoc 40 (entget name))))
								(polar (car pt-list) (cdr (assoc 51 (entget name))) (cdr (assoc 40 (entget name))))
							      )
						      )
					)
				)
				((= (cdr (assoc 0 (entget name))) "LINE")
					(setq pt-list (reverse (cons (cdr (assoc 11 (entget name))) pt-list)))
				)
				((= (cdr (assoc 0 (entget name))) "POLYLINE")
					(setq ent-list (entget name))
					(while (/= (cdr (assoc 0 ent-list)) "SEQEND")
						(setq ent-list (entget (setq name (entnext name))))
						(if (assoc 10 ent-list)
							(setq pt-list (cons (cdr (assoc 10 ent-list)) pt-list))
						)
					)
					(setq pt-list (reverse (cdr (reverse pt-list))))
				)
				(t
					(setq pt-list (reverse pt-list))
				)
			)
			nil
		)

	)

	(if (setq jsel (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
		(progn
			(if (not (setq file-name (getfiled "Création du fichier d'extraction :" (getvar "DWGPREFIX") "csv" 1)))
				(exit)
				(setq file (open file-name "w"))
			)
			(write-line
				(strcat "Point,"
					"X,"
					"Y,"
					"Z,"
				)
				file
			)
			(repeat (setq i (sslength jsel))
				(setq name (ssname jsel (setq i (1- i)))
				      pt-list (get-pt-list name)
				      n 0
				)
				(write-line (strcat "Objet n°" (itoa (- (sslength jsel) i))) file)
				(foreach pt pt-list
					(write-line (strcat (itoa (setq n (1+ n)))
							    ","
							    (rtos (car pt) 2 2)
							    ","
							    (rtos (cadr pt) 2 2)
							    ","
							    (cond
							    	((= (length pt) 2)
									(rtos (getpropertyvalue name "Elevation") 2 2)
								)
								((= (length pt) 3)
									(rtos (caddr pt) 2 2)
								)
							    )
						    )
						    file
					)
				)
			)
		 	(close file)
			(prompt (strcat "\nUn total de "
					(itoa (sslength jsel))
					" polylignes ont été traitées."
					"\nLe fichier d'extraction (.csv) se trouve à l'emplacement \""
					file-name
					"\"."
				)
			)
		)
		(prompt "\nAbsence de polyligne(s) dans la sélection / abandon de la commande...")
	)
	(princ)
  
)




