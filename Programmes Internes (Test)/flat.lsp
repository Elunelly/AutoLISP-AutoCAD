(defun Flatten-All (jsel / name i n)

	(if jsel
		(progn
			(sssetfirst nil nil)
			(setq i 0
			      n 0
			)
			(while (< i (sslength jsel))
				(setq name (ssname jsel i)
				      i (1+ i)
				)
				(cond
					((not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "Elevation" 0.0))))
						(setq n (1+ n))
					)
					((not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "Location/Z" 0.0))))
						(setq n (1+ n))
					)
					((not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "Position/Z" 0.0))))
						(setq n (1+ n))
					)
					((not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "Center/Z" 0.0))))
						(setq n (1+ n))
					)
					((not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "AlignmentPoint/Z" 0.0))))
						(setq n (1+ n))
					)
					((not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "Normal/Z" 0.0))))
						(setq n (1+ n))
					)
					((and (not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "StartPoint/Z" 0.0))))
					      (not (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name "EndPoint/Z" 0.0))))
					 )
						(setq n (1+ n))
					)
				)
			)
			(prompt (strcat "\nUn total de "
					(itoa i)
					" objets ont été traités. Parmi eux, "
					(itoa (- i n))
					" objets ("
					(rtos (* (/ (- i n) (atof (rtos i))) 100) 2 2)
					" %) ne possèdent pas de propriété en Z ou une erreur est survenue."
				)
			)
		)
		(prompt "\nErreur : Aucun jeu de sélection...")
	)
	(princ)

)