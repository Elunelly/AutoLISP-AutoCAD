(defun C:Mul_Offset ( / obj ent vla-ent pt_sel param deriv alpha dis_offset where_pt v1 v2 det_or e_last)
(while (not (setq obj (entsel "\nSélectionner l'objet à décaler: "))))
(setq ent (car obj) vla-ent (vlax-ename->vla-object ent))
(cond
	((member
		(vlax-get-property vla-ent 'ObjectName)
		'("AcDbLine" "AcDbXline" "AcDbPolyline" "AcDb2dPolyline" "AcDbSpline" "AcDbEllipse" "AcDbArc" "AcDbCircle")
	 )
		(setq
			pt_sel (vlax-curve-getClosestPointTo vla-ent (trans (cadr obj) 1 0))
			param (vlax-curve-getparamatpoint vla-ent pt_sel)
			deriv (vlax-curve-getfirstderiv vla-ent param)
			alpha (atan (cadr deriv) (car deriv))
		)
		(redraw ent 3)
		(initget "Par _Through")
		(setvar "OFFSETDIST"
			(if (not (setq dis_offset (getdist (strcat "\nSpécifiez la distance de décalage ou [Par] <" (if (< (getvar "OFFSETDIST") 0) "Par" (rtos (getvar "OFFSETDIST"))) ">: "))))
				(progn (if (< (getvar "OFFSETDIST") 0) (setq dis_offset "Through")) (getvar "OFFSETDIST"))
				(if (eq dis_offset "Through") -1 dis_offset)
			)
		)
		(if (< (getvar "OFFSETDIST") 0)
			(princ "\nAttribuez une valeur à \"Par le point\": ")
			(princ "\nSpécifiez un point sur le côté à décaler: ")
		)
		(initget 9)
		(setq where_pt (getpoint))
		(if (< (getvar "OFFSETDIST") 0)
			(setvar "OFFSETDIST"
				(distance
					(vlax-curve-getClosestPointToProjection vla-ent
						(trans where_pt 1 0)
						(mapcar '- (trans (getvar "VIEWDIR") 1 0) (trans '(0 0 0) 1 0))
						T
					)
					(list (car (trans where_pt 1 0)) (cadr (trans where_pt 1 0)))
				)
			)
		)
		(redraw ent 4)
		(setq alpha (atan (cadr deriv) (car deriv)))
		(setq
			v1 (mapcar '- (polar pt_sel alpha 1.0) pt_sel)
			v2 (mapcar '- (trans where_pt 1 0) pt_sel)
		)
		(setq det_or (apply '(lambda (x1 y1 z1 x2 y2 z2) (- (* x1 y2) (* y1 x2))) (append v1 v2)))
		(cond
			((> det_or 0.0) (setvar "OFFSETDIST" (- (abs (getvar "OFFSETDIST")))))
			((< det_or 0.0) (setvar "OFFSETDIST" (abs (getvar "OFFSETDIST"))))
		)
		(if (member (vlax-get-property vla-ent 'ObjectName) '("AcDbLine" "AcDbXline")) (setvar "OFFSETDIST" (abs (getvar "OFFSETDIST"))))
		(setq e_last ent)
		(initget 7)
		(repeat (getint "\nNombre de répétition : ")
			(if
				(and e_last (vl-catch-all-error-p
					(vl-catch-all-apply
						'vla-Offset
						(list (vlax-ename->vla-object e_last)
							(getvar "OFFSETDIST")
						)
					)
				))
				(progn (setq e_last nil) (princ "\nL'objet ne peut pas être décalé."))
				(setq e_last (entlast))
			)
		)
	)
	(T (princ "\nL'objet ne peut pas être décalé."))
)
(princ)
)