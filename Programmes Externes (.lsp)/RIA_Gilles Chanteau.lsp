(defun c:RIA (/ *error* doc space radius rad startPt circle pt0 pt1 pts pline break)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (and msg
	 (/= msg "Fonction annulée")
	 (princ (strcat "\nErreur: " msg))
    )
    (vla-EndUndoMark doc)
    (princ)
  )

  (vla-StartUndoMark doc)
  (setq	space  (vla-get-Block (vla-get-activeLayout doc))
	radius (cond
		 ((getenv "RiaMaximumRadius"))
		 ((setenv "RiaMaximumRadius" "30.00"))
	       )
  )
  (if (setq rad (getdist (strcat "\nEntrez le rayon maximum <" radius ">: ")))
    (setenv "RiaMaximumRadius" (rtos rad 2 2))
    (setq rad (atof radius))
  )
  (if (setq startPt (getpoint "\nSpécifier le point de départ: "))
    (progn
      (setq circle (vla-AddCircle space (vlax-3d-point (trans startPt 1 0)) rad))
      (setq pt0	startPt
      )
      (while (and
	       (not break)
	       (setq pt1 (getpoint pt0 "\nSpécifier le point suivant (ou valider): "))
	     )
	(vla-Delete circle)
	(if pline
	  (if (< (+ (vla-get-Length pline) (distance (vlax-curve-getEndPoint pline) pt1)) rad)
	    ((lambda (p)
	       (vlax-put
		 pline
		 'Coordinates
		 (append (vlax-get pline 'Coordinates) (list (car p) (cadr p)))
	       )
	       (setq circle
		      (vla-AddCircle
			space
			(vlax-3d-point p)
			(- rad (vla-get-Length pline))
		      )
	       )
	     )
	      (trans pt1 1 0)
	    )
	    ((lambda (p)
	       (vlax-put
		 pline
		 'Coordinates
		 (append (vlax-get pline 'Coordinates) (list (car p) (cadr p)))
	       )
	       (setq break T)
	     )
	      (trans (polar pt0 (angle pt0 pt1) (- rad (vla-get-Length pline))) 1 0)
	    )
	  )
	  (if (< (distance pt0 pt1) rad)
	    ((lambda (p0 p1)
	       (setq pline
		      (vlax-invoke
			space
			'AddLightWeightPolyline
			(list (car p0) (cadr p0) (car p1) (cadr p1))
		      )
	       )
	       (setq circle
		      (vla-AddCircle
			space
			(vlax-3d-point p1)
			(- rad (distance pt0 pt1))
		      )
	       )
	     )
	      (trans pt0 1 0)
	      (trans pt1 1 0)
	    )
	    ((lambda (p0 p1)
	       (setq pline
		      (vlax-invoke
			space
			'AddLightWeightPolyline
			(list (car p0) (cadr p0) (car p1) (cadr p1))
		      )
	       )
	       (setq break T)
	     )
	      (trans pt0 1 0)
	      (trans (polar pt0 (angle pt0 pt1) (- rad (vla-get-Length pline))) 1 0)
	    )
	  )
	)
	(setq pt0 pt1)
      )
    )
  )
  (*error* nil)
)