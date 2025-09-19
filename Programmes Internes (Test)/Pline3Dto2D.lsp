(defun c:toto (/ *error* doc opt del ss alt zlst pl)
 (vl-load-com)
 (defun *error* (msg)
   (and (/= msg "Fonction annulée")
 (princ (strcat "\nErreur: " msg))
   )
   (and ss (vla-delete ss))
   (vla-EndUndoMark doc)
   (princ)
 )
 (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
 (if (ssget '((0 . "POLYLINE") (-4 . "&") (70 . 8)))
   (progn
     (vla-StartUndoMark doc)
     (initget "maXimum miNimum moYenne Zéro")
     (or (setq	opt
	 (getkword
	   "\Altitude de la polyligne plane [maXimum/miNimum/Zéro/moYenne] <moYenne> : "
	 )
  )
  (setq opt "moYenne")
     )
     (initget "Oui Non")
     (or (setq	del
	 (getkword "\nSupprimer la polyligne 3d source ? [Oui/Non] <Non> : ")
  )
  (setq del "Non")
     )
     (vlax-for	p (setq ss (vla-get-ActiveSelectionSet doc))
(setq pts (3d-coord->pt-lst (vlax-get p 'Coordinates))
      zlst (mapcar 'caddr pts)
      alt (cond         
	    ((= opt "maXimum")
	     (apply 'max zlst)
	    )
	    ((= opt "miNimum")
	     (apply 'min zlst)
	    )
 	    ((= opt "Zéro")
	     (apply '- '(1 1))
	    )             
	    (T (/ (apply '+ zlst) (float (length pts))))
	  )
      pl  (vlax-invoke
	    (vla-get-ModelSpace doc)
	    'addLightWeightPolyline
	    (apply
	      'append
	      (mapcar
		'(lambda (p)
		   (setq p (trans p 0 (trans '(0 0 1) 1 0 T)))
		   (list (car p) (cadr p))
		 )
		pts
	      )
	    )
	  )
)
(vla-put-elevation pl alt)
(vla-put-Closed pl (vla-get-Closed p))
;;; Ajouter un point-virgule ; devant les lignes ci-dessous si l'on ne désire pas conserver la propriété de l'objet initial
(vla-put-TrueColor pl (vla-get-TrueColor p))         ; Conserve la propriété de couleur de l'objet initial
(vla-put-Linetype pl (vla-get-Linetype p))           ; Conserve la propriété de type de ligne de l'objet initial
(vla-put-LinetypeScale pl (vla-get-LinetypeScale p)) ; Conserve la propriété d'échelle de type de ligne de l'objet initial
(vla-put-Layer pl (vla-get-Layer p))                 ; Conserve la propriété de calque de l'objet initial
(vla-put-EntityTransparency pl (vla-get-EntityTransparency p)) ; Conserve la propriété d'échelle de type de ligne de l'objet initial
;;; Ajouter un point-virgule ; devant les lignes ci-dessus si l'on ne désire pas conserver la propriété de l'objet initial
(if (= del "Oui")
  (vla-delete p)
)
     )
     (vla-delete ss)
     (vla-EndUndoMark doc)
   )
 )
 (princ)
)

;;; 3d-coord->pt-lst Convertit une liste de coordonnées 3D en liste de points
;;; (3d-coord->pt-lst '(1.0 2.0 3.0 4.0 5.0 6.0)) -> ((1.0 2.0 3.0) (4.0 5.0 6.0))

(defun 3d-coord->pt-lst	(lst)
 (if lst
   (cons (list (car lst) (cadr lst) (caddr lst))
  (3d-coord->pt-lst (cdddr lst))
   )
 )
)