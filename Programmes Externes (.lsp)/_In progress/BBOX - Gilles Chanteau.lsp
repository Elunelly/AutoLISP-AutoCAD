;; Doug C. Broad, Jr.
;; can be used with vla-transformby to
;; transform objects from the UCS to the WCS
(defun UCS2WCSMatrix ()
  (vlax-tmatrix
    (append
      (mapcar
	'(lambda (vector origin)
	   (append (trans vector 1 0 t) (list origin))
	 )
	(list '(1 0 0) '(0 1 0) '(0 0 1))
	(trans '(0 0 0) 0 1)
      )
      (list '(0 0 0 1))
    )
  )
)
;; transform objects from the WCS to the UCS
(defun WCS2UCSMatrix ()
  (vlax-tmatrix
    (append
      (mapcar
	'(lambda (vector origin)
	   (append (trans vector 0 1 t) (list origin))
	 )
	(list '(1 0 0) '(0 1 0) '(0 0 1))
	(trans '(0 0 0) 1 0)
      )
      (list '(0 0 0 1))
    )
  )
)


;;; BBOX (gile)
;;; Crée une entité (polyligne ou boite) figurant la "bounding box" de l'objet sélectionné.

(defun c:bbox (/ bbox_err AcDoc	Space obj bb minpoint maxpoint pt1 pt2 lst poly
	       box cen norm)
  (vl-load-com)

  (defun bbox_err (msg)
    (if	(or
	  (= msg "Fonction annulée")
	  (= msg "quitter / sortir abandon")
	)
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (vla-endundomark
      (vla-get-activedocument (vlax-get-acad-object))
    )
    (setq *error* m:err
	  m:err	nil
    )
    (princ)
  )

  (setq	AcDoc	(vla-get-activedocument (vlax-get-acad-object))
	Space	(if (= (getvar "CVPORT") 1)
		  (vla-get-PaperSpace AcDoc)
		  (vla-get-ModelSpace AcDoc)
		)
	m:err	*error*
	*error*	bbox_err
  )
  (vla-startUndoMark AcDoc)
  (while (not (setq obj (car (entsel)))))
  (setq obj (vlax-ename->vla-object obj))
  (vla-TransformBy obj (UCS2WCSMatrix))
  (setq	bb (vl-catch-all-apply
	     'vla-getboundingbox
	     (list obj
		   'minpoint
		   'maxpoint
	     )
	   )
  )
  (if (vl-catch-all-error-p bb)
    (progn
      (princ
	(strcat "; erreur: " (vl-catch-all-error-message bb))
      )
      (vla-TransformBy obj (WCS2UCSMatrix))
    )
    (progn
      (setq pt1	(vlax-safearray->list minpoint)
	    pt2	(vlax-safearray->list maxpoint)
      )
      (if (or (equal (car pt1) (car pt2) 1e-007)
	      (equal (cadr pt1) (cadr pt2) 1e-007)
	      (equal (caddr pt1) (caddr pt2) 1e-007)
	  )
	(progn
	  (cond
	    ((equal (car pt1) (car pt2) 1e-007)
	     (setq lst (list pt1
			     (list (car pt1) (cadr pt1) (caddr pt2))
			     pt2
			     (list (car pt1) (cadr pt2) (caddr pt1))
		       )
	     )
	    )
	    ((equal (cadr pt1) (cadr pt2) 1e-007)
	     (setq lst (list pt1
			     (list (car pt1) (cadr pt1) (caddr pt2))
			     pt2
			     (list (car pt2) (cadr pt1) (caddr pt1))
		       )
	     )
	    )
	    ((equal (caddr pt1) (caddr pt2) 1e-007)
	     (setq lst (list pt1
			     (list (car pt1) (cadr pt2) (caddr pt1))
			     pt2
			     (list (car pt2) (cadr pt1) (caddr pt1))
		       )
	     )
	    )
	  )
	  (setq	box
		 (vlax-invoke Space 'add3dPoly (apply 'append lst))
	  )
	  (vla-put-closed box :vlax-true)
	)
	(progn
	  (setq	cen (mapcar '(lambda (x y) (/ (+ x y) 2)) pt1 pt2)
		pt2 (mapcar '- pt2 pt1)
		box (vla-addBox
		      Space
		      (vlax-3d-point cen)
		      (car pt2)
		      (cadr pt2)
		      (caddr pt2)
		    )
	  )
	)
      )
      (if (= (vla-get-ObjectName obj) "AcDbText")
	(progn
	  (setq	norm (vlax-get obj 'Normal)
	  )
	  (vla-Move
	    box
	    (vlax-3d-point (trans '(0 0 0) norm 0))
	    (vlax-3d-point
	      (trans
		(list
		  0
		  0
		  (caddr
		    (trans (vlax-get obj 'InsertionPoint)
			   0
			   norm
		    )
		  )
		)
		norm
		0
	      )
	    )
	  )
	)
      )
      (mapcar '(lambda (x) (vla-TransformBy x (WCS2UCSMatrix)))
	      (list obj box)
      )
    )
  )
  (vla-endUndoMark AcDoc)
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)






















































;; gc:TMatrixFromTo
;; Returns the 4X4 transformation matrix from a coordinate system to an other one
;;
;; Arguments
;; from to: same arguments as for the 'trans' function

(defun gc:TMatrixFromTo (from to)
  (append
    (mapcar
      (function
	(lambda	(v o)
	  (append (trans v from to T) (list o))
	)
      )
      (list '(1. 0. 0.) '(0. 1. 0.) '(0. 0. 1.))
      (trans '(0. 0. 0.) to from)
    )
    (list '(0. 0. 0. 1.))
  )
)

;; gc:UcsBoundingBox
;; Returns the UCS coordinates of the object bounding box about current UCS
;;
;; Arguments
;; obj: an entity (ENAME or VLA-OBJCET)
;; _OutputMinPtSym: a quoted symbol (output)
;; _OutputMaxPtSym: a quoted symbol (output)

(defun gc:UcsBoundingBox (obj _OutputMinPtSym _OutputMaxPtSym)
  (vl-load-com)
  (and (= (type obj) 'ENAME)
       (setq obj (vlax-ename->vla-object obj))
  )
  (vla-TransformBy obj (vlax-tmatrix (gc:TMatrixFromTo 1 0)))
  (vla-GetBoundingBox obj _OutputMinPtSym _OutputMaxPtSym)
  (vla-TransformBy obj (vlax-tmatrix (gc:TMatrixFromTo 0 1)))
  (set _OutputMinPtSym (vlax-safearray->list (eval _OutputMinPtSym)))
  (set _OutputMaxPtSym (vlax-safearray->list (eval _OutputMaxPtSym)))
)

;; gc:SelSetUcsBBox
;; Returns the UCS coordinates of the object bounding box about current UCS
;;
;; Arguments
;; ss: a selection set
;; _OutputMinPtSym: a quoted symbol (output)
;; _OutputMaxPtSym: a quoted symbol (output)

(defun gc:SelSetUcsBBox	(ss _OutputMinPtSym _OutputMaxPtSym / n l1 l2)
  (repeat (setq n (sslength ss))
    (gc:UcsBoundingBox (ssname ss (setq n (1- n))) _OutputMinPtSym _OutputMaxPtSym)
    (setq l1 (cons (eval _OutputMinPtSym) l1)
	  l2 (cons (eval _OutputMaxPtSym) l2)
    )
  )
  (set _OutputMinPtSym (apply 'mapcar (cons 'min l1)))
  (set _OutputMaxPtSym (apply 'mapcar (cons 'max l2)))
)