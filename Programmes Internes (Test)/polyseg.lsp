; Création le 27/07/2020 v1.0.0
;;;;(defun get-polyseg-list (name / b pt entlist lst)
;;;;
;;;;	(if (= (cdr (assoc 0 (entget name))) "LWPOLYLINE")
;;;;		(progn
;;;;			(setq z (cdr (assoc 38 (entget name)))
;;;;			      entlist (member (assoc 10 (entget name)) (entget name))
;;;;			)
;;;;			(repeat (1- (cdr (assoc 90 (entget name))))
;;;;				(setq b (cdr (assoc 42 entlist))
;;;;				      pt (cons (cdr (assoc 10 entlist)) z)
;;;;				      entlist (member (assoc 10 (cdr entlist)) (cdr entlist))
;;;;				      lst (cons (list pt b (cdr (assoc 10 entlist))) lst)
;;;;				)
;;;;			)
;;;;		)
;;;;	)
;;;;	(reverse lst)
;;;;
;;;;)
;;;;
;;;;(defun get-polyseg-info (pt1 pt2 b / r co ce f a)
;;;;
;;;;	
;;;;
;;;;)




(defun c:CP2DV (/ foo polymake textmake name lst pt ep osm a p1 p2 p3 p4)
  (defun foo (name / bulge->radius entlist blst i d r lst)
    (defun bulge->radius (pt1 b pt2 / c s r)
      (setq
        c (distance pt1 pt2)
        s (* (/ c 2.0) (* -1 b))
        r (/ (+ (expt (/ c 2.0) 2) (expt s 2)) (* s 2.0))
      )
    )
    
    (and
      (setq entlist (entget name))
      (= "LWPOLYLINE" (cdr (assoc 0 entlist)))
      (setq blst (reverse (vl-remove-if-not '(lambda (x) (= (car x) 42)) entlist)))
      (setq blst (cdr (mapcar 'cdr blst)))
      (repeat (setq i (fix (vlax-curve-getEndParam name)))
        (setq
          d (vlax-curve-getDistAtParam name i)
          r
            (if (or (= i (1+ (fix (vlax-curve-getEndParam name)))) (= 0.0 (car blst)))
              0.0
              (bulge->radius (vlax-curve-getPointAtParam name i) (car blst) (vlax-curve-getPointAtParam name (1- i)))
            )
          i (1- i)
          blst (cdr blst)
          lst (cons (list d r) lst)
        )
      )
    )
    lst
  )
  (defun polymake (plist c)
    (entmakex
      (append
        (vl-remove
          nil
          (list
            '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            '(100 . "AcDbPolyline")
            (cons 90 (length plist))
            (if c '(70 . 1))
          )
        )
        (mapcar '(lambda (p) (cons 10 p)) plist)
      )
    )
  )
  (defun textmake (pt str / txt)
    (setq txt
      (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(100 . "AcDbText")
          (cons 10 pt)
          (cons 1 str)
          (cons 40 (getvar 'textsize))
          '(72 . 4)
          '(73 . 2)
        )
      )
    )
    (setq txt (vlax-ename->vla-object txt))
    (vla-move txt (vlax-3D-point (vlax-get txt 'InsertionPoint)) (vlax-3D-point pt))
  )

  (and
    (setq name (car (entsel "\nSélectionner une coupe : ")))
    (setq lst (foo name))
    (setq pt (getpoint "\nChoisir un point d'insertion : "))
    (setq ep (getdist pt "\nDéfinir une épaisseur : "))
    (setq osm (getvar 'osmode))
    (setvar 'osmode 0)
    (setq a (/ pi 2.))
    (setq p0 (polar pt a ep) p1 pt p2 p0)
    (mapcar
      '(lambda (x / d r)
        (setq
          d (car x)
          r (cadr x)
          p3 (polar p0 0.0 d)
          p4 (polar pt 0.0 d)
        )
        (polymake (list p1 p2 p3 p4) T)
        (if (not (zerop r))
          (progn
            (polymake (list p1 p3) nil)
            (polymake (list p2 p4) nil)
            (textmake (mapcar '(lambda (a b) (/ (+ a b) 2.)) p1 p3) (strcat "R: " (rtos r 2 2)))
          )
        )
        (setq
          p1 p4
          p2 p3
        )
       )
      lst
    )
    (setvar 'osmode osm)
  )
  (princ)
)