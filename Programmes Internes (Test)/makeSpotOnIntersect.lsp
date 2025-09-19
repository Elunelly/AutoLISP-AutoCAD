(defun makeSpotOnIntersect (poly line e / LM:intersections lwpoly-AddVertex 2D-Point foo obj pti p pt1 pt2 b)
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
  (defun lwpoly-AddVertex (obj pt i / e n s)
    (setq
      e (vlax-vla-object->ename obj)
      n (cdr (assoc 90 (entget e)))
      pt (2D-Point pt)
      s (vlax-make-safearray vlax-vbDouble '(0 . 1))
      i (cond ((null i) n) ((minusp i) 0) ((< i n) (fix i)) (n))
    )
    (vlax-safearray-fill s pt)
    (vla-AddVertex obj i s)
    (vla-Update obj)
    (if (= (1+ n) (cdr (assoc 90 (entget e)))) (1+ n))
  )
  (defun 2D-Point (pt / lg)
    (cond
      ( (or (not (listp pt)) (not (setq lg (vl-list-length pt)))) nil)
      ( (= 3 lg) (reverse (cdr (reverse pt))))
      ( (= 2 lg) pt)
    )
  )
  (defun foo (pt / i d pt1 pt2)
    (setq
      i (vlax-curve-getParamAtPoint poly pt)
      d (vlax-curve-getDistAtPoint poly pt)
    )
    (cond
      ( (= i (fix i))
        ; Ajout de 2 point + suppression d'un point
      )
      ( (setq i (1+ i))
        (setq
          pt1 (vlax-curve-getPointAtDist poly (- d (/ e 2.)))
          pt2 (vlax-curve-getPointAtDist poly (+ d (/ e 2.)))
        )
        (lwpoly-AddVertex obj pt2 i)
        (lwpoly-AddVertex obj pt1 i)
        (vla-setBulge obj i -0.5)
        i
      )
    )
    
  )
  
  (setq
    obj (vlax-ename->vla-object poly)
    pti (LM:intersections obj (vlax-ename->vla-object line) acextendotherentity)
  )
  (mapcar 'foo pti)
)


(defun foo (/ prec jsel i name)
  (initget 6)
  (setq prec (cond ((getint (strcat "\nNombre de décimal à considérer pour l'arrondi <" (getvar "LUPREC") "> : "))) ((getvar "LUPREC"))))
  (and
    (setq jsel (ssget '((0 . "LINE,POLYLINE"))))
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        typ (cdr (assoc 0 (entget name)))
      )
      (cond
        ( (= "LINE" typ)
          (setq pts (list (cdr (assoc 10 (entget name))) (cdr (assoc 11 (entget name)))))
        )
      )
    )
  )
)