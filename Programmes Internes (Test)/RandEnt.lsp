(defun randent (n inf sup / f x)
  (defun f (ent i s / l)
    (cond
      ( (= ent "LWPOLYLINE")
        (append
          (list
            (cons 90 (setq n (LM:randrange 2 23)))
          )
          (repeat n
            (setq l (cons (cons 10 (list (LM:randrange i s) (LM:randrange i s))) l))
          )
        )
      )
      ( (= ent "LINE")
        (list
          (cons 10 (list (LM:randrange i s) (LM:randrange i s)))
          (cons 11 (list (LM:randrange i s) (LM:randrange i s)))
        )
      )
      ( (= ent "CIRCLE")
        (list
          (cons 10 (list (LM:randrange i s) (LM:randrange i s)))
          (cons 40 (LM:randrange (/ i 2.0) (/ s 4.0)))
        )
      )
    )
  )
  (setq objlist
    (list
      (cons "AcDbCircle"   "CIRCLE")
      (cons "AcDbLine"     "LINE")
      (cons "AcDbPolyline" "LWPOLYLINE")
    )
  )
  (repeat n
    (setq x (nth (LM:randrange 0 (1- (length objlist))) objlist))
    (entmake
      (append
        (list
          (cons 0 (cdr x))
          '(100 . "AcDbEntity")
          (cons 100 (car x))
          (cons 8 (strcat "Layer " (itoa (LM:randrange 1 10))))
        )
        (f (cdr x) inf sup)
      )
    )
  )
  (princ)
)