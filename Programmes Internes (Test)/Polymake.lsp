(defun polymake (plist)
  (entmakex
    (append
      (list
        '(0 . "LWPOLYLINE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbPolyline")
        (cons 90 (length plist))
      )
      (mapcar '(lambda (p) (cons 10 p)) plist)
    )
  )
)

(defun foo (PtA PtB Ep Blg rcv d / f l a b e)
  (defun f (d)
    (cond
      ( (= d (/ 1 3.0)) (/ 2 3.0))
      ( (= d 0.5) d)
      ((/ 1 3.0))
    )
  )
  (setq
    l (distance PtA PtB)
    a (angle PtA PtB)
    b (+ a (* pi 0.5))
  )
  (if (<= l Blg)
    (progn
      (setq e (* Ep (cond (d (f d)) (0.5))))
      (polymake
        (list
          (polar PtA b e)
          (polar PtB b e)
        )
      )
    )
    (progn
      (setq e (* Ep (f d)))
      (polymake
        (list
          (polar PtA b e)
          (polar (setq PtA (polar PtA a Blg)) b e)
        )
      )
      (foo
        (polar PtA (+ a pi) rcv)
        PtB
        Ep
        Blg
        rcv
        (f d)
      )
    )
  )
)