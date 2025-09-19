(defun c:TESTT (/ cnpoly jsel i name)
  (defun cnpoly (ent / i pt-list lst r)
    (setq i 0 pt-list (get-pt-list ent))
    (mapcar
      '(lambda (pt1 pt2)
        (if
          (not
            (and
              (equal (cdr pt1) (cdr pt2) 0.01)
              (< (abs (setq r (- (car pt1) (car pt2)))) 0.18)
            )
          )
          (entmod
            (vl-remove-if
              '(lambda (x)
                (and
                  (= 10 (car x))
                  (equal (distance (cdr x) (if (minusp r) pt2 pt1)) 0.001)
                )
              )
            )
            (entget ent)
          )
        )
      )
      pt-list
      (cdr pt-list)
    )
  )
  (setq jsel (ssget '((0 . "LWPOLYLINE"))))
  (repeat (setq i (sslength jsel))
    (setq name (ssname jsel (setq i (1- i))))
    (cnpoly name)
  )
  (princ)
)