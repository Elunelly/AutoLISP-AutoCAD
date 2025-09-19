(defun try (/ nlst blst lst pt f r x p)
  (and
    (setq nlst (nentsel))
    (= 4 (length nlst))
    (setq blst (last nlst))
    (setq lst
      (mapcar
        '(lambda (e / l)
          (setq l (entget e))
          (list
            (cdr (assoc 10 l))
            (list (cdr (assoc 41 l)) (cdr (assoc 42 l)) (cdr (assoc 43 l)))
            (cdr (assoc 50 l))
          )
         )
        (reverse blst)
      )
    )
    (setq lst lst)
    (setq
      pt '(0.0 0.0 0.0)
      f '(1.0 1.0 1.0)
      r 0.0
    )
    (while lst
      (setq
        x (car lst)
        lst (cdr lst)
        p (car x)
        p (mapcar '* p f)
        pt (polar pt (+ r (angle pt p)) (distance '(0.0 0.0 0.0) p))
        f (cadr x)
        r (+ r (caddr x))
      )
    )
  )
  pt
)