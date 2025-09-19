    ;; Returns the convex hull points list (Graham's scan algorithm)
    (defun convhull (pts / clockwise getPivot getConvHull)
     
      ;; Evaluates if p1 p2 p3 are clockwise
      (defun clockwise (p1 p2 p3)
        (< (- (* (- (car p2) (car p1)) (- (cadr p3) (cadr p1)))
              (* (- (cadr p2) (cadr p1)) (- (car p3) (car p1)))
           )
           1e-8
        )
      )
     
      ;; Gets the pivot
      (defun getPivot (p l)
        (if l
          (getPivot
            (if (or (and (= (cadar l) (cadr p)) (< (caar l) (car p)))
                    (< (cadar l) (cadr p))
                )
              (car l)
              p
            )
            (cdr l)
          )
          p
        )
      )
     
      ;; Constructs the points list
      (defun getConvHull (lst acc)
        (if lst
          (if (and (cdr acc) (clockwise (cadr acc) (car acc) (car lst)))
            (getConvHull lst (cdr acc))
            (getConvHull (cdr lst) (cons (car lst) acc))
          )
          acc
        )
      )
     
      ((lambda (p0)
         (reverse
           (getConvHull
             (vl-sort pts
                      '(lambda (p1 p2 / d1 d2 c1 c2)
                         (setq d1 (distance p0 p1)
                               d2 (distance p0 p2)
                         )
                         (if (or (= 0 d1)
                                 (= 0 d2)
                                 (equal (setq c1 (/ (- (car p0) (car p1)) d1))
                                        (setq c2 (/ (- (car p0) (car p2)) d2))
                                        1e-9
                                 )
                             )
                           (< d1 d2)
                           (< c1 c2)
                         )
                       )
             )
             nil
           )
         )
       )
        (getPivot (car pts) (cdr pts))
      )
    )

(defun c:ch (/ ss n name pt-list lst)
  (if (setq lst '() ss (ssget))
    (progn
      (repeat (setq n (sslength ss))
        (setq name (ssname jsel (setq n (1- n))))
        (setq pt-list (get-pt-list name))
        (setq lst (append lst pt-list))
      )
      (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (entmake
        (vl-list*
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline")
          (cons 90 (length lst))
          '(70 . 1)
          (mapcar '(lambda (p) (list 10 (car p) (cadr p))) (convhull lst))
        )
      )
      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    )
  )
  (princ)
)