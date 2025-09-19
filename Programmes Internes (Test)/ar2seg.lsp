(defun arc2segs (o r a b m / rounddiv l i n)
  ;; m
  ;; (0 . angle)
  ;; (1 . segment_length)
  ;; (2 . number_of_division)
  (defun rounddiv (n)
    (cond ((zerop (- n (fix n)))) ((1+ (fix n))))
  )
  (defun E2B (e r)
    ()
  )
)

;; Tangent  -  Lee Mac
;; Args: x - real

(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;; ArcSine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun asin ( x )
    (if (<= -1.0 x 1.0)
        (atan x (sqrt (- 1.0 (* x x))))
    )
)

;; ArcCosine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun acos ( x )
    (if (<= -1.0 x 1.0)
        (atan (sqrt (- 1.0 (* x x))) x)
    )
)

;; Hyperbolic Sine  -  Lee Mac
;; Args: x - real

(defun sinh ( x )
    (/ (- (exp x) (exp (- x))) 2.0)
)

;; Hyperbolic Cosine  -  Lee Mac
;; Args: x - real

(defun cosh ( x )
    (/ (+ (exp x) (exp (- x))) 2.0)
)

;; Hyperbolic Tangent  -  Lee Mac
;; Args: x - real

(defun tanh ( x )
    (/ (sinh x) (cosh x))
)

;; Area Hyperbolic Sine  -  Lee Mac
;; Args: x - real

(defun asinh ( x )
    (log (+ x (sqrt (1+ (* x x)))))
)

;; Area Hyperbolic Cosine  -  Lee Mac
;; Args: 1 <= x

(defun acosh ( x )
    (if (<= 1.0 x)
        (log (+ x (sqrt (1- (* x x)))))
    )
)

;; Area Hyperbolic Tangent  -  Lee Mac
;; Args: -1 < x < 1

(defun atanh ( x )
    (if (< (abs x) 1.0)
        (/ (log (/ (1+ x) (- 1.0 x))) 2.0)
    )
)






(setq
  nlst (nentsel)
  name (car nlst)
)
(if (= 4 (length nlst))
  (progn
    (setq blst (last nlst))
    (mapcar
      '(lambda (e / l b)
        (setq
          l (entget e)
          b (cdr (ass))
        )
       )
    )
  )
)

(defun c:NCopy_Multiple (/ obj tmp)
  (while (setq obj	(nentselp "\nSelect (One by One) Objects to copy from XREF ... "))
    (if (= 4 (length obj))
      (progn
        (setq tmp (car (last obj)))
        (setq tmp (entmakex (entget tmp)))
        (vla-transformby
	        (vlax-ename->vla-object tmp)
	        (vlax-tmatrix (caddr obj))
        )
        (Vla-put-layer (vlax-ename->vla-object (entlast)) (getvar 'clayer))
      )
    )
  )
)