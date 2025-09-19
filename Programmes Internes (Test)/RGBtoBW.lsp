(defun RGB->BW (RGB m / R G B Rm Gm Bm)

	(if (and (listp m)
		 (= 1.0 (apply '+ m))
	    )
		(setq c
			(apply '+
				(mapcar '* RGB m)
			)
		)
		(setq c
			(apply '+
				(mapcar '*
					RGB
					(list (/ 1. 3.)
					      (/ 1. 3.)
					      (/ 1. 3.)
					)
				)
			)
		)
	)
	(mapcar 'round (list c c c))

)

(defun round (n)

	(fix (+ n 0.5))

)

;; RGB -> True  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values

(defun LM:RGB->True ( r g b )
    (logior (lsh (fix r) 16) (lsh (fix g) 8) (fix b))
)

;; True -> RGB  -  Lee Mac
;; Args: c - [int] True Colour

(defun LM:True->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(8 16 24))
)

;; RGB -> ACI  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values

(defun LM:RGB->ACI ( r g b / c o )
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
            (vlax-release-object o)
            (if (vl-catch-all-error-p c)
                (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
                c
            )
        )
    )
)

;; ACI -> RGB  -  Lee Mac
;; Args: c - [int] ACI (AutoCAD Colour Index) Colour (1<=c<=255)

(defun LM:ACI->RGB ( c / o r )
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq r
                (vl-catch-all-apply
                   '(lambda ( )
                        (vla-put-colorindex o c)
                        (list (vla-get-red o) (vla-get-green o) (vla-get-blue o))
                    )
                )
            )
            (vlax-release-object o)
            (if (vl-catch-all-error-p r)
                (prompt (strcat "\nError: " (vl-catch-all-error-message r)))
                r
            )
        )
    )
)

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)

(defun SetBWColors (entlist / c r g b)

	(cond
		((assoc 420 entlist)
			(setq c (RGB->BW (LM:True->RGB (cdr (assoc 420 entlist))) nil)
			      r (nth 0 c)
			      g (nth 1 c)
			      b (nth 2 c)
			      entlist
				(entmod
					(subst
						(cons 420 (LM:RGB->True r g b))
						(assoc 420 entlist)
						entlist
					)
				)
			      entlist
				(entmod
					(subst
						(cons 62 (LM:RGB->ACI r g b))
						(assoc 62 entlist)
						entlist
					)
				)
			)
		)
		((assoc 62 entlist)
			(setq c (RGB->BW (LM:RGB->ACI (cdr (assoc 62 entlist))) nil)
			      r (nth 0 c)
			      g (nth 1 c)
			      b (nth 2 c)
			      entlist
				(entmod
					(subst
						(cons 62 (LM:RGB->ACI r g b))
						(assoc 62 entlist)
						entlist
					)
				)
			)
		)
	)

)