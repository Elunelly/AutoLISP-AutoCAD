(defun divstr (str n)

	(if (> (strlen str) n)
		(append (list (substr str 1 n)) (divstr (substr str (1+ n)) n))
		(list str)
	)

)

(defun ASCII->HEX (n)

	(list (/ n 16) (rem n 16))

)

(defun hex->str (str)

	(vl-list->string
		(mapcar
			'(lambda (c)
				(if (= (strlen c) 2)
					(LM:base->dec c 16)
				)
			 )
			(divstr str 2)
		)
	)

)

(defun str->hex (str)

	(apply	'strcat
		(mapcar
			'(lambda (c)
				(LM:dec->base c 16)
			 )
			(vl-string->list str)
		)
	)

)

;; Decimal to Base  -  Lee Mac
;; Converts a decimal number to another base.
;; n - [int] decimal integer
;; b - [int] non-zero positive integer base
;; Returns: [str] Representation of decimal in specified base

(defun LM:dec->base ( n b )
    (if (< n b)
        (chr (+ n (if (< n 10) 48 55)))
        (strcat (LM:dec->base (/ n b) b) (LM:dec->base (rem n b) b))
    )
)

;; Base to Decimal  -  Lee Mac
;; Converts an number in an arbitrary base to decimal.
;; n - [str] string representing number to convert
;; b - [int] base of input string
;; Returns: [int] Decimal representation of supplied number

(defun LM:base->dec ( n b / l )
    (if (= 1 (setq l (strlen n)))
        (- (ascii n) (if (< (ascii n) 65) 48 55))
        (+ (* b (LM:base->dec (substr n 1 (1- l)) b)) (LM:base->dec (substr n l) b))
    )
)
