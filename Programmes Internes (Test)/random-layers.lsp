(defun random-layers (n pmax imax flag / f m p i l pp)
  (defun f (a)
    (cond
      ( (= a 0) (chr (LM:RandRange 48 57)))
      ( (= a 1) (chr (LM:RandRange 65 90)))
      ( (= a 2) (chr (LM:RandRange 97 122)))
    )
  )
  (defun m (a)
    (cond
      ( (= a 0) (strcat (cond (p) ("")) (cond (i) (""))))
      ( (= a 1) (strcat (cond (i) ("")) (cond (p) (""))))
      ( (= a 2) (m (LM:RandRange 0 1)))
    )
  )
  (repeat n
    (setq p "" i "")
    (cond
      ( (= 0 (LM:RandRange 0 2)) (setq p pp))
      (T
        (repeat (LM:RandRange 0 pmax)
          (setq p (strcat p (f (LM:RandRange 1 2))))
        )
      )
    )
    (repeat (LM:RandRange 0 imax)
      (setq i (strcat i (f 0)))
    )
    (setq pp p l (m flag))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 l)
        '(70 . 0)
        (cons 62 (LM:RandRange 1 255))
      )
    )
  )
  (princ)
)

(progn
  (mapcar
    '(lambda (s)
      (write-line
        (vl-prin1-to-string
          (cond
            ((RegExpExecute s "(^.+\\D)?(\\d+$)" nil nil))
            (s)
          )
        )
      )
     )
    lst
  )
  (princ)
)

(defun Slice-String (str / f)
  (defun f (s)
    (cond
      ( (null s))
      ( (null (substr s 2)) s)
      ( (= (< 47 (ascii s) 58) (< 47 (ascii (substr s 2)) 58))
        (strcat (substr s 1 1) (cond ((f (substr s 2))) ("")))
      )
    )
  )
  (if str
    (cons )
  )
)

(defun AlphaNum2lst (a / s)
  (cons
    (setq s (vl-string-right-trim "0123456789" a))
    (read (substr a (1+ (strlen s))))
  )
)

(defun sort-list (lst fun IgnoreCase)
  (mapcar
    '(lambda (i) (nth i lst))
    (vl-sort-i
      (mapcar
        '(lambda (s) (AlphaNum2lst (if IgnoreCase (strcase s) s)))
        lst
      )
      '(lambda (a b)
        (if (= (car a) (= car b))
          ((eval fun) (cdr a) (cdr b))
          ((eval fun) (car a) (car b))
        )
       )
    )
  )
)