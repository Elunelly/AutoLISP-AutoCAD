(defun c:ATTABLE (/ *error* date Shell:Open att lst pt filename file r c)
  (defun *error* (msg)
    (if file (close file))
    (princ msg)
  )
  (defun date (/ r d h)
    (setq
      r (rtos (getvar "CDATE") 2 6)
      d (strcat (substr r 1 4) "-" (substr r 5 2) "-" (substr r 7 2))
      h (strcat (substr r 10 2) "-" (substr r 12 2) "-" (substr r 14 2))
      r (strcat d "." h)
    )
  )
  (defun Shell:Open (filepath / shell)
    (if filepath
      (progn
        (setq shell (vlax-create-object "Shell.Application"))
        (vlax-invoke shell 'Open filename)
        (vlax-release-object shell)
      )
    )
  )
  
  (and
    (setq att "IDEN")
    (setq lst
      (loop-a-list-properties
        (ssget '((0 . "INSERT") (2 . "DN*") (66 . 1)))
        (list 'EffectiveName att)
        1
        '+
        nil
      )
    )
    (setq pt
      (getkdh
        (quote (getpoint msg))
        "\nPoint d'insertion de la table"
        '("CSV")
        " : "
        "0.0,0.0,0.0"
        "\n [point] → correspond au point d'insertion de la table de résultat (coin supérieur gauche)
         \n [CSV]   → exporte les résultats dans un fichier .csv (exploitable par Excel)
         \n [0,0,0] → valeur par défaut définie sur l'origine SCG du plan (0.0 0.0 0.0)"
      )
    )
    (cond
      ( (listp pt) (setq pt (trans pt 1 0)))
      ( (= pt "0.0,0.0,0.0") (setq pt '(0.0 0.0 0.0)))
      ( (= pt "CSV") (setq pt (not pt)))
    )
    (if pt
      (progn
        (setq lst (apply 'append (mapcar '(lambda (x) (mapcar 'cons (list (car x)) (cdr x))) lst)))
        (JH:list-to-table (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))) lst pt "STANDARD")
      )
      (progn
        (setq filename (getfiled "Recherche fichier CSV" (strcat (getvar "DWGPREFIX") (getvar "DWGNAME") "_ATTABLE " (date)) "csv" 1))
        (setq file (open filename "W"))
        (write-line (strcat "NOM;" att ";Nombre") file)
        (mapcar
          '(lambda (b / l)
            (setq
              l (cdr b)
              b (car b)
            )
            (mapcar
              '(lambda (a / n)
                (setq
                  n (cdr a)
                  a (car a)
                )
                (write-line (strcat b ";" a ";" (itoa n)) file)
               )
              l
            )
           )
          lst
        )
        (setq file (close file))
        (Shell:Open filename)
      )
    )
  )
  (princ)
)

;; JH:list-to-table --> Jonathan Handojo
;; Creates a table from a list of lists of strings
;; space - ModelSpace or Paperspace vla object
;; lst - list of lists where each list is a list of strings
;;  => if you wish to insert a block in the cell, prefix using "<block>" followed by the block name
;;  => e.x. if you want to insert the block "TestBlock1", input the string as "<block>TestBlock1"
;; pt - Insertion point of table (2 or 3 reals)
;; tblstyle - Table style to use
(defun JH:list-to-table (space lst pt tblstyle / i j lens ncols rows totlen txt vtable)
  (setq
    ncols (apply 'max (mapcar 'length lst))
    vtable (vla-AddTable space (vlax-3d-point pt) (length lst) ncols 10 10)
  )
  (vla-put-RegenerateTableSuppressed vtable :vlax-true)
  (vla-put-StyleName vtable tblstyle)
  (repeat (setq i (length lst))
    (setq rows (nth (setq i (1- i)) lst))
    (vla-SetRowHeight vtable i (* 2 (vlax-invoke vtable 'GetCellTextHeight i 0)))
    (repeat (setq j (length rows))
      (setq lens
        (cons
          (+
            (abs
              (apply
                '-
                (mapcar
                  'car
                  (textbox
                    (list
                      (cons 1 (setq txt (nth (setq j (1- j)) rows)))
                      (cons 40 (vlax-invoke vtable 'GetCellTextHeight i j))
                      (cons 7 (vlax-invoke vtable 'GetCellTextStyle i j))
                    )
                  )
                )
              )
            )
            (vlax-invoke vtable 'GetCellTextHeight i j)
          )
          lens
        )
      )
      (if (eq (strcase (substr txt 1 7)) "<BLOCK>")
        (progn
          (setq blk (substr txt 8))
          (if
            (and
              (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*")
              (vlax-method-applicable-p vtable 'setblocktablerecordid32)
            )
            (vla-SetBlockTableRecordId32 vtable i j (vla-get-ObjectID (vla-item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) blk)))
            (vla-SetBlockTableRecordId vtable i j (vla-get-ObjectID (vla-item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) blk)) :vlax-true)
          )
        )
        (vla-SetText vtable i j txt)
      )
    )
    (setq totlen (cons lens totlen) lens nil)
  )
  (repeat ncols (vla-SetColumnWidth vtable (setq ncols (1- ncols)) (apply 'max (vl-remove nil (mapcar '(lambda (x) (nth ncols x)) totlen)))))
  (vla-put-RegenerateTableSuppressed vtable :vlax-false)
  vtable
)



(defun newpoint (pt ht jf / x y z up str ang sel)
  (setq
    sel (ssadd)
    up pt
    pt (trans pt 1 0)
    x (car pt)
    y (cadr pt)
    z (cond ((caddr pt)) (0.0))
    str
      (strcat
        "\\pxql;Coordinates (WCS)"
        "\nX : " (rtos x)
        "\nY : " (rtos y)
        "\nZ : " (rtos z)
      )
    ang (angle '(0.0 0.0 0.0) (getvar "UCSXDIR"))
  )
  (if (vl-remove T (mapcar 'equal pt up))
    (setq str
      (strcat
        str
        "\nCoordinates (UCS)"
        "\nX : " (rtos (car up))
        "\nY : " (rtos (cadr up))
        "\nZ : " (rtos (caddr up))
      )
    )
  )
  (ssadd
    (entmakex
      (list
        '(0 . "POINT")
        '(100 . "AcDbEntity")
        '(100 . "AcDbPoint")
        (cons 10 pt)
      )
    )
    sel
  )
  (ssadd
    (entmakex
      (list
        '(0 . "MTEXT")
        '(100 . "AcDbEntity")
        '(100 . "AcDbMText")
        (cons 10 pt)
        (cons 40 ht)
        (cons 41 (* ht 13))
        (cons 71 jf)
        (cons 1 str)
        (cons 50 ang)
        '(90 . 16)
        '(63 . 256)
        '(45 . 1.5)
        '(441 . 0)
      )
    )
    sel
  )
  (sssetfirst nil sel)
  pt
)