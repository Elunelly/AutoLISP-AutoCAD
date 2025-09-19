
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  Array-Def  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] Array-Def []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 06/09/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Gets some useful properties about array objects in order to calculate the number of objects in it and their graphical structure.                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Array-Def) have 1 argument(s) :                                                                                                     ;
;   --•  name                   > is the entity name of the "AcDbAssociative*Array"                                                                 ;
;     (type name) = 'ENAME                      | Ex. : <Entity name: ab2e580>, (car (entsel)), ...                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> get-DXF-value                                 | v1.0.0 - 19/06/2020 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Array-Def) returns an association list constituted as follows, otherwise nil :                                                    ;
;   (                                                                                                                                               ;
;     ("TotalObject" . #.#)               --> ['INT]  the total number of objects visible in the array (not depending on source objects)            ;
;     ("ColumnSpacing" . #.#)             --> ['REAL] the distance between each column (along the X axis of the SCO)                                ;
;     ("Columns" . #)                     --> ['INT]  the number of columns                                                                         ;
;     ("RowSpacing" . #.#)                --> ['REAL] the distance between each row (along the Y axis of the SCO)                                   ;
;     ("Rows" . #)                        --> ['INT]  the number of rows                                                                            ;
;     ("LevelSpacing" . #.#)              --> ['REAL] the distance between each level (along the Z axis of the SCO)                                 ;
;     ("Levels" . #)                      --> ['INT]  the number of levels                                                                          ;
;     (90 . #)                            --> ['INT]  the number of source objects                                                                  ;
;     (330 . <Entity name: ab2e580> ...)  --> ['LST]  the list of source entities name (that can be modified/studied)                               ;
;   )                                                                                                                                               ;
;     Ex. : (Array-Def (car (entsel))) returns, for exemple                                                                                         ;
;             (                                                                                                                                     ;
;               ("TotalObject" . 11)                                                                                                                ;
;               ("ColumnSpacing" . 258.456)                                                                                                         ;
;               ("Columns" . 4)                                                                                                                     ;
;               ("RowSpacing" . 241.861)                                                                                                            ;
;               ("Rows" . 3)                                                                                                                        ;
;               ("LevelSpacing" . 1.0)                                                                                                              ;
;               ("Levels" . 1)                                                                                                                      ;
;               (90 . 1)                                                                                                                            ;
;               (330 . <Entity name: ab2e580> ...)                                                                                                  ;
;             )                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Supression of the "AxesAngle" property and addition of the "TotalObject" property to count the real total number in the array  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Array-Def (name / i ent Array-Properties Array-Definition Array-ItemList ItemList ent-lst lst n p)

  (if
    (and
      (= (cdr (assoc 0 (entget name))) "INSERT")
      (wcmatch (getpropertyvalue name "Classname") "AcDbAssociative*Array")
    )
    (progn
      (setq
        Array-properties (entget (cdr (assoc 330 (entget (cdr (assoc 330 (entget name)))))))
        Array-ItemList (entget (cdr (assoc 360 Array-Properties)))
        i (get-DXF-value Array-ItemList 90 4)
      )
      (while (setq Array-ItemList (member '(100 . "AcDbAssocArrayItem") Array-ItemList))
        (setq
          n
            (strcat
              (itoa (get-DXF-value Array-ItemList 90 2))
              ","
              (itoa (get-DXF-value Array-ItemList 90 3))
              ","
              (itoa (get-DXF-value Array-ItemList 90 4))
            )
          p
            (cond
              ((= (logand (get-DXF-value Array-ItemList 90 5) 1) 1) 0)
              ((= (logand (get-DXF-value Array-ItemList 90 5) 8) 8) 1)
            )
          ItemList (cons (cons n p) ItemList)
          Array-ItemList (cdr Array-ItemList)
        )
      )
      (setq
        Array-definition (tblsearch "BLOCK" (cdr (assoc 2 (entget (cdr (assoc -2 (tblsearch "BLOCK" (cdr (assoc 2 (entget name))))))))))
        ent (cdr (assoc -2 Array-Definition))
        lst
          (list
            (cons "TotalObject" (apply '+ (mapcar 'cdr ItemList)))
            (cons "ColumnSpacing" (cdr (assoc 40 (member '(1 . "ItemSpacing") Array-Properties))))
            (cons "Columns" (cdr (assoc 90 (cdddr (member '(1 . "Items") Array-Properties)))))
            (cons "RowSpacing" (cdr (assoc 40 (member '(1 . "RowSpacing") Array-Properties))))
            (cons "Rows" (cdr (assoc 90 (cdddr (member '(1 . "Rows") Array-Properties)))))
            (cons "LevelSpacing" (cdr (assoc 40 (member '(1 . "LevelSpacing") Array-Properties))))
            (cons "Levels" (cdr (assoc 90 (cdddr (member '(1 . "Levels") Array-Properties)))))
          )
      )
      (while ent
        (setq
          ent-lst (cons ent ent-lst)
          ent (entnext ent)
        )
      )
      (setq lst (append lst (list (cons 90 (length ent-lst)) (cons 330 ent-lst))))
    )
  )
  lst

)