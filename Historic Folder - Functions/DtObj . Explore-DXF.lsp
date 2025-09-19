
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  Explore-DXF  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] Explore-DXF []-----------------------[]                                        ;
;--- Date of creation       > 22/02/2021                                                                                                            ;
;--- Last modification date > 16/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.1                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   From a DXF list of an object, allows to display the list in a more readable way (1 line per pair) and also allows to dig in the DXF lists via   ;
;   the DXF codes related to the names of entities (e.g. : 330, 360, 331, 350, ...). The result can be obtained directly in the current .dwg by     ;
;   displaying it in the command history (F2) or in a .txt file at the desired location. The purpose of this command is to make it easier to        ;
;   understand the construction of DXF lists and their codependent relationships between each physical and non-physical entity. Not only can you    ;
;   choose the DXF codes that allow you to "dig deeper", but you can also choose the number of levels you can go through before you stop digging.   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Explore-DXF) have 4 argument(s) :                                                                                                   ;
;   --•  entlist                > corresponds to the list of pointed pairs that you wish to explore. This allows you to use it not only for DXF     ;
;                               lists, but for any list of pointed pairs, or even to specify the [applist] argument of the function (entget)        ;
;     (type entlist) = 'LST                     | Ex. : (entget (car (entsel))), (entget (car (entsel)) '("PVcase")), ...                           ;
;   --•  n                      > corresponds to the number of levels you can go through before you stop digging                                    ;
;     (type n) = 'INT                           | Ex. : 0, 1, 5, ...                                                                                ;
;   --•  lst                    > corresponds to the list of DXF codes you want to explore by digging further thanks to them. Only DXF codes with   ;
;                               an entity name as value will be considered                                                                          ;
;     (type lst) = 'LST                         | Ex. : '(330 360 331 350), nil, ...                                                                ;
;   --•  flag                   > if you want to print the results on the command historic panel (F2), set 'flag' to nil and if you want to print   ;
;                               it on an external .txt file because you want to save the results or dig really deep, set 'flag' to T                ;
;     (type flag) = 'SYM                        | Ex. : nil or T                                                                                    ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> space                                         | v1.0.0 - 22/02/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> prompt-entlist                                | v3.0.0 - 15/07/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Explore-DXF) returns the results in the command historic panel (F2) if 'flag' equals nil, otherwise it returns nil and print the  ;
;   results in an external .txt file if 'flag' equals T.                                                                                            ;
;     Ex. : (Explore-DXF (entget (car (entsel))) 0 nil nil) returns                                                                                 ;
;       Explore-DXF in progress from "LWPOLYLINE" (<Entity name: 20aca1664d0>) :                                                                    ;
;       |      (-1 . <Entity name: 20aca1664d0>)                                                                                                    ;
;       |      (0 . "LWPOLYLINE")                                                                                                                   ;
;       |      (330 . <Entity name: 20aca1641f0>)                                                                                                   ;
;       |      (5 . "285")                                                                                                                          ;
;       |      (100 . "AcDbEntity")                                                                                                                 ;
;       |      (67 . 0)                                                                                                                             ;
;       |      (410 . "Model")                                                                                                                      ;
;       |      (8 . "0")                                                                                                                            ;
;       |      (100 . "AcDbPolyline")                                                                                                               ;
;       |      (90 . 2)                                                                                                                             ;
;       |      (70 . 0)                                                                                                                             ;
;       |      (43 . 0.0)                                                                                                                           ;
;       |      (38 . 0.0)                                                                                                                           ;
;       |      (39 . 0.0)                                                                                                                           ;
;       |      (10 1477.97 1097.68)                                                                                                                 ;
;       |      (40 . 0.0)                                                                                                                           ;
;       |      (41 . 0.0)                                                                                                                           ;
;       |      (42 . 0.0)                                                                                                                           ;
;       |      (91 . 0)                                                                                                                             ;
;       |      (10 2559.87 1851.45)                                                                                                                 ;
;       |      (40 . 0.0)                                                                                                                           ;
;       |      (41 . 0.0)                                                                                                                           ;
;       |      (42 . 0.0)                                                                                                                           ;
;       |      (91 . 0)                                                                                                                             ;
;       |      (210 0.0 0.0 1.0)                                                                                                                    ;
;       |                                                                                                                                           ;
;       End of exploration...                                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Fixing (strcat (if ...)) issues                                                                                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   ...                                                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.1   |   Added opening the file via Notepad if a file has been created                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   ...                                                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Added the flag variable and the ability to generate a .txt file for extracting and saving DXF data                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Replaced the use of (vl-princ-to-string) functions with (vl-prin1-to-string) in order to keep the exact display of the strings | ;
; |            |   present in the DXF list (preservation of inverted commas).                                                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Explore-DXF (entlist n lst flag / prompt-entlist filename file)
  (defun prompt-entlist (entlist str-list / eType eName DXFcode eList DXFpath i str)
    (mapcar
      'set
      (list 'eType 'eName 'DXFcode 'eList 'DXFpath 'i)
      str-list
    )
    (cond
      ((= (length str-list) 2)
        (setq i '(0)
              eList (list eType)
              DXFpath '(0)
              str
          (strcat  "Explore-DXF in progress from \""
            eType
            "\" ("
            (vl-prin1-to-string eName)
            ") : "
          )
        )
      )
      (t
        (setq eList (append eList (list eType))
              DXFpath (append DXFpath (list ""))
              i (append i (list 1))
              str
          (strcat  (apply 'strcat (mapcar '(lambda (s) (strcat (itoa s) ".")) i))
            "  \""
            eType
            "\" ("
            (vl-prin1-to-string eName)
            ") at "
            (itoa DXFcode)
            " code, from : "
            "\n  "
            (apply 'strcat (mapcar '(lambda (e c) (strcat "-> " e (if (numberp c) (strcat " (" (itoa c) ") ") ""))) eList DXFpath))
          )
        )
      )
    )
    (if (and flag
       file
       str
        )
      (write-line (strcat "\n" str "\n") file)
      (prompt  (strcat  "\n" str "\n|"))
    )
    (mapcar
      '(lambda (l)
        (setq str (strcat (space (+ (* (length i) 2) 4)) (vl-prin1-to-string l)))
        (if (and flag
           file
           str
            )
          (write-line str file)
          (prompt  (strcat  str "\n|"))
        )
       )
      entlist
    )
    (if (<= (length i) n)
      (progn  
        (foreach l entlist
          (if (and
            (member (car l) lst)
            (= (type (cdr l)) 'ENAME)
            (setq entlist (entget (cdr l)))
            (setq i (subst (1+ (last i)) (last i) i))
            (setq eList (subst eType (last eList) eList))
            (setq DXFpath (subst (car l) (last DXFpath) DXFpath))
              )
            (prompt-entlist
              entlist
              (list  (cdr (assoc 0 entlist))
                (cdr (assoc -1 entlist))
                (car l)
                eList
                DXFpath
                i
              )
            )
          )
        )
      )
      (setq i (reverse (cdr (reverse i)))
            eList (reverse (cdr (reverse eList)))
            DXFpath (reverse (cdr (reverse DXFpath)))
      )
    )
    (princ)
  )

  (if flag
    (setq filename
      (getfiled "Save file - DXF Export"
          (strcat (getvar "DWGPREFIX") (rtos (getvar "CDATE") 2 0) "_" (vl-filename-base (getvar "DWGNAME")) " - " (cdr (assoc 0 entlist)) ".txt")
          "txt"
          45
      )
          file (open filename "w")
    )
  )
  (prompt-entlist
    entlist
    (list  (cdr (assoc 0 entlist))
      (cdr (assoc -1 entlist))
    )
  )
  (if (and flag file)
    (progn
      (write-line "\nEnd of exploration..." file)
      (close file)
      (prompt "\nFile has been created !")
      (startapp "NotePad" filename)
    )
    (prompt "\nEnd of exploration...")
  )
  (princ)
)