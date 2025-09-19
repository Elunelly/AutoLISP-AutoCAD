
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                            --{  PVROWNUMBERING  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] PVROWNUMBERING []-----------------------[]                                      ;
;--- Date of creation       > 04/07/2024                                                                                                            ;
;--- Last modification date > 04/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   PVcase is actually not allowing to change the starting number for rows when different zones don't have parallel rows (so PVcase always starts   ;
;   at 1). In order to work correctly with our process (every row number have to be different), this programm allows us to increase the numbering   ;
;   selected row all at once.                                                                                                                       ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtUse" ---> getkdh                                        | v2.0.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (PVROWNUMBERING) returns [...]                                                                                                                 ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:PVROWNUMBERING ( / val c jsel i name n lst mx mn)
  (setq jsel (ssget '((0 . "TEXT,MTEXT") (8 . "PVcase Row Numbering"))))
  (setq val
    (getkdh
      (quote (getint msg))
      (LgT
        "\nLast row's number"
        "\nNuméro de la dernière rangée"
        nil
      )
      (list 2)
      " : "
      (vlax-ldata-get "URBASOLAR" "PVROWNUMBERING_Start" 1)
      (LgT
        (strcat
          "\nPVROWNUMBERING : Last row's number"
          "\nCorresponds to the additional value that will be added on each selected text value."
          "\nFor example, if the last numbered row is 17 and the selected PVarea texts start at 1, specifying 17 means each row will have "
          "their value added to 17 (row n°1 → 1+17 = 18), making it easier to make sure each zone will follow the previous one."
        )
        (strcat
          "\nPVROWNUMBERING : Numéro de la dernière rangée"
          "\nCorrespond à la valeur qui sera ajoutée pour chaque texte sélectionné."
          "\nPar exemple, si la dernière rangée numérotée est 17 et les textes sélectionnés commencent à 1, alors chaque rangée se verra "
          "additionné de la valeur 17 (rangée n°1 → 1+17 = 18), simplifiant la continuité de numérotation des rangées pour l'ensemble du projet."
        )
        nil
      )
    )
  )
  (repeat (setq c (sslength jsel) i c)
    (setq name (ssname jsel (setq i (1- i))))
    (entmod
      (subst
        (cons 1 (itoa (setq n (+ val (atoi (cdr (assoc 1 (entget name))))))))
        (assoc 1 (entget name))
        (entget name)
      )
    )
    (setq lst (cons n lst))
  )
  (setq
    mx (apply 'max lst)
    mn (apply 'min lst)
  )
  (vlax-ldata-put "URBASOLAR" "PVROWNUMBERING_Start" mx)
  (princ
    (LgT
      (strcat
        "\n  • Amount of row = " (itoa (/ c 2)) " or " (itoa (1+ (- mx mn))) "u  (→ " (itoa i) " texts)"
        "\n  • First row     = " (itoa mn)
        "\n  • Last row      = " (itoa mx)
      )
      (strcat
        "\n  • Nombre de rangée = " (itoa (/ c 2)) " ou " (itoa (1+ (- mx mn))) "u  (→ " (itoa i) " textes)"
        "\n  • Première rangée  = " (itoa mn)
        "\n  • Dernière rangée  = " (itoa mx)
      )
      nil
    )
  )
  (princ)
)