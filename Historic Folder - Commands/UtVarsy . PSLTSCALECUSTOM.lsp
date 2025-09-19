
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                           --{  PSLTSCALECUSTOM  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] PSLTSCALECUSTOM []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.1                                                                                                                 ;
;--- Class                  > "UtVarsy"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Changes the system variable PSLTSCALE value on several (or all) layout tab(s), because different layout tabs can have different settings for    ;
;   PSLTSCALE.                                                                                                                                      ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves the current layout tab (to get back on it at the end)                                                                 ;
; Step n°2        : While the new value is not 0 or 1, asks the user (with (getkdh) function) again                                                 ;
; Step n°2.a        : If 'value = "?", it will open the Help Center from AutoCAD directly on the PSLTSCALE (System Variable) page                   ;
; Step n°3        : Retrieves the current value of NOMUTT and sets it to 1                                                                          ;
; Step n°4        : Retrieves the layout tab's list to be modified (if you don't want to run it for all of them). They're all selected by default   ;
; Step n°5        : For each selected layout tab,                                                                                                   ;
; Step n°5.a        : Sets the layout tab current (to be able to access to its PSLTSCALE's value)                                                   ;
; Step n°5.b        : Sets PSLTSCALE to the new value and keep the old value too                                                                    ;
; Step n°6        : Gets back on initial layout tab and reset NOMUTT to its initial value                                                           ;
; Step n°7        : Retrieves the length of the longest string for the name of layout tabs                                                          ;
; Step n°8        : Displays the new value for the list of selected layout tabs, with the old/new value of PSLTSCALE for each layout name           ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> space                                         | v1.0.0 - 22/02/2021 (Luna)                                                    ;
;   --•  "UtCom" ---> SetVarList                                    | v2.0.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.0.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> get-layouts-pos                               | v2.0.0 - 24/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.1.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (PSLTSCALECUSTOM) returns the list of layout tab(s) selected by the user with the old value / new value of PSLTSCALE for each one.  ;
;   Then you'll have the number of layout tab(s) where old value = new value.                                                                       ;
;     Ex. :                                                                                                                                         ;
;       Command: PSLTSCALECUSTOM                                                                                                                    ;
;       Enter a new value for PSLTSCALE [0/1/?] <0>: 0                                                                                              ;
;       The PSLTSCALE system variable is now redefined as 0 for the following layout tab(s):                                                        ;
;        •  Layout1    ( 1 → 0 )                                                                                                                    ;
;        •  Layout2    ( 0 → 0 )                                                                                                                    ;
;        •  Layout3    ( 1 → 0 )                                                                                                                    ;
;        •  Layout4    ( 0 → 0 )                                                                                                                    ;
;        •  Layout5    ( 1 → 0 )                                                                                                                    ;
;        •  Layout6    ( 1 → 0 )                                                                                                                    ;
;        •  Layout7    ( 1 → 0 )                                                                                                                    ;
;        •  Layout8    ( 1 → 0 )                                                                                                                    ;
;        •  Layout9    ( 0 → 0 )                                                                                                                    ;
;        •  Layout10   ( 0 → 0 )                                                                                                                    ;
;        •  Layout11   ( 1 → 0 )                                                                                                                    ;
;        •  Layout12   ( 1 → 0 )                                                                                                                    ;
;        •  Layout13   ( 0 → 0 )                                                                                                                    ;
;        •  Layout14   ( 1 → 0 )                                                                                                                    ;
;        •  Layout15   ( 0 → 0 )                                                                                                                    ;
;        •  Layout16   ( 0 → 0 )                                                                                                                    ;
;        •  Layout17   ( 0 → 0 )                                                                                                                    ;
;        •  Layout18   ( 0 → 0 )                                                                                                                    ;
;        •  Layout19   ( 0 → 0 )                                                                                                                    ;
;        •  Layout20   ( 0 → 0 )                                                                                                                    ;
;       In the list above, 11 / 20 layout tab(s) already had PSLTSCALE = 0.                                                                         ;
;       Command:                                                                                                                                    ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:PSLTSCALECUSTOM ( / *error* InitLayout nom value lst lng)
  (defun *error* (msg)
    (if InitLayout (setvar "CTAB" InitLayout))
    (if nom (setvar "NOMUTT" nom))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )

  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("CTAB" InitLayout nil)))
  (while
    (<
      1
      (setq value
        (getkdh
          (quote (getint msg))
          (LgT
            "\nEnter a new value for PSLTSCALE"
            "\nEntrez une nouvelle valeur pour PSLTSCALE"
            nil
          )
          (list 4 "0 1")
          ": "
          0
          '(help "" "PSLTSCALE")
        )
      )
    )
    (princ
      (LgT
        "\nRequires 0 or 1 only."
        "\nNécessite seulement 0 ou 1."
        nil
      )
    )
  )
  (SetVarList '(("NOMUTT" nom 1)))
  (setq lst
    (mapcar
      '(lambda (layout / lst psl)
        (setq
          lst (SetVarList (list (list "CTAB" nil layout) (list "PSLTSCALE" nil value)))
          psl (cadr lst)
        )
        (list layout (cadr psl) (last psl))
       )
      (ListBox
        (LgT
          "PSLTSCALE: Layout tab(s) selection"
          "PSLTSCALE: Sélection des présentation(s)"
          nil
        )
        (LgT
          "Please, select one or more layout tab(s) to be modified :"
          "Veuillez sélectionner la ou les présentation(s) à modifier :"
          nil
        )
        (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
        (layoutlist)
        2
        nil
      )
    )
  )
  (SetVarList (list (list "CTAB" nil InitLayout) (list "NOMUTT" nil nom)))
  (setq
    InitLayout nil
    nom nil
    lng (apply 'max (mapcar '(lambda (x) (strlen (car x))) lst))
  )
  (princ
    (strcat
      (LgT
        "\nThe PSLTSCALE system variable is now redefined as "
        "\nLa variable PSLTSCALE est désormais redéfinie à "
        nil
      )
      (itoa value)
      (LgT
        " for the following layout tab(s):"
        " pour les onglet(s) de présentations suivant :"
        nil
      )
      (apply
        'strcat
        (mapcar
          '(lambda (x) (strcat "\n •  " (car x) (space (- lng (strlen (car x)))) "   (" (itoa (cadr x)) " → " (itoa (last x)) ")"))
          lst
        )
      )
      (LgT
        "\nIn the list above, "
        "\nDans la liste ci-dessus, "
        nil
      )
      (itoa (length (vl-remove-if-not '(lambda (x) (= value (cadr x))) lst)))
      " / "
      (itoa (length lst))
      (LgT
        " layout tab(s) already had PSLTSCALE = "
        " onglet(s) de présentation avaient déjà PSLTSCALE = "
        nil
      )
      (itoa value)
      "."
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)