
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  DATECART  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] DATECART []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 23/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.5                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allows the user to change the "DATE" attribute from the title blocks (only displayed in EXE).                                                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Asks the user for the layout tabs's list he wants to modify                                                                     ;
; Step n°2        : Asks the user for the new date to be specified                                                                                  ;
; Step n°3        : If the specified date doesn't correspond to "##/##/####" format, the today's date is set by default                             ;
; Step n°4        : Selects all "*Cartouche*" blocks for the specified layout tabs                                                                  ;
; Step n°5        : For each title block, checks if the attribute "DATE" exists                                                                     ;
; Step n°5.a      :   If :True, set the new date's value for the attribute and add the layout tab's name in the 'lst'                               ;
; Step n°5.b      :   If :False, remove the entity from the selection set                                                                           ;
; Step n°6        : Displays the number and list of layout tabs successfully modified                                                               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> lst2str                                       | v1.0.0 - 05/03/2021 (Luna)                                                    ;
;   --•  "UtWin" ---> get-date                                      | v1.0.0 - 19/06/2020 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> select-filter                                 | v3.1.1 - 01/01/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> set-att-value                                 | v2.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> set-cart-date                                 | v1.0.1 - 23/12/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (DATECART) returns the number and list of layout tabs successfully modified                                                         ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.5   |   Correct the filter BlockName of (select-filter) to allow the selection of new cartridges (2023)                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.4   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.3   |   Update (LgT) function to v1.1.0                                                                                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.2   |   Update (ListBox) function to v4.0.0                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Adding the $lf$ global variable for "FORCEDLANGUAGE" system variable                                                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with english/french version                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:DATECART ( / *error* set-cart-date layout-list ll)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun set-cart-date (ll str / jsel i name lst)
    (if
      (not
        (and
          str
          (= (strlen str) 10)
          (wcmatch str "##/##/####")
        )
      )
      (setq str (substr (get-date) 1 10))
    )
    (if (setq jsel (select-filter "BLC" '("*Cartouche*" "`*U*") (list "_X" (list (cons 410 (lst2str ll ",")) (cons 66 1))) nil nil))
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if
          (and
            (assoc "DATE" (get-att-list name))
            (set-att-value name "DATE" str)
          )
          (setq lst (cons (cdr (assoc 410 (entget name))) lst))
          (ssdel name jsel)
        )
      )
    )
    (cons str lst)
  )

  (sssetfirst nil nil)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if
    (and
      (setq layout-list
        (ListBox
          (LgT
            "DATECART: Layout tab(s) selection"
            "DATECART: Sélection des présentation(s)"
            nil
          )
          (LgT
            "Please, select one or more layout tab(s) to be re-dated :"
            "Veuillez sélectionner la ou les présentation(s) à re-dater :"
            nil
          )
          (vl-remove-if '(lambda (x) (member (strcase x) '("TOOLKIT" "TRAVAIL"))) (layoutlist))
          (getvar "CTAB")
          2
          nil
        )
      )
      (setq ll
        (set-cart-date
          layout-list
          (getstring
            (strcat
              (LgT
                "\nPlease, define the desired date (ENTER for \""
                "\nVeuillez définir la date souhaitée (ENTREE pour \""
                nil
              )
              (substr (get-date) 1 10)
              "\") : "
            )
          )
        )
      )
    )
    (princ
      (LgT
        (strcat
          "\nThe new date \"" (car ll) "\" has been successfully defined on "
          (itoa (length (cdr ll)))
          " / "
          (itoa (length layout-list))
          " layout tabs"
          (if (cdr ll)
            (strcat
              " :\n  - "
              (lst2str (cdr ll) "\n  - ")
            )
            "..."
          )
        )
        (strcat
          "\nLa nouvelle date \"" (car ll) "\" a été définie avec succès sur "
          (itoa (length (cdr ll)))
          " / "
          (itoa (length layout-list))
          " présentations"
          (if (cdr ll)
            (strcat
              " :\n  - "
              (lst2str (cdr ll) "\n  - ")
            )
            "..."
          )
        )
        nil
      )
    )
    (princ
      (LgT
        "\nAn error has occurred..."
        "\nUne erreur est survenue..."
        nil
      )
    )
  )
  (sssetfirst nil nil)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)