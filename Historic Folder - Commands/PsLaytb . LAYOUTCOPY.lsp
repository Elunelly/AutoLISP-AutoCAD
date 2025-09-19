
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  LAYOUTCOPY  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] LAYOUTCOPY []-----------------------[]                                        ;
;--- Date of creation       > 29/09/2022                                                                                                            ;
;--- Last modification date > 19/12/2023                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.3                                                                                                                 ;
;--- Class                  > "PsLaytb"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Copy the current layout tab next to it and set the new layout tab current.                                                                      ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (LAYOUTCOPY) returns the historic's lines of _-LAYOUT command.                                                                      ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.3   |   Fix the (setvar "CTAB") to set the new created layout as the current layout and display the current layout changed             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.2   |   Use the default answer of _-LAYOUT command and retrieve the new name in layout collection through TabOrder                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Fix the issue of additionnal space for iteration of copy                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:LAYOUTCOPY ( / layout layouts tbo tabs)
  (if (= "Model" (setq layout (getvar "CTAB")))
    (princ
      (LgT
        "\nThe Model tab can't be copied..."
        "\nL'onglet Objet ne peut pas être copié..."
        nil
      )
    )
    (progn
      (setq
        layouts (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object)))
        tbo (vla-get-TabOrder (vla-item layouts layout))
      )
      (command-s "_-LAYOUT" "_Copy" "" "")
      (vlax-for l layouts (setq tabs (cons (cons (vla-get-TabOrder l) (vla-get-Name l)) tabs)))
      (setq layout (cdr (assoc tbo tabs)))
      (setvar "CTAB" layout)
      (princ
        (strcat
          (LgT "\nThe new layout created \"" "\nLa nouvelle présentation crée \"" nil)
          layout
          (LgT "\" is now the current layout." "\" est désormais la présentation courante." nil)
        )
      )
    )
  )
  (princ)
)