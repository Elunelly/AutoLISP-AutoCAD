
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                          --{  QuickLayoutSwitch  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] QuickLayoutSwitch []-----------------------[]                                     ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 22/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.1                                                                                                                 ;
;--- Class                  > "PsLaytb"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Using the filter functionality of (ListBox) function, allows the user to switch the current tab.                                                ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves the new layout's name by using (ListBox) function, with a pop-up list                                                 ;
; Step n°2        : Sets the system variable "CTAB" to the new value, selected by the user                                                          ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (QuickLayoutSwitch) returns null and defines the "QLS_Settings" in "URBASOLAR" dictionnary as the initial layout before switching.  ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Add the "Model" layout to the list                                                                                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Add the lData "QLS_Settings" in "URBASOLAR" dictionary to keep in memory for each drawing the name of the last layout          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.3   |   Update of (LgT) function to v1.1.0                                                                                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.2   |   Update of (ListBox) function to v4.0.0                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Adding the $lf$ global variable for "FORCEDLANGUAGE" system variable                                                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Re-designing the whole command to simplify the functionality                                                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:QuickLayoutSwitch ( / layout)
  (setq layout
    (ListBox
      (LgT
        "QUICKLAYOUTSWITCH: Layout tab selection"
        "QUICKLAYOUTSWITCH: Sélection de la présentation"
        nil
      )
      (LgT
        "Please, select the layout tab you want to set to current:"
        "Veuillez sélectionner la présentation que vous souhaitez rendre courante :"
        nil
      )
      (append '("Model") (layoutlist))
      (cond ((vlax-ldata-get "URBASOLAR" "QLS_Settings")) ((getvar "CTAB")))
      0
      nil
    )
  )
  (if layout
    (progn
      (vlax-ldata-put "URBASOLAR" "QLS_Settings" (getvar "CTAB"))
      (setvar "CTAB" layout)
    )
  )
  (princ)
)