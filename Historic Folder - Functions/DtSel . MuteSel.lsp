
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  MuteSel  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] MuteSel []-----------------------[]                                          ;
;--- Date of creation       > 16/06/2022                                                                                                            ;
;--- Last modification date > 16/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Displays custom messages while using (ssget) instead of the basic "select objects :" thanks to "NOMUTT" system variable.                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (MuteSel) have 2 argument(s) :                                                                                                       ;
;   --•  msg                    > is the message you wanna display for selection                                                                    ;
;     (type msg) = 'STR                         | Ex. : "\nSelect objects :", "\nPlease select a dark viewport, thanks in advance ! ", ...          ;
;   --•  fun                    > is the quoted function that will be use for selection (It has to be inside a (quote) function !)                  ;
;     (type fun) = 'LST                         | Ex. : (quote (ssget)), (quote (ssget "_.+:E:S" '((0 . "LWPOLYLINE")))), ...                       ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 16/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (MuteSel) returns a selection set if no errors, nil otherwise.                                                                     ;
;     Ex. :                                                                                                                                         ;
;     (mutesel "\nSelect a line :" (quote (ssget '((0 . "LINE")))))                                                                                 ;
;     Select a line :                                                                                                                               ;
;     <Selection set: 37f>                                                                                                                          ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun MuteSel (msg fun / *error* nom sel)
  (defun *error* (msg)
    (setvar "NOMUTT" nom)
    (princ msg)
    nil
  )
  (setq nom (getvar "NOMUTT"))
  (princ msg)
  (setvar "NOMUTT" 1)
  (setq sel (eval fun))
  (setvar "NOMUTT" nom)
  sel
)