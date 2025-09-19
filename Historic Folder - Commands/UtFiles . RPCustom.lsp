
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  RPCustom  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] RPCustom []-----------------------[]                                         ;
;--- Date of creation       > 11/03/2005                                                                                                            ;
;--- Last modification date > 26/05/2022                                                                                                            ;
;--- Author                 > Didier                                                                                                                ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   With UCSFOLLOW = 0, changing the UCS results in generating a plan view of the new UCS in the current viewport without zooming out by using the  ;
;   value of "VIEWSIZE" and "VIEWCTR" system variables to keep the actual view.                                                                     ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves the current value of "CMDECHO", "VIEWSIZE" and "VIEWCTR"                                                              ;
; Step n°2        : Sets "CMDECHO" to 0                                                                                                             ;
; Step n°3        : Changes the UCS results in generating a plan view of the new UCS in the current viewport (zooms out)                            ;
; Step n°4        : Changes the zoom state by rezooming on the previous view                                                                        ;
; Step n°5        : Sets "CMDECHO" to its previous value                                                                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (RPCustom) returns nothing                                                                                                          ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Correcting some problems                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:RPCustom ( / o h c)
  (mapcar 'set '(o h c) (mapcar 'getvar '("CMDECHO" "VIEWSIZE" "VIEWCTR")))
  (setvar "CMDECHO" 0)
  (command "_PLAN" "")
  (command "_ZOOM" "C" c h)
  (setvar "CMDECHO" o)
  (princ)
)