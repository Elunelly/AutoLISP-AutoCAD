
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                             --{  COPYCHSPACE  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] COPYCHSPACE []-----------------------[]                                        ;
;--- Date of creation       > 23/09/2022                                                                                                            ;
;--- Last modification date > 23/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "PsViewp"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Combines the _COPY and _CHSPACE commands to copy objects between Model Space and Paper Space, without deleting the originals.                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (COPYCHSPACE) returns nothing, except the prompting lines of _COPY and _CHSPACE commands                                            ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:COPYCHSPACE (/ jsel)
  (if (and (= 0 (getvar "TILEMODE")) (setq jsel (ssget)))
    (progn
      (command "_COPY" jsel "" "" "0.0,0.0,0.0")
      (command "_CHSPACE" jsel "")
    )
  )
  (princ)
)