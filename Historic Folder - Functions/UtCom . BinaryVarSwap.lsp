
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  BinaryVarSwap  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] BinaryVarSwap []-----------------------[]                                       ;
;--- Date of creation       > 27/05/2022                                                                                                            ;
;--- Last modification date > 27/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to change the value of a binary system variable from its current value. If the variable exists and its current value is 0 or 1,      ;
;   change its value to 1 (if currently 0) or 0 (if currently 1).                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (BinaryVarSwap) have 1 argument(s) :                                                                                                 ;
;   --•  var                    > corresponds to the name of system variable you want to swap its value                                             ;
;     (type var) = 'STR                         | Ex. : "TILEMODE", "CMDECHO", "FILEDIA", "CMDDIA", "ATTDIA", "PICKFIRST", ...                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtVarsy" ---> TileModeSwitch                              | v2.0.0 - 07/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (BinaryVarSwap) returns T if successful, nil otherwise.                                                                            ;
;     Ex. : (BinaryVarSwap "TILEMODE") returns T                                                                                                    ;
;           (BinaryVarSwap "OSMODE") returns nil if current value's different from 0 or 1, T otherwise. So be careful about this issue.             ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun BinaryVarSwap (var / val)
  (and
    (setq val (getvar var))
    (numberp val)
    (member val '(0 1))
    (setvar var (- 1 val))
  )
)