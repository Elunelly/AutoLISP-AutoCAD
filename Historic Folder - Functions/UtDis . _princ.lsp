
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  _princ  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] _princ []-----------------------[]                                          ;
;--- Date of creation       > ##/##/2005                                                                                                            ;
;--- Last modification date > ##/##/2005                                                                                                            ;
;--- Author                 > Michael_Puckett                                                                                                       ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Forces screen updates, allowing for example to prompt a progress bar on command line.                                                           ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (_princ) have 1 argument(s) :                                                                                                        ;
;   --•  str                    > corresponds to the string you need to prompt on command line                                                      ;
;     (type str) = 'STR                         | Ex. : "Benchmarking...", "Command in progress", ...                                               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (_princ) returns nothing but forces screen updates on command line.                                                                ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun _princ (str)
  (princ str)
  (princ)
)