
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  LM:rand  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] LM:rand []-----------------------[]                                          ;
;--- Date of creation       > 05/03/2016                                                                                                            ;
;--- Last modification date > 05/03/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Generates a pseudo-random decimal number between 0 and 1. The multiplier 'a', the modulo 'm' and the incremental parameter 'c' are from the     ;
;   book "Numerical Recipes" by William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery. Its generation is based on the    ;
;   value of the system variable DATE, different every second.                                                                                      ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:rand) have 0 argument(s) :                                                                                                       ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaAri" ---> LM:randrange                                  | v1.0.0 - 05/03/2016 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:rand) returns a pseudo-random value, between 0 and 1. The variable $xn is a global variable !                                  ;
;     Ex. : (LM:rand) returns 0.147379                                                                                                              ;
;           (LM:rand) returns 0.817889                                                                                                              ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:rand ( / a c m )
  (setq
    m   4294967296.0
    a   1664525.0
    c   1013904223.0
    $xn (rem (+ c (* a (cond ($xn) ((getvar "DATE"))))) m)
  )
  (/ $xn m)
)