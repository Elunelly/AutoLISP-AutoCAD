
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:randrange  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:randrange []-----------------------[]                                       ;
;--- Date of creation       > 05/03/2016                                                                                                            ;
;--- Last modification date > 05/03/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Generates a pseudo-random integer between a minimum and maximum number specified in the arguments. This function is based on the (LM:rand)      ;
;   function of LeeMac.                                                                                                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:randrange) have 2 argument(s) :                                                                                                  ;
;   --•  a                      > corresponds to the lower bound (inclusive value) for the generation of the pseudo-random number.                  ;
;     (type a) = 'INT                           | Ex. : 1, 5, ...                                                                                   ;
;   --•  b                      > corresponds to the upper bound (inclusive value) for the generation of the pseudo-random number.                  ;
;     (type b) = 'INT                           | Ex. : 8, 49, ...                                                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:rand                                       | v1.0.0 - 05/03/2016 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:randrange) returns a pseudo-random integer value between the bounds specified in the argument.                                 ;
;     Ex. : (LM:randrange 6 11) returns either 6, 7, 8, 9, 10 or 11 in a pseudo-random way                                                          ;
;           (LM:randrange -1 1) returns either -1, 0 or 1 in a pseudo-random way                                                                    ;
;           (LM:randrange 3 0) returns either 0, 1, 2 or 3 in a pseudo-random way                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:randrange ( a b )
  (+ (min a b) (fix (* (LM:rand) (1+ (abs (- a b))))))
)