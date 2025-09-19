
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  LM:roundm  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] LM:roundm []-----------------------[]                                        ;
;--- Date of creation       > 28/08/2017                                                                                                            ;
;--- Last modification date > 28/08/2017                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to round a number 'n' to the nearest multiple of 'm'.                                                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:round) have 2 argument(s) :                                                                                                      ;
;   --•  n                      > corresponds to the real number needed to be rounded                                                               ;
;     (type n) = 'REAL                          | Ex. : 15.463, -68.975, ...                                                                        ;
;   --•  m                      > corresponds to multiple we want to use for rounding                                                               ;
;     (type m) = 'REAL                          | Ex. : 0.25, 0.1, 15, ...                                                                          ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaAri" ---> round                                         | v1.0.0 - 06/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:roundm) returns the nearest multiple of 'm' for 'n'.                                                                           ;
;     Ex. : (LM:roundm 15.463 0.25) returns 15.5                                                                                                    ;
;           (LM:roundm 15.503 0.1) returns 15.5                                                                                                     ;
;           (LM:roundm -68.975 0.7) returns -69.3                                                                                                   ;
;           (LM:roundm -68.135 15) returns -75                                                                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:roundm ( n m )
  (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
)