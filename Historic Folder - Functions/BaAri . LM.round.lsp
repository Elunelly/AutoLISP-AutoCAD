
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  LM:round  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] LM:round []-----------------------[]                                         ;
;--- Date of creation       > 28/08/2017                                                                                                            ;
;--- Last modification date > 28/08/2017                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to round a number 'n' to the nearest integer.                                                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:round) have 1 argument(s) :                                                                                                      ;
;   --•  n                      > corresponds to the real number needed to be rounded                                                               ;
;     (type n) = 'REAL                          | Ex. : 15.463, -68.975, ...                                                                        ;
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
;   The function (LM:round) returns the nearest integer of 'n'.                                                                                     ;
;     Ex. : (LM:round 15.463) returns 15                                                                                                            ;
;           (LM:round 15.503) returns 16                                                                                                            ;
;           (LM:round -68.975) returns -69                                                                                                          ;
;           (LM:round -68.135) returns -68                                                                                                          ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:round ( n )
  (fix (+ n (if (minusp n) -0.5 0.5)))
)