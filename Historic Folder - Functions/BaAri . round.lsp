
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  round  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] round []-----------------------[]                                           ;
;--- Date of creation       > 06/12/2021                                                                                                            ;
;--- Last modification date > 06/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Round a number in the set R to a number of decimal places, a multiple of number n, etc...                                                       ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (round) have 3 argument(s) :                                                                                                         ;
;   --•  n                      > corresponds to the number you want to round                                                                       ;
;     (type n) = 'REAL                          | Ex. : 2.12, -9, (/ 56.21 8.1284), ...                                                             ;
;   --•  p                      > corresponds to the number of decimal if 'f' equals nil, to the rounded multiple if 'f' equals T, '< or '>         ;
;     (type p) = 'REAL                          | Ex. : 2.12, -9, (/ 56.21 8.1284), ...                                                             ;
;   --•  f                      > corresponds to the function we want to apply for the rounding                                                     ;
;     (type f) = 'SYM                           | Ex. : '< (arrondi inf.), '> (arrondi sup.), T (multiple) ou nil (d•cimal)                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:round                                      | v1.1.0 - 28/08/2017 (LeeMac)                                                  ;
;   --•  "BaAri" ---> LM:roundm                                     | v1.1.0 - 28/08/2017 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (round) returns the number 'n' rounded to 'p' decimal places if 'f' equals nil, otherwise returns the number 'n' rounded to the    ;
;   multiple 'p' (in upper, lower or simple rounding). If 'p' < 0, returns nil.                                                                     ;
;     Ex. : (round 2.12 0.5 T) returns 2.0                                                                                                          ;
;           (round 31.2 10 '>) returns 40.0                                                                                                         ;
;           (round 31.2 10 '<) returns 30.0                                                                                                         ;
;           (round 31.2 10 T) returns 30                                                                                                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun round (n p f)
  (cond
    ( (= p 0) (if (null f) (LM:round n)))
    ( (> p 0)
      (cond
        ( (null f) (LM:roundm n (expt 10.0 (- p))))
        ( (= f '<) ((lambda ( r ) (cond ((equal 0.0 r 1e-8) n) ((< n 0) (- n r p)) ((- n r)))) (rem n p)))
        ( (= f '>) ((lambda ( r ) (cond ((equal 0.0 r 1e-8) n) ((< n 0) (- n r)) ((+ n (- p r))))) (rem n p)))
        ( (= f T) (LM:roundm n p))
      )
    )
  )
)