
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                 --{  tan  }--                                                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] tan []-----------------------[]                                            ;
;--- Date of creation       > ##/##/####                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > Lee-Mac                                                                                                               ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Calculate the tangent.                                                                                                                          ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (tan) have 1 argument(s) :                                                                                                           ;
;   --•  x                      > correspond to the real number                                                                                     ;
;     (type x) = 'REAL                          | Ex. : 15.65, 0.1, ...                                                                             ;
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
;   The function (tan) returns the tangent.                                                                                                         ;
;     Ex. : (tan )                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun tan (x)
  (if (not (equal 0.0 (cos x) 1e-10))
    (/ (sin x) (cos x))
  )
)