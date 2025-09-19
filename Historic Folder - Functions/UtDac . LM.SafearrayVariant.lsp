
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                         --{  LM:SafearrayVariant  }--                                                         | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] LM:SafearrayVariant []-----------------------[]                                    ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Returns a populated safearray variant of a specified data type                                                                                  ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:SafearrayVariant) have 2 argument(s) :                                                                                           ;
;   --•  typ                    > Variant type enum (e.g. vlax-vbdouble)                                                                            ;
;     (type typ) = 'INT                     | Ex. : 5 (= vlax-vbDouble), 2 (= vlax-vbInteger), 8 (= vlax-vbString), 9 (= vlax-vbObject), ...        ;
;   --•  lst                    > List of static type data                                                                                          ;
;     (type lst) = 'LST                     | Ex. : '(0 1 2 3 4 5 6 7 8 9), '("A" "B" "C" "D"), (-1.5 9.56 7.2 1.0), ...                            ;
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
;   The function (LM:SafearrayVariant) converts a list of static type data into a Safearray and returns a safearray variant.                        ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:SafearrayVariant ( typ lst )
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray typ (cons 0 (1- (length lst))))
      lst
    )
  )
)