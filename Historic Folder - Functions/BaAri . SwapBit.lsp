
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  SwapBit  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] SwapBit []-----------------------[]                                          ;
;--- Date of creation       > 24/06/2022                                                                                                            ;
;--- Last modification date > 24/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Swap the value of a bit-coded integer with another value                                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (SwapBit) have 2 argument(s) :                                                                                                       ;
;   --•  a                      > any bit-coded integer                                                                                             ;
;     (type a) = 'INT                           | Ex. : 1, 251, 64, 15, ...                                                                         ;
;   --•  b                      > any bit-coded integer                                                                                             ;
;     (type b) = 'INT                           | Ex. : 1, 251, 64, 15, ...                                                                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (SwapBit) returns the result of the logical bitwise inclusive OR of two integers minus the logical bitwise AND of the 2 integers.  ;
;     Ex. : (SwapBit 123 1) returns 122                                                                                                             ;
;           (SwapBit 1 123) returns 122                                                                                                             ;
;           (SwapBit 1 122) returns 123                                                                                                             ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun SwapBit (a b)
  (- (logior b a) (logand b a))
)