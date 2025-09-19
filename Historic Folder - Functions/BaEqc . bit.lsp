
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                 --{  bit  }--                                                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] bit []-----------------------[]                                            ;
;--- Date of creation       > 30/06/2022                                                                                                            ;
;--- Last modification date > 30/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaEqc"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Check if the result of the logical bitwise AND is equal to the lowest integer. If true, then the lowest integer is contained in the higher one  ;
;   and allow to use some bit-coded flag.                                                                                                           ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (bit) have 2 argument(s) :                                                                                                           ;
;   --•  a                      > the integer that can be contained in the other one                                                                ;
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
;   --•  "BaLst" ---> FormAssoc                                     | v1.0.0 - 30/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (bit) returns T if the result of the logical bitwise AND is equal to the lowest integer, nil otherwise.                            ;
;     Ex. : (bit 1 13) returns T                                                                                                                    ;
;           (bit 1 12) returns nil                                                                                                                  ;
;           (bit 12 8) returns nil                                                                                                                  ;
;           (bit 8 12) returns T                                                                                                                    ;
;           (bit 8 64) returns nil                                                                                                                  ;
;           (bit 24 39) returns nil                                                                                                                 ;
;           (bit 24 31) returns T                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun bit (a b)
  (= a (logand a b))
)