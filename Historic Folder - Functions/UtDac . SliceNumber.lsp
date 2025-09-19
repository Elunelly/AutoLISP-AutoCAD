
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  SliceNumber  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] SliceNumber []-----------------------[]                                        ;
;--- Date of creation       > 29/12/2021                                                                                                            ;
;--- Last modification date > 13/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   For any real number, positive or negative, returns as a list the integer part of the number separated from the decimal part. In the case of an  ;
;   integer number, its decimal part is 0 to ensure a uniform return.                                                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (SliceNumber) have 1 argument(s) :                                                                                                   ;
;   --•  n                      > corresponds to the number, integer or real, needed to be cut in two parts (integer + decimal), in string format   ;
;     (type n) = 'STR                           | Ex. : "10.2", "45836.4634", "42", "-23.0", (rtos 1.62691e+06), ...                                ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtObj" ---> GetAnyProperty                                | v2.0.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "DtObj" ---> SetAnyProperty                                | v2.0.0 - 05/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (SliceNumber) returns a cons-list with the first value representing the integer part of the number and the second value            ;
;   representing the decimal part of the number. Each part is an integer.                                                                           ;
;     Ex. : (SliceNumber "10.2") returns (10 . 2)                                                                                                   ;
;           (SliceNumber "10") returns (10 . 0)                                                                                                     ;
;           (SliceNumber "-42.23") returns (-42 . 23)                                                                                               ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Change the 'n' argument type, to enable the use of the function with scientific writing                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun SliceNumber (n / p)
  (if (distof n)
    (if (setq p (vl-string-search "." n))
      (cons
        (atoi (substr n 1 p))
        (atoi (substr n (+ p 2)))
      )
      (cons (atoi n) 0)
    )
  )
)