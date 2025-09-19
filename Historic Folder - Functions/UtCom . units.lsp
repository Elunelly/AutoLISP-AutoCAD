
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  units  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] units []-----------------------[]                                           ;
;--- Date of creation       > 23/06/2022                                                                                                            ;
;--- Last modification date > 23/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts the value of "INSUNITS" in a string representing this unit.                                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (units) have 0 argument(s) :                                                                                                         ;
;   --•  u                      > value of "INSUNITS" or a forced value corresponding to the possibles values of "INSUNITS"                         ;
;     (type u) = 'INT                           | Ex. : (getvar "INSUNITS"), 6, 4, ...                                                              ;
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
;   The function (units) returns the string representing the unit of "INSUNITS"                                                                     ;
;     Ex. : (units 6) returns "m" (for meters)                                                                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun units (u)
  (cond
    ((= u 1)  "in")
    ((= u 2)  "ft")
    ((= u 3)  "mi")
    ((= u 4)  "mm")
    ((= u 5)  "cm")
    ((= u 6)  "m")
    ((= u 7)  "km")
    ((= u 8)  "µin")
    ((= u 9)  "min")
    ((= u 10) "yd")
    ((= u 11) "Å")
    ((= u 12) "nm")
    ((= u 13) "µm")
    ((= u 14) "dm")
    ((= u 15) "dam")
    ((= u 16) "hm")
    ((= u 17) "Gm")
  )
)