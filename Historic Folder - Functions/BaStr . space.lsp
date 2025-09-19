
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  space  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] space []-----------------------[]                                           ;
;--- Date of creation       > 22/02/2021                                                                                                            ;
;--- Last modification date > 22/02/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Creates a string by multiplying n times the space character " ". Useful to creates alinea in a message displayed on screen.                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (space) have 1 argument(s) :                                                                                                         ;
;   --•  n                      > corresponds to the number of time the space character will be multiplied                                          ;
;     (type n) = 'INT                           | Ex. : 1, 5, 52, ...                                                                               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> prompt-list                                   | v1.0.0 - 22/03/2021 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtObjet" ---> C:GETLAYER                                  | v3.0.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (space) returns a string containing only space characters and the number of space depends on the number n specified in argument.   ;
;     Ex. : (space 5) returns "     "                                                                                                               ;
;           (space 0) returns ""                                                                                                                    ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun space (n / str)
  (setq str "")
  (repeat n
    (setq str (strcat str " "))
  )
  str
)