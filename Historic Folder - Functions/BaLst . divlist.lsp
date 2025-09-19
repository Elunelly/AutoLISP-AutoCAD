
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  divlist  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] divlist []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 16/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to divide a list specified in argument in several lists of equal length (except the last one which is the remainder).                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (divlist) have 2 argument(s) :                                                                                                       ;
;   --•  lst                    > is the list you want to cut                                                                                       ;
;     (type lst) = 'LST                         | Ex. : '(0 1 2 3 4 5 6 7 8 9), (layoutlist), (entget (car (entsel))), ...                          ;
;   --•  lng                    > is the length of each division for the sub-lists                                                                  ;
;     (type lng) = 'INT                         | Ex. : nil, -5, 23, 2, ...                                                                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> sublist                                       | v1.1.0 - 24/06/2020 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> divlist                                       | v2.0.0 - 16/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (divlist) returns a list composed of sub-lists of equal length (except last one) in case of 'lng' is not null or minusp, but if    ;
;   'lng' is equal or higher than 'lst' length, it will return a list within a single sub-list. If 'lng' null or minusp, returns 'lst' directly.    ;
;     Ex. : (divlist '(0 1 2 3 4 5 6 7 8 9) 2) returns ((0 1) (2 3) (4 5) (6 7) (8 9))                                                              ;
;           (divlist '(0 1 2 3 4 5 6 7 8 9) 3) returns ((0 1 2) (3 4 5) (6 7 8) (9))                                                                ;
;           (divlist '(0 1 2 3 4 5 6 7 8 9) 15) returns ((0 1 2 3 4 5 6 7 8 9))                                                                     ;
;           (divlist '(0 1 2 3 4 5 6 7 8 9) nil) returns (0 1 2 3 4 5 6 7 8 9)                                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesign the whole program to correct some issues on (sublist) cuts and use recursion                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun divlist (lst lng)
  (cond
    ( (or
        (null lng)
        (minusp lng)
      )
      lst
    )
    ( lst
      (cons
        (sublist lst 1 lng)
        (divlist (sublist lst (1+ lng) nil) lng)
      )
    )
  )
)