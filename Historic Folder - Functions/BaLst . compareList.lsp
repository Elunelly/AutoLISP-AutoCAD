
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  compareList  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] compareList []-----------------------[]                                        ;
;--- Date of creation       > 29/11/2024                                                                                                            ;
;--- Last modification date > 29/11/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to divide a list specified in argument in several lists of equal length (except the last one which is the remainder).                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (compareList) have 2 argument(s) :                                                                                                       ;
;   --•  lst                    > is the list you want to cut                                                                                       ;
;     (type lst) = 'LST                         | Ex. : '(0 1 2 3 4 5 6 7 8 9), (layoutlist), (entget (car (entsel))), ...                          ;
;   --•  lng                    > is the length of each division for the sub-lists                                                                  ;
;     (type lng) = 'INT                         | Ex. : nil, -5, 23, 2, ...                                                                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> sublist                                       | v1.1.0 - 24/06/2020 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> compareList                                       | v2.0.0 - 16/06/2022 (Luna)                                                ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (compareList) returns a list composed of sub-lists of equal length (except last one) in case of 'lng' is not null or minusp, but if;
;   'lng' is equal or higher than 'lst' length, it will return a list within a single sub-list. If 'lng' null or minusp, returns 'lst' directly.    ;
;     Ex. : (compareList '(0 1 2 3 4 5 6 7 8 9) 2) returns ((0 1) (2 3) (4 5) (6 7) (8 9))                                                          ;
;           (compareList '(0 1 2 3 4 5 6 7 8 9) 3) returns ((0 1 2) (3 4 5) (6 7 8) (9))                                                            ;
;           (compareList '(0 1 2 3 4 5 6 7 8 9) 15) returns ((0 1 2 3 4 5 6 7 8 9))                                                                 ;
;           (compareList '(0 1 2 3 4 5 6 7 8 9) nil) returns (0 1 2 3 4 5 6 7 8 9)                                                                  ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun compareList (lst1 lst2 / rslt)
  (setq rslt '())
  (while (and lst1 lst2)
    (setq x1 (car lst1) x2 (car lst2))
    (if (not (equal x1 x2))
      (progn
        (setq rslt (cons (list x1 x2) rslt))
        (princ
          (strcat
            "\n"
            (vl-princ-to-string x1)
            " -> "
            (vl-princ-to-string x2)
          )
        )
      )
    )
    (setq lst1 (cdr lst1) lst2 (cdr lst2))
  )
  (princ "\n")
  rslt
)