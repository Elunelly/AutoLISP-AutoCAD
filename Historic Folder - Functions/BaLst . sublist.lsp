
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  sublist  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] sublist []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Returns a sub-list of a list from the position of an element and a specified length. This is the equivalent function to (substr) but for a list.;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (sublist) have 3 argument(s) :                                                                                                       ;
;   --•  lst                    > corresponds to the list we want to cut                                                                            ;
;     (type lst) = 'LST                         | Ex. : '(1 2 3 4 1 3 7 5 8 1 3 6 8), '("label1" "label2" "label3" "label4"), ...                   ;
;   --•  s                      > corresponds to the starting position of the list (positive value). The first list element is index 1              ;
;     (type s) = 'INT                           | Ex. : 1, 5, 52, ...                                                                               ;
;   --•  l                      > is the length of the desired sub-list. If nil, the sub-list continues to the end of the list                      ;
;     (type l) = 'INT                           | Ex. : 2, 10, nil, ...                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> divlist                                       | v1.0.0 - 19/06/2020 (Luna)                                                    ;
;   --•  "DtObj" ---> Add-Poly2D-point                              | v2.0.0 - 10/05/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (sublist) returns the sub-list according to the input data, nil if s is greater than the length of the list.                       ;
;     Ex. : (sublist '(1 2 3 4 5 6 7 8 9) 3 5) returns '(3 4 5 6 7)                                                                                 ;
;           (sublist '(1 2 3 4 5 6 7 8 9) 3 nil) returns '(3 4 5 6 7 8 9)                                                                           ;
;           (sublist '(1 2 3 8 2 4 6 3 7 2 1 8 9) 5 5) returns '(2 4 6 3 7)                                                                         ;
;           (sublist '("label1" "label2" "label3") 4 nil) returns nil                                                                               ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Removal of the (member) and (vl-remove-if) functions, sources of problems when repeating identical elements in the list and    | ;
; |            |   removal of the redefinition of the 'start' variable to 1. Modification of the naming of variables and arguments                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun sublist (lst s l)
  (repeat (1- s) (setq lst (cdr lst)))
  (setq lst (reverse lst))
  (if
    (or
      (null l)
      (minusp l)
      (not (<= l (- (length lst) (1- s))))
    )
    lst
    (repeat (- (length lst) l)
      (setq lst (cdr lst))
    )
  )
  (reverse lst)
)