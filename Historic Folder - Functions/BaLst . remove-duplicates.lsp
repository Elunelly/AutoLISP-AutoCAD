
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                          --{  remove-duplicates  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] remove-duplicates []-----------------------[]                                     ;
;--- Date of creation       > 18/02/2020                                                                                                            ;
;--- Last modification date > 29/04/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to delete duplicates within a list.                                                                                                  ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (remove-duplicates) have 1 argument(s) :                                                                                             ;
;   --•  lst                    > corresponds to the list we want to modify                                                                         ;
;     (type lst) = 'LST                         | Ex. : '("A" "B" "A" "C" "D" "F" "B" "D" "E" "C" "A" "F" "E"), ...                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> DXF_List                                      | v1.2.2 - 06/05/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (remove-duplicates) returns the modified list from which all duplicate values have been removed, except for the first occurrence.  ;
;   The returned list keeps the order of appearance of the values with respect to their first occurrence.                                           ;
;     Ex. : (remove-duplicates '("A" "B" "A" "C" "D" "F" "B" "D" "E" "C" "A" "F" "E")) returns ("A" "B" "C" "D" "F" "E")                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Using recursive method to remove duplicates and changing the name of the function                                              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun remove-duplicates (lst)
  (if lst
    (cons (car lst) (remove-duplicates (vl-remove (car lst) (cdr lst))))
  )
)