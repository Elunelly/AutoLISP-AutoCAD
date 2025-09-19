
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  get-DXF-value  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] get-DXF-value []-----------------------[]                                       ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 05/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the value of the n-th DXF code within a list of pointed pairs. Created to more easily explore DXF lists of AutoCAD entities, this     ;
;   function works with any type of list composed of pointed pairs.                                                                                 ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-DXF-value) have 3 argument(s) :                                                                                                 ;
;   --•  entlist                > corresponds to the list of pointed pairs (mainly DXF list)                                                        ;
;     (type entlist) = 'LST                     | Ex. : (entget (car (entsel))), '((-1 . <Entity name: 3b240de0>) ... (210 0.0 0.0 1.0)), ...       ;
;   --•  code                   > corresponds to the key searched in the list of pointed pairs                                                      ;
;     (type code) = '...                        | Ex. : 330, 10, "A", '(0.0 0.0 0.0), ...                                                           ;
;   --•  n                      > corresponds to the position of the searched key. The first key is at position 0, and if this key is repeated in   ;
;                               the list, then the second occurrence of the key corresponds to position 1, etc                                      ;
;     (type n) = 'INT                           | Ex. : 0, 1, 3, 15, ...                                                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> get-DXF-List                                  | v1.0.1 - 23/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-DXF-value) returns the value associated with the DXF code searched according to its position, nil if the search failed.       ;
;     Ex. : (get-DXF-value (entget (car (entsel))) 330 0) returns <Entity name: 3b23e9f0>                                                           ;
;           (get-DXF-value (entget (car (entsel))) 330 1) returns nil                                                                               ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Re-design of the function with recursive version and first position of the key to 0 instead of 1                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-DXF-value (entlist key pos)
  (if (and entlist (< 0 pos))
    (get-DXF-value (cdr (member (assoc key entlist) entlist)) key (1- pos))
    (cdr (assoc key entlist))
  )
)