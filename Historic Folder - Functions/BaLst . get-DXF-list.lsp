
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  get-DXF-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] get-DXF-list []-----------------------[]                                       ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 23/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the list of all values associated with a key within a list of pointed pairs. Created to explore more easily the DXF lists of AutoCAD  ;
;   entities, this function works with any type of pointed pair list.                                                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-DXF-list) have 2 argument(s) :                                                                                                  ;
;   --•  entlist                > corresponds to the list of pointed pairs (mainly DXF list)                                                        ;
;     (type entlist) = 'LST                     | Ex. : (entget (car (entsel))), '((-1 . <Entity name: 3b240de0>) ... (210 0.0 0.0 1.0)), ...       ;
;   --•  code                   > corresponds to the key searched in the list of pointed pairs                                                      ;
;     (type code) = '...                        | Ex. : 330, 10, "A", '(0.0 0.0 0.0), ...                                                           ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> get-DXF-value                                 | v1.1.0 - 05/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-DXF-list) returns the set of values associated to the searched key in the form of a list, in the order of appearance of the   ;
;   values in the list.                                                                                                                             ;
;     Ex. : (get-DXF-list (entget (car (entsel))) 10) returns ((-65.7146 68.9704) (-61.0142 73.3744))                                               ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Update (get-DXF-value) function to v1.1.0                                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-DXF-list (entlist code / n value lst)
  (setq n -1)
  (while (setq value (get-DXF-value entlist code (setq n (1+ n))))
    (setq lst (cons value lst))
  )
  (reverse lst)
)