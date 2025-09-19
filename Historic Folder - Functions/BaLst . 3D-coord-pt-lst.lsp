
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  3D-coord->pt-lst  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] 3D-coord->pt-lst []-----------------------[]                                     ;
;--- Date of creation       > ##/##/####                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > (gile)                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts the given lst of 'Coordinates or 'ControlPoints in a list of 3D coordinates.                                                           ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (3D-coord->pt-lst) have 1 argument(s) :                                                                                              ;
;   --•  lst                    > is the list of coordinates (mostly retrieved from 'Coordinates property of VLA-Object)                            ;
;     (type lst) = 'LST                         | Ex. :                                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (3D-coord->pt-lst) returns the converted list with 3 element's list for each point.                                                ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun 3D-coord->pt-lst (lst)
  (if lst (cons (list (car lst) (cadr lst) (caddr lst)) (3D-coord->pt-lst (cdddr lst))))
)