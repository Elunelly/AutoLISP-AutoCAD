
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  LM:DO-MoveAbove  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] LM:DO-MoveAbove []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Moves a set of objects above a supplied object in the draw order                                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:DO-MoveAbove) have 2 argument(s) :                                                                                               ;
;   --•  obs                    > selection set or list of objects with same owner                                                                  ;
;     (type obs) = 'LST or PICKSET              | Ex. : (ssget), (list (entlast)), ...                                                              ;
;   --•  obj                    > object above which to move supplied objects                                                                       ;
;     (type obj) = 'VLA-OBJECT                  | Ex. : (vlax-ename->VLA-Object (car (entsel))), ...                                                ;
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
;   The function (LM:DO-MoveAbove) returns T if sucessful, nil otherwise.                                                                           ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Modify the function's name to add "DO" (Draw Order) to group up all related functions                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:DO-MoveAbove ( obs obj / tab )
  (if
    (and
      (or
        (listp obs)
        (setq obs (LM:ss->vla obs))
      )
      (setq tab (LM:sortentstable (LM:getowner (car obs))))
    )
    (not (vla-moveabove tab (LM:safearrayvariant vlax-vbobject obs) obj))
  )
)