
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  set-layouts-pos  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] set-layouts-pos []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the position of each layout tab in an association list format like ((<LayoutName1> . <TabOrder1>) ...).                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-layouts-pos) have 1 argument(s) :                                                                                               ;
;   --•  f                      > is a sorting function quoted to be apply on the (layoutlist) or directly the list of layouts                      ;
;     (type f) = 'SUBR or 'LST                  | Ex. : nil, '<, '>, '(lambda (x) ...), ... or (layoutlist), '("layout2" "layout1" "layout3"), ...  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-layouts-pos) returns the list of layouts rearranged in the correct order.                                                     ;
;     Ex. : (set-layouts-pos '<) returns ("layout1" "Layout1 (2)" "Layout2")                                                                        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Changing the name of function, variables and arguments and add the 'f' argument to choose the sorting function or the list     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-layouts-pos (f / i tab ll l)
  (setq
    i 0
    tab (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
    ll (cond ((null f) (layoutlist)) ((listp f) f) (f (vl-sort (layoutlist) f)))
  )
  (foreach l ll
    (vla-put-taborder (vla-item tab l) (setq i (1+ i)))
  )
  ll
)