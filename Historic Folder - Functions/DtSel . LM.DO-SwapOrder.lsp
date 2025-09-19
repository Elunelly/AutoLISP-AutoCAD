
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  LM:DO-SwapOrder  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] LM:DO-SwapOrder []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Moves a set of objects above a supplied object in the draw order                                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:DO-SwapOrder) have 2 argument(s) :                                                                                               ;
;   --•  ob1                    > object to swap its draw order with the second one                                                                 ;
;     (type ob1) = 'VLA-OBJECT                  | Ex. : (vlax-ename->VLA-Object (car (entsel))), ...                                                ;
;   --•  ob2                    > object to swap its draw order with the first one                                                                  ;
;     (type ob2) = 'VLA-OBJECT                  | Ex. : (vlax-ename->VLA-Object (car (entsel))), ...                                                ;
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
;   The function (LM:DO-SwapOrder) returns T if sucessful, nil otherwise.                                                                           ;
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

(defun LM:DO-SwapOrder ( ob1 ob2 / tab )
  (if (setq tab (LM:sortentstable (LM:getowner ob1)))
    (not (vla-swaporder tab ob1 ob2))
  )
)