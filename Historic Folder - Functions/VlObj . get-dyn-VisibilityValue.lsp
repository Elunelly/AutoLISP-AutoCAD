
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  get-dyn-VisibilityValue  }--                                                       | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] get-dyn-VisibilityValue []-----------------------[]                                  ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Returns the name of the visibility parameter of a Dynamic Block Reference (if present).                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-dyn-VisibilityValue) have 1 argument(s) :                                                                                       ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have a visibility parameter                           ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlObj" ---> get-dyn-value                                 | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-VisibilityName                        | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-dyn-VisibilityValue) returns the value of visibility parameter, nil otherwise.                                                ;
;     Ex. : (get-dyn-VisibilityValue (vlax-ename->vla-object (car (entsel)))) returns "TPC Ø160" if present, nil otherwise                          ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Affecting the function's name (get-dyn-value) and (get-dyn-VisibilityName) from my own library                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-dyn-VisibilityValue (blk / vis)
  (if (setq vis (get-dyn-VisibilityName blk))
    (get-dyn-value blk vis)
  )
)