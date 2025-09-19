
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                          --{  set-dyn-FlipState  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] set-dyn-FlipState []-----------------------[]                                     ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Toggles the flip parameter if present in a supplied Dynamic Block Reference.                                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-dyn-FlipState) have 1 argument(s) :                                                                                             ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have a flip parameter                                 ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-dyn-FlipState) returns the new flip parameter value, nil otherwise.                                                           ;
;     Ex. : (set-dyn-FlipState (vlax-ename->vla-object (car (entsel)))) returns 1 or 0 if successful, nil otherwise                                 ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Using (vlax-get 'value) instead of (vla-get-value) to avoid the variant type                                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-dyn-FlipState (blk)
  (vl-some
    '(lambda (dyn / rtn)
      (if (equal '(0 1) (vlax-get dyn 'AllowedValues))
        (progn
          (vla-put-value dyn (setq rtn (- 1 (vlax-get dyn 'value))))
          rtn
        )
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)