
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  get-dyn-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] get-dyn-list []-----------------------[]                                       ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.1                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the complete list of dynamic properties and their values for a given block reference. The list is composed of a set of pointed pairs  ;
;   in the form (TagName . TagValue).                                                                                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-dyn-list) have 1 argument(s) :                                                                                                  ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have dynamic properties                               ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtSel" ---> Select-Filter                                 | v3.2.0 - 27/04/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-dyn-list) returns the association list of ((<tag> . <value>) ...) of the block reference, nil otherwise.                      ;
;     Ex. : (get-dyn-list (vlax-ename->vla-object (car (entsel)))) returns                                                                          ;
;             ( ("Distance1" . 42.19)                                                                                                               ;
;               ("Distance2" . 23.48)                                                                                                               ;
;               ("Visibility" . "EXE")                                                                                                              ;
;             )                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Using (vlax-get 'value) instead of (vla-get-value) to avoid the variant type                                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Modifying the return values for each dynamic property to not have variant but variant values                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-dyn-list (blk)
  (mapcar
    '(lambda (dyn)
      (cons
        (vla-get-propertyname dyn)
        (vlax-get dyn 'value)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)