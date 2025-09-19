
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  get-dyn-value  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] get-dyn-value []-----------------------[]                                       ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the dynamic property value of a named tag if found, nil otherwise.                                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-dyn-value) have 2 argument(s) :                                                                                                 ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have dynamic properties                               ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;   --•  tag                    > correspond to the name of the tag of dynamic property for the block reference                                     ;
;     (type tag) = 'STR                         | Ex. : "Distance1", "Visibility", ...                                                              ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "VlObj" ---> get-dyn-VisibilityValue                       | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-dyn-value) returns the dynamic property value of the block reference if 'tag' is found, nil otherwise.                        ;
;     Ex. : (get-dyn-value (car (entsel)) "Distance1") returns 23.86                                                                                ;
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

(defun get-dyn-value (blk tag)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (dyn)
      (if (= tag (strcase (vla-get-propertyname dyn)))
        (vlax-get dyn 'value)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)