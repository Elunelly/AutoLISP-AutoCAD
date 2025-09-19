
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  set-dyn-value  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] set-dyn-value []-----------------------[]                                       ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.1.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Sets the value of the first dynamic property with the given tag found within the block, if present.                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-dyn-value) have 3 argument(s) :                                                                                                 ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have dynamic properties                               ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;   --•  tag                    > correspond to the name of the tag of dynamic property for the block reference                                     ;
;     (type tag) = 'STR                         | Ex. : "Number", "TITRE_1", ...                                                                    ;
;   --•  val                    > correspond to the new value you want to set for the specified tag                                                 ;
;     (type val) = '...                         | Ex. : 23.84, "EXE", 0, ...                                                                        ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;   --•  "VlDtc" ---> get-dyn-AllowedValues                         | v3.0.0 - 30/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "VlObj" ---> set-dyn-list                                  | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-VisibilityValue                       | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-dyn-value) returns the dynamic property value if successful, nil otherwise.                                                   ;
;     Ex. : (set-dyn-value (car (entsel)) "Distance1" 23.86) returns 23.86 if "Distance1" 'tag' is present, nil otherwise                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Adding the (get-dyn-AllowedValues) function to be sure if the specified value can be set or not                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-dyn-value (blk tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (dyn / avl)
      (if
        (and
          (= tag (strcase (vla-get-propertyname dyn)))
          (cond ((setq avl (get-dyn-AllowedValues blk tag)) (member val avl)) ((not avl)))
          (null (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-value (list dyn val))))
        )
        val
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)