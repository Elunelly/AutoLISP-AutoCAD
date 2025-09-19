
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  set-att-value  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] set-att-value []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Sets the value of the first attribute with the given tag found within the block, if present.                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-att-value) have 3 argument(s) :                                                                                                 ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have attributes                                       ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;   --•  tag                    > correspond to the name of the tag of attribute for the block reference                                            ;
;     (type tag) = 'STR                         | Ex. : "Number", "TITRE_1", ...                                                                    ;
;   --•  val                    > correspond to the new value you want to set for the specified tag                                                 ;
;     (type val) = 'STR                         | Ex. : "LT 1", "EXAMPLE OF NUMBERING", "-62.12", ...                                               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-att-value) returns the attribute value if successful, nil otherwise.                                                          ;
;     Ex. : (set-att-value (car (entsel)) "C:LINE_TYPE" "Continuous") returns "Continuous" if "C:LINE_TYPE" 'tag' is present, nil otherwise         ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-att-value (blk tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
      (if (= tag (strcase (vla-get-tagstring att)))
        (progn (vla-put-textstring att val) val)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
  )
)