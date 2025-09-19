
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  get-att-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] get-att-list []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the complete list of attribute names and their values for a given block reference. The list is composed of a set of pointed pairs in  ;
;   the form (TagName . TagValue).                                                                                                                  ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-att-list) have 1 argument(s) :                                                                                                  ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have attributes                                       ;
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
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-att-list) returns the association list of ((<tag> . <value>) ...) of the block reference, nil otherwise.                      ;
;     Ex. : (get-att-list (vlax-ename->vla-object (car (entsel)))) returns                                                                          ;
;             ( ("C:COLOR" . "144")                                                                                                                 ;
;               ("C:LAYER" . "Test2")                                                                                                               ;
;               ("C:LINE_TYPE" . "test")                                                                                                            ;
;               ("C:LINE_WEIGHT" . "211")                                                                                                           ;
;               ("C:TRANSPARENCY" . "14")                                                                                                           ;
;               ("C:MATERIAL" . "DuCalque")                                                                                                         ;
;               ("C:POSITION_X" . "-23")                                                                                                            ;
;               ("C:POSITION_Y" . "-59.154")                                                                                                        ;
;               ("C:POSITION_Z" . "2.34")                                                                                                           ;
;               ("C:SCALE_X" . "-1")                                                                                                                ;
;               ("C:SCALE_Y" . "0.43")                                                                                                              ;
;               ("C:SCALE_Z" . "0.8")                                                                                                               ;
;               ("C:ROTATION" . "-100")                                                                                                             ;
;             )                                                                                                                                     ;
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

(defun get-att-list (blk)
  (mapcar
    '(lambda (att)
      (cons
        (vla-get-tagstring att)
        (vla-get-textstring att)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
  )
)