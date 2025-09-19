
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                        --{  get-dyn-VisibilityName  }--                                                       | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                   []-----------------------[] get-dyn-VisibilityName []-----------------------[]                                  ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Returns the name of the visibility parameter of a Dynamic Block Reference (if present).                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-dyn-VisibilityName) have 1 argument(s) :                                                                                        ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have a visibility parameter                           ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "VlObj" ---> get-dyn-VisibilityValue                       | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-VisibilityValue                       | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-dyn-VisibilityName) returns the name of visibility parameter, nil otherwise.                                                  ;
;     Ex. : (get-dyn-VisibilityName (vlax-ename->vla-object (car (entsel)))) returns "Visibility1" if present, nil otherwise                        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-dyn-VisibilityName (blk / vis)
  (setq blk (ConvName blk 'VLA-OBJECT))
  (if
    (and
      (vlax-property-available-p blk 'EffectiveName)
      (setq
        blk
          (vla-item
            (vla-get-blocks (vla-get-document blk))
            (vla-get-EffectiveName blk)
          )
      )
      (= :vlax-true (vla-get-IsDynamicBlock blk))
      (= :vlax-true (vla-get-HasExtensionDictionary blk))
      (setq
        vis
          (vl-some
            '(lambda (pair)
              (if
                (and
                  (= 360 (car pair))
                  (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                )
                (cdr pair)
              )
             )
            (dictsearch
              (ConvName (vla-getExtensionDictionary blk) 'ENAME)
              "ACAD_ENHANCEDBLOCK"
            )
          )
      )
    )
    (cdr (assoc 301 (entget vis)))
  )
)