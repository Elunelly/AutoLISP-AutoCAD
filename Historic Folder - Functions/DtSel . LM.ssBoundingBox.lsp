
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  LM:ssBoundingBox  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] LM:ssBoundingBox []-----------------------[]                                     ;
;--- Date of creation       > 04/01/2017                                                                                                            ;
;--- Last modification date > 04/01/2017                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   This function will return a list of the lower-left and upper-right coordinates (in WCS) of the rectangular frame describing the bounding box of ;
;   all objects in the supplied selection set. In this context, the bounding box is defined as the smallest rectangular cuboid with sides parallel  ;
;   to the three WCS coordinate axes which encloses all objects in the set.                                                                         ;
;   Note that this bounding box does not necessarily correspond to the rectangular frame with the smallest possible area which could enclose the    ;
;   objects in the set, as the edges are always parallel to the WCS coordinate axes.                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:ssBoundingBox) have 1 argument(s) :                                                                                              ;
;   --•  sel                    > is the selection set you want to get its bounding box                                                             ;
;     (type sel) = 'PICKSET                     | Ex. : (ssget), ...                                                                                ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtSel" ---> LM:ssBoundingBoxMidPt                         | v1.0.1 - 09/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:ssBoundingBox) returns a list of the lower-left and upper-right WCS coordinates of a rectangular frame bounding all objects in ;
;   the supplied selection set.                                                                                                                     ;
;     Ex. : (LM:ssBoundingBox (ssget)) returns ((1613.52 1080.73 0.0) (2561.27 1724.41 0.0))                                                        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
  (repeat (setq idx (sslength sel))
    (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
    (if
      (and
        (vlax-method-applicable-p obj 'getboundingbox)
        (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
      )
      (setq
        ls1 (mapcar 'min (vlax-safearray->list llp) (cond (ls1) ((vlax-safearray->list llp))))
        ls2 (mapcar 'max (vlax-safearray->list urp) (cond (ls2) ((vlax-safearray->list urp))))
      )
    )
  )
  (if (and ls1 ls2) (list ls1 ls2))
)