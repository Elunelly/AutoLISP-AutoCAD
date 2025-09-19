
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                        --{  LM:ssBoundingBoxMidPt  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                   []-----------------------[] LM:ssBoundingBoxMidPt []-----------------------[]                                   ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 09/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   This function will calculate the lower-left and upper-right coordinates (in WCS) of the rectangular frame describing the bounding box of all    ;
;   objects in the supplied selection set and returns the center of this frame. In this context, the bounding box is defined as the smallest        ;
;   rectangular cuboid with sides parallel to the three WCS coordinate axes which encloses all objects in the set.                                  ;
;   Note that this bounding box does not necessarily correspond to the rectangular frame with the smallest possible area which could enclose the    ;
;   objects in the set, as the edges are always parallel to the WCS coordinate axes.                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:ssBoundingBoxMidPt) have 1 argument(s) :                                                                                         ;
;   --•  sel                    > is the selection set you want to get its bounding box                                                             ;
;     (type sel) = 'PICKSET                     | Ex. : (ssget), ...                                                                                ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "DtSel" ---> LM:ssBoundingBox                              | v1.2.0 - 04/01/2017 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BbSelct" ---> c:MID_MOVE                                  | v2.0.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:ssBoundingBoxMidPt) returns the WCS coordinates of the center of rectangular frame bounding all objects in the supplied        ;
;   selection set.                                                                                                                                  ;
;     Ex. : (LM:ssBoundingBoxMidPt (ssget)) returns (2087.39 1402.57 0.0)                                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Adding variables 'Pt1 and 'Pt2 to avoid the use of (car) and (cadr) repeatedly                                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:ssboundingboxMidPt ( sel / Pts Pt1 Pt2 )
  (if
    (and
      (setq Pts (LM:ssBoundingBox sel))
      (setq Pt1 (car Pts))
      (setq Pt2 (cadr Pts))
    )
    (trans (polar Pt1 (angle Pt1 Pt2) (/ (distance Pt1 Pt2) 2.0)) 0 1)
  )
)