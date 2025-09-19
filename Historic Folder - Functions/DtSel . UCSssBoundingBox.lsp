
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  UCSssBoundingBox  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] UCSssBoundingBox []-----------------------[]                                     ;
;--- Date of creation       > 07/10/2022                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > LeeMac/(gile)/Luna                                                                                                    ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   This function will return a list of the lower-left and upper-right coordinates (in WCS) of the rectangular frame describing the bounding box of ;
;   all objects in the supplied selection set. In this context, the bounding box is defined as the smallest rectangular cuboid with sides parallel  ;
;   to the three WCS coordinate axes which encloses all objects in the set.                                                                         ;
;   Note that this bounding box does not necessarily correspond to the rectangular frame with the smallest possible area which could enclose the    ;
;   objects in the set, as the edges are always parallel to the WCS coordinate axes.                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (UCSssBoundingBox) have 1 argument(s) :                                                                                              ;
;   --•  sel                    > is the selection set you want to get its bounding box                                                             ;
;     (type sel) = 'PICKSET                     | Ex. : (ssget), ...                                                                                ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlMet" ---> UCS2WCSMatrix                                 | v1.0.0 - 21/01/2007 (Douglas C. Broad, Jr.)                                   ;
;   --•  "VlMet" ---> WCS2UCSMatrix                                 | v1.0.0 - 21/01/2007 (Douglas C. Broad, Jr.)                                   ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (UCSssBoundingBox) returns a list of the lower-left and upper-right WCS coordinates of a rectangular frame bounding all objects in ;
;   the supplied selection set.                                                                                                                     ;
;     Ex. : (UCSssBoundingBox (ssget)) returns ((1613.52 1080.73 0.0) (2561.27 1724.41 0.0))                                                        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun UCSssBoundingBox (sel / i obj sa1 sa2 pt1 pt2 lst)
  (repeat (setq i (sslength sel))
    (setq obj (vlax-ename->vla-object (ssname sel (setq i (1- i)))))
    (vla-TransformBy obj (UCS2WCSMatrix))
    (if
      (and
        (vlax-method-applicable-p obj 'getboundingbox)
        (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'sa1 'sa2))))
      )
      (setq
        pt1 (mapcar 'min (vlax-safearray->list sa1) (cond (pt1) ((vlax-safearray->list sa1))))
        pt2 (mapcar 'max (vlax-safearray->list sa2) (cond (pt2) ((vlax-safearray->list sa2))))
      )
    )
    (vla-TransformBy obj (WCS2UCSMatrix))
  )
  (setq lst
    (cond
      ( (equal (car pt1) (car pt2) 1e-007)
        (list
          pt1
          (list (car pt1) (cadr pt1) (caddr pt2))
          pt2
          (list (car pt1) (cadr pt2) (caddr pt1))
        )
      )
      ( (equal (cadr pt1) (cadr pt2) 1e-007)
        (list
          pt1
          (list (car pt1) (cadr pt1) (caddr pt2))
          pt2
          (list (car pt2) (cadr pt1) (caddr pt1))
        )
      )
      ( (equal (caddr pt1) (caddr pt2) 1e-007)
        (list
          pt1
          (list (car pt1) (cadr pt2) (caddr pt1))
          pt2
          (list (car pt2) (cadr pt1) (caddr pt1))
        )
      )
      ( (and pt1 pt2)
        (list
          (mapcar '(lambda (x y) (/ (+ x y) 2.)) pt1 pt2)
          (mapcar '- pt2 pt1)
        )
      )
    )
  )
)