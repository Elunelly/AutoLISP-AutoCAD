
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  PVBB-Draw  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] PVBB-Draw []-----------------------[]                                         ;
;--- Date of creation       > 04/01/2017                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > LeeMac/Luna                                                                                                           ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   For a selection set, it will construct a bounding box surrounding all objects in the selection.                                                 ;
;   If all objects in the selection are coplanar and reside in a plane parallel to the WCS plane, the following program will construct the 2D       ;
;   rectangular bounding box in the form of a 2D LWPolyline.                                                                                        ;
;   If any object in the selection resides in a different construction plane, the program will construct the 3D cuboid bounding box in the form of  ;
;   a 3D solid.                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (PVBB-Draw) have 1 argument(s) :                                                                                                     ;
;   --•  sel                    > corresponds to the selection set you want to see its bounding box                                                 ;
;     (type sel) = 'PICKSET                     | Ex. : (ssget), (ssget "_I"), ...                                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "DtSel" ---> UCSssBoundingBox                              | v1.0.0 - 07/10/2022 (LeeMac/(gile)/Luna)                                      ;
;   --•  "VlMet" ---> WCS2UCSMatrix                                 | v1.0.0 - 21/01/2007 (Douglas C. Broad, Jr.)                                   ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BbBound" ---> c:PreViewBoundingBox                        | v1.1.0 - 07/10/2022 (Luna)                                                    ;
;   --•  "BbSelct" ---> c:MID_MOVE                                  | v2.0.2 - 07/10/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (PVBB-Draw) returns a dotted pair list with the VLA-Object of the new created object representing the bounding box as the key, and ;
;   the bottom left and upper right corners coordinates as the value of the dotted pair, nil otherwise.                                             ;
;     Ex. : (PVBB-Draw (ssget)) returns (#<VLA-OBJECT IAcad3DSolid 0000024ea35f9428> (-20.7107 -20.7107 0.0) (120.711 120.711 50.0)) or nil         ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   Draw a LightweightPolyline, 3DPoly or box parallel to UCS axis, depending of which plan they're aligned to                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.3.0   |   Adapt the command example from LeeMac site (Selection set Bounding Box) into a function                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun PVBB-Draw ( sel / box obj spc )
  (if
    (and
      (setq box (UCSssBoundingBox sel))
      (setq spc
        (vlax-get-property
          (vla-get-activedocument (vlax-get-acad-object))
          (if (= 1 (getvar 'cvport))
            'paperspace
            'modelspace
          )
        )
      )
      (if (= 4 (length box))
        (if (apply '= (mapcar 'caddr box))
          (progn
            (setq obj (vlax-invoke spc 'AddLightweightPolyline (apply 'append (mapcar '(lambda (p) (reverse (cdr (reverse p)))) box))))
            (vla-put-Closed obj :vlax-true)
            (vla-put-elevation obj (caddar box))
            obj
          )
          (progn
            (setq obj (vlax-invoke spc 'Add3DPoly (apply 'append box)))
            (vla-put-Closed obj :vlax-true)
            obj
          )
        )
        (if (= 2 (length box))
          (setq obj (vla-AddBox spc (vlax-3D-point (car box)) (car (cadr box)) (cadr (cadr box)) (caddr (cadr box))))
        )
      )
      (null (vla-TransformBy obj (WCS2UCSMatrix)))
    )
    (cons obj (if (= 4 (length box)) (mapcar '(lambda (p) (trans p 1 0)) box) (list (trans (car box) 1 0) (cadr box))))
  )
)