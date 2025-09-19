
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  get-pt-list  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] get-pt-list []-----------------------[]                                        ;
;--- Date of creation       > 08/03/2019                                                                                                            ;
;--- Last modification date > 31/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "UtGeo"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to recover the list of the summits of the selected entity in the form of list of coordinates (2D or 3D according to the type of the      ;
;   object). According to the object, certain calculations are carried out to remove or add coordinates to the list.                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-pt-list) have 1 argument(s) :                                                                                                   ;
;   --•  name                   > is the entity name whose vertices are to be known                                                                 ;
;     (type name) = 'ENAME                      | Ex. : <Entity name: ab2e580>, ...                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> rp                                            | v1.0.0 - 31/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtObj" ---> Add-Poly2D-Point                              | v2.0.0 - 10/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-pt-list) returns a list of 3D coordinates depending on the object. For polylines and 3D polylines, the list contains each     ;
;   vertices of the object. For lines and splines, only the first and last vertices are returned (the spline's vertices are not physical so it's    ;
;   difficult to get them). For arcs, circles and ellipses, the list is constructed with 3 points ; the first point corresponds to the origin, the  ;
;   second one corresponds to the first vertice (= StartPoint) and the third one corresponds to the last vertice (= EndPoint). Hatch can't be used. ;
;     Ex. : (get-pt-list (car (entsel))) returns '((12.2403 1.82402 0.0) (13.6226 1.82402 0.0) (13.6226 3.44065 0.0) (12.2403 3.44065 0.0))         ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Global redesign of the function using the functions (vlax-curve-...) for a better operation. The rendering remains the same    | ;
; |            |   except that HATCH's objects are no longer supported and SPLINE's objects are now supported (partially).                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Changing the naming of variables and arguments                                                                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Modification of the function name, variables and arguments. Addition of the taking into account of particular entities (the    | ;
; |   v2.0.0   |   arcs, the lines, the polylines 2D/3D, the hatchings) and inversion of the list to obtain a list in the direction of creation   | ;
; |            |   of the object                                                                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.1   |   Declaration of local variables (partially)                                                                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Added 'E_name' argument to not force selection                                                                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-pt-list (name / rp f o s e i l)
  (defun rp (p f)
    (mapcar '(lambda (c) (if (equal c 0.0 f) 0.0 c)) p)
  )
  
  (and
    name
    (setq f 1e-15)
    (setq o (cdr (assoc 0 (entget name))))
    (member o '("LWPOLYLINE" "POLYLINE" "LINE" "SPLINE" "ARC" "CIRCLE" "ELLIPSE"))
    (setq s (vlax-curve-getStartParam name))
    (setq e (vlax-curve-getEndParam name))
    (cond
      ( (member o '("LWPOLYLINE" "POLYLINE"))
        (repeat (setq i (1+ (fix e)))
          (setq l (cons (rp (vlax-curve-getPointAtParam name (setq i (1- i))) f) l))
        )
      )
      ( (member o '("LINE" "SPLINE"))
        (setq l
          (list
            (rp (vlax-curve-getPointAtParam name s) f)
            (rp (vlax-curve-getPointAtParam name e) f)
          )
        )
      )
      ( (member o '("ARC" "CIRCLE" "ELLIPSE"))
        (setq l
          (list
            (rp (cdr (assoc 10 (entget name))) f)
            (rp (vlax-curve-getPointAtParam name s) f)
            (rp (vlax-curve-getPointAtParam name e) f)
          )
        )
      )
    )
  )
  l
)