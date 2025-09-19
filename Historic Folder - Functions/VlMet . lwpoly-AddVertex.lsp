
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                          --{  lwpoly-AddVertex  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] lwpoly-AddVertex []-----------------------[]                                      ;
;--- Date of creation       > 16/11/2023                                                                                                            ;
;--- Last modification date > 16/11/2023                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlMet"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
; Invoke the ActiveX method AddVertex for LightWeightPolylines to add a vertex (2D/3D) to a single polyline at a specific position (0 for start,    ;
; nil for end).                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (lwpoly-AddVertex) have 3 argument(s) :                                                                                              ;
;   --•  obj                    > correspond to the lwpolyline you want to add a vertex                                                             ;
;     (type obj) = 'VLA-OBJECT or 'ENAME        | Ex. : (vlax-ename->vla-object (car (entsel))), #<VLA-OBJECT IAcadLWPolyline 00000237d9aea9e8>, ...;
;   --•  pt                     > correspond to the new point you want to add to the lwpolyline                                                     ;
;     (type pt) = 'LST                          | Ex. : '(0.0 0.0 0.0), '(0.0 0.0), (getpoint "\nPoint: "), ...                                     ;
;   --•  i                      > correspond to the index for the new vertex to add. To place a vertex at the beginning, it's index 0 and nil for   ;
;                                 the end. If the index is negative then it'll consider 0, if it's nil or above the actual number of vertex, it'll  ;
;                                 consider the number at the end.                                                                                   ;
;     (type i) = 'INT                           | Ex. : 0, nil, 6, (getint "\nIndex: "), ...                                                        ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> 2D-POINT                                      | v1.0.0 - 12/08/2022 (Luna)                                                    ;
;   --•  "VlDtc" ---> CONVNAME                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (lwpoly-AddVertex) returns the updated number of vertex for the given polyline.                                                    ;
;     Ex. : (lwpoly-AddVertex (car (entsel)) (getpoint "\nNew point: ") nil) returns 8                                                              ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun lwpoly-AddVertex (obj pt i / e n s)
  (setq
    e (ConvName obj 'ENAME)
    obj (ConvName obj 'VLA-OBJECT)
    n (cdr (assoc 90 (entget e)))
    pt (2D-Point pt)
    s (vlax-make-safearray vlax-vbDouble '(0 . 1))
    i (cond ((null i) n) ((minusp i) 0) ((< i n) (fix i)) (n))
  )
  (vlax-safearray-fill s pt)
  (vla-AddVertex obj i s)
  (vla-Update obj)
  (if (= (1+ n) (cdr (assoc 90 (entget e)))) (1+ n))
)