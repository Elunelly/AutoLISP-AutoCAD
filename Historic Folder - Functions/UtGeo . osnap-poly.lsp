
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  osnap-poly  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] osnap-poly []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "UtGeo"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the coordinates of the vertex of an entity closest to a point specified on the screen provided that it respects the minimum distance  ;
;   entered as an argument, otherwise returns nil. The list of vertices is retrieved via the (get-pt-list) function, works with any type of entity  ;
;   returning a list of real points via (get-pt-list) but its real use is limited to polylines.                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (osnap-poly) have 3 argument(s) :                                                                                                    ;
;   --•  pt-list                > is the list of points of a polyline object                                                                        ;
;     (type pt-list) = 'LST                     | Ex. : (get-pt-list (car (entsel))), ...                                                           ;
;   --•  pt                     > corresponds to the point of comparison with the vertices of the entity                                            ;
;     (type pt) = 'LST                          | Ex. : '(54.15 84.62 -0.2), (getpoint), ...                                                        ;
;   --•  fuzz                   > corresponds to the minimum distance to search for a connection point                                              ;
;     (type fuzz) = 'REAL                       | Ex. : 0.001, 15.52, nil, ...                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtObj" ---> Add-Poly2D-Point                              | v2.0.0 - 10/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (osnap-poly) returns the coordinates of the vertex of the entity closest to the point of comparison, nil otherwise.                ;
;     Ex. : (osnap-poly (get-pt-list (car (entsel))) '(54.15 84.62 -0.2) 100) returns (32.14 76.67 0.0)                                             ;
;           (osnap-poly (get-pt-list (car (entsel))) '(54.15 84.62 -0.2) 0.1) returns nil                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Replace 'name' argument with 'pt-list' to remove dependency on the function (get-pt-list)                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Addition of the fuzz argument to check the concordance between a point issued from (getpoint) and a point issued from the      | ;
; |            |   (get-pt-list) function having differences in coordinates although it is an identical point (cf. function (trans))              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun osnap-poly (pt-list pt fuzz / dist p)
  (setq pt (trans pt 1 0))
  (foreach p pt-list
    (setq dist (cons (cons (distance p pt) p) dist))
  )
  (setq pt (assoc (apply 'min (mapcar 'car dist)) dist))
  (if
    (or
      (null fuzz)
      (<= (car pt) fuzz)
    )
    (cdr pt)
  )
)