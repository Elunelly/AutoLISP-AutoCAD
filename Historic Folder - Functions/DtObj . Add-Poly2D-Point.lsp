
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  Add-Poly2D-Point  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] Add-Poly2D-Point []-----------------------[]                                     ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 10/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to add a point for polylines ("LWPOLYLINE") only from the previous point, following the polyline direction. Using a maximum distance of  ;
;   1E2 (= 100 units) to consider the distance between the specified point and the closest point to it.                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Add-Poly2D-Point) have 3 argument(s) :                                                                                              ;
;   --•  name                   > is the entity name of the polyline to be modified                                                                 ;
;     (type name) = 'ENAME                      | Ex. : <Entity name: ab2e580>, (car (entsel)), ...                                                 ;
;   --•  Start-pt               > corresponds to the starting point, preceding the point you wish to add. It may not belong to the list of vertices ;
;                               of the polyline because the function (osnap-poly) is assigned to it in order to select the vertex belonging to the  ;
;                               polyline being closest to this point.                                                                               ;
;     (type Start-pt) = 'LST                    | Ex. : '(12.45 83.14 0.0), (getpoint), ...                                                         ;
;   --•  Add-pt                 > corresponds to the point we want to add to the list of vertices of the polyline                                   ;
;     (type Add-pt) = 'LST                      | Ex. : '(19.20 78.95 0.0), (getpoint), ...                                                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> sublist                                       | v1.1.0 - 24/06/2020 (Luna)                                                    ;
;   --•  "UtGeo" ---> get-pt-list                                   | v3.0.0 - 31/12/2021 (Luna)                                                    ;
;   --•  "UtGeo" ---> osnap-poly                                    | v3.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Add-Poly2D-Point) returns the DXF list of the modified polyline, nil otherwise.                                                   ;
;     Ex. : (Add-Poly2D-Point <Entity name: ab2e5c0> '(12.4372 1.52465 0.0) '(13.4171 1.0542 0.0)) returns T if sucessful, nil otherwise            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Updated version of (osnap-poly) function to 3.0.0 and return only T if successful, nil otherwise                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Add-Poly2D-Point (name Start-pt Add-pt / entlist pt-list pos add)
  (and
    (= (cdr (assoc 0 (entget name))) "LWPOLYLINE")
    (setq entlist (entget name))
    (setq pt-list (get-pt-list name))
    (setq Start-pt (osnap-poly pt-list Start-pt 1E2))
    (setq pos (+ 5 (- (length entlist) (length (member Start-pt (mapcar 'cdr entlist))))))
    (setq add  
      (list  
        (assoc 40 (sublist entlist (- pos 4) nil))
        (assoc 41 (sublist entlist (- pos 4) nil))
        (assoc 42 (sublist entlist (- pos 4) nil))
        (assoc 91 (sublist entlist (- pos 4) nil))
      )
    )
    (setq entlist
      (entmod
        (append
          (sublist entlist 1 pos)
          (append
            (list
              (cons
                10
                (if (/= 2 (length Add-pt))
                  (list (car Add-pt) (cadr Add-pt))
                  Add-pt
                )
              )
            )
            add
          )
          (sublist entlist (1+ pos) nil)
        )
      )
    )
  )
)