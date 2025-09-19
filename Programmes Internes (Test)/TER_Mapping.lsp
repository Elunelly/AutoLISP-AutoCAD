(defun c:GENSQUAREMAP (/)
 (setq
   fromPt (getpoint "\nPlease enter the top-left corner's coordinates of the map: ")
   nbTilesX (getkdh (quote (getint msg)) "\nPlease enter the number of tile in X" '(6) ": " 30 nil)
   nbTilesY (getkdh (quote (getint msg)) "\nPlease enter the number of tile in Y" '(6) ": " 30 nil)
   dimTile (getkdh (quote (getreal msg)) "\nPlease enter the dimension of a tile" '(6) ": " 10 nil)
   row 0 col 0 nextPt fromPt
   doc (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (while (< row nbTilesY)
    (while (< col nbTilesX)
      (setq plst
        (list
          nextPt
          (list (+ dimTile (car nextPt)) (cadr nextPt))
          (list (+ dimTile (car nextPt)) (- (cadr nextPt) dimTile))
          (list (car nextPt) (- (cadr nextPt) dimTile))
        )
      )
      (setq ent
        (vlax-invoke
          (vla-get-ModelSpace doc)
          'addLightWeightPolyline
          (apply
            'append
            (mapcar
              '(lambda (p) (list (car p) (cadr p)))
              plst
            )
          )
        )
      )
      (vlax-put ent 'Closed 1)
      (setq
        col (1+ col)
        nextPt (polar nextPt 0 dimTile)
      )
    )
    (setq
      row (1+ row)
      col 0
      nextPt (polar fromPt (- (/ pi 2.)) (* dimTile row))
    )
  )
)

(defun polygonCircumscribing (centerPt innerCircleRadius nbVertex / angleBetweenAxe angleMidAxe angleStart dimensionAxe i polygonPts)
  (setq
    angleBetweenAxe (/ (* pi 2) nbVertex)
    angleMidAxe (/ angleBetweenAxe 2.)
    angleStart (+ angleMidAxe (/ pi 2.))
    dimensionAxe (/ innerCircleRadius (cos angleMidAxe))
  )
  (repeat (setq i nbVertex)
    (setq polygonPts (cons (polar centerPt (+ (* (setq i (1- i)) angleBetweenAxe) angleStart) dimensionAxe) polygonPts))
  )
  polygonPts
)

(defun polygonMap (fromPt innerCircleRadius nbVertex nbTilesX nbTilesY / doc fromPt centerPt row col anglePolygon polygonPts polygon)
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    fromPt (list (+ (car fromPt) innerCircleRadius) (- (cadr fromPt) innerCircleRadius))
    anglePolygon (- (/ pi 2.) (/ (* pi 2) nbVertex))
  )
  (repeat (setq row nbtilesY)
    (setq
      row (1- row)
      centerPt
        (polar
          fromPt
          (/ pi -2.)
          (* 2 innerCircleRadius row)
        )
    )
    (repeat (setq col nbTilesX)
      (setq
        col (1- col)
        polygonPts
          (polygonCircumscribing
            (setq centerPt
              (polar
                centerPt
                (if (= 1 (rem col 2)) anglePolygon (- anglePolygon))
                (* 2 innerCircleRadius)
              )
            )
            innerCircleRadius
            nbVertex
          )
        polygon
          (vlax-invoke
            (vla-get-ModelSpace doc)
            'addLightWeightPolyline
            (apply
              'append
              (mapcar
                '(lambda (p) (list (car p) (cadr p)))
                polygonPts
              )
            )
          )
      )
      (vlax-put polygon 'Closed 1)
    )
  )
)

(defun c:GENMAP (/)
  (setq
    fromPt (getpoint "\nPlease enter the top-left corner's coordinates of the map: ")
    nbTilesX (getkdh (quote (getint msg)) "\nPlease enter the number of tile in X" '(6) ": " 40 nil)
    nbTilesY (getkdh (quote (getint msg)) "\nPlease enter the number of tile in Y" '(6) ": " 30 nil)
    dimTile (getkdh (quote (getdist msg)) "\nPlease enter the radius of the inscribed circle of a tile" '(6) ": " 10.0 nil)
    nbVertex (getkdh (quote (getint msg)) "\nPlease enter the number of vertex for the tiles" '(6) ": " 4 nil)
  )
  (polygonMap fromPt dimTile nbVertex nbTilesX nbTilesY)
  (princ)
)