
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  SelByObj  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] SelByObj []-----------------------[]                                          ;
;--- Date of creation       > ##/##/####                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > #.#.#                                                                                                                 ;
;--- Class                  > "XxXxx"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (SelByObj) have 3 argument(s) :                                                                                                      ;
;   --•  ent                    > is the entity name of the object used as bounding box                                                             ;
;     (type ent) = 'ENAME                       | Ex. : (car (entsel)), ...                                                                         ;
;   --•  opt                    > is the (ssget) mode between "_WP" (Window Polygonal) and "_CP" (Capture Polygonal)                                ;
;     (type opt) = 'STR                         | Ex. : "_WP" or "_CP"                                                                              ;
;   --•  fltr                   > is the entity name of the object used as bounding box                                                             ;
;     (type fltr) = 'LIST                       | Ex. : (car (entsel)), ...                                                                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (SelByObj) returns [...]                                                                                                           ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun SelByObj (ent opt fltr / obj dist n lst prec dist p_lst ss)
  (vl-load-com)
  (if (= (type ent) 'ENAME)
    (setq obj (vlax-ename->vla-object ent))
    (setq
      obj ent
      ent (vlax-vla-object->ename ent)
    )
  )
  (cond
    ( (member (vla-get-ObjectName obj) '("AcDbCircle" "AcDbEllipse"))
      (setq
        dist (/ (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 50)
        n 0
      )
      (repeat 50
        (setq lst
          (cons
            (trans
              (vlax-curve-getPointAtDist obj (* dist (setq n (1+ n))))
              0
              1
            )
            lst
          )
        )
      )
    )
    ( (and
        (= (vla-get-ObjectName obj) "AcDbPolyline")
        (= (vla-get-Closed obj) :vlax-true)
      )
      (setq p_lst
        (vl-remove-if-not
          '(lambda (x)
            (or
              (= (car x) 10)
              (= (car x) 42)
            )
           )
          (entget ent)
        )
      )
      (while p_lst
        (setq lst
          (cons
            (trans
              (append
                (cdr (assoc 10 p_lst))
                (list (cdr (assoc 38 (entget ent))))
              )
              ent
              1
            )
            lst
          )
        )
        (if (/= 0 (cdadr p_lst))
          (progn
            (setq
              prec (1+ (fix (* 25 (sqrt (abs (cdadr p_lst))))))
              dist
                (/
                  (-
                    (if (cdaddr p_lst)
                      (vlax-curve-getDistAtPoint obj (trans (cdaddr p_lst) ent 0))
                      (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))
                    )
                    (vlax-curve-getDistAtPoint obj (trans (cdar p_lst) ent 0))
                  )
                  prec
                )
              n 0
            )
            (repeat (1- prec)
              (setq lst
                (cons
                  (trans
                    (vlax-curve-getPointAtDist
                      obj
                      (+
                        (vlax-curve-getDistAtPoint obj (trans (cdar p_lst) ent 0))
                        (* dist (setq n (1+ n)))
                      )
                    )
                    0
                    1
                  )
                  lst
                )
              )
            )
          )
        )
        (setq p_lst (cddr p_lst))
      )
    )
  )
  (cond
    ( lst
      (vla-ZoomExtents (vlax-get-acad-object))
      (setq ss (ssget opt lst fltr))
      (vla-ZoomPrevious (vlax-get-acad-object))
      ss
    )
  )
)