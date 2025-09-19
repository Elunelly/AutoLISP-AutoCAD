
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                             --{  POLYADDPOINT  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] POLYADDPOINT []-----------------------[]                                       ;
;--- Date of creation       > 04/01/2022                                                                                                            ;
;--- Last modification date > 15/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.2                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 13/06/2024 (Luna)                                                    ;
;   --•  "UtDis" ---> temp-circles-make                             | v1.0.0 - 13/06/2024 (Luna)                                                    ;
;   --•  "UtDis" ---> temp-circles-delete                           | v1.1.0 - 24/06/2024 (Luna)                                                    ;
;   --•  "UtDis" ---> temp-circles-update                           | v1.0.0 - 13/06/2024 (Luna)                                                    ;
;   --•  "UtDis" ---> CrosshairColor->RGB                           | v1.0.0 - 13/06/2024 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (POLYADDPOINT) returns [...]                                                                                                        ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.2   |   Exclude locked layers from selection (with "_:L" option) to avoid errors                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Add UndoMark on (layer-get-or-create) and (temp-circles-make) + Update (temp-circles-delete) function to v1.1.0                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:POLYADDPOINT ( / *error* temp-circles-make temp-circles-delete temp-circles-update doc mode name i pt jsel n name pts)
  (defun *error* (msg)
    (temp-circles-delete)
    (setvar "CLAYER" layer)
    (command-s "_-LAYDEL" "_Name" tmp "" "_Yes")
    (vla-EndUndoMark doc)
    (princ msg)
  )
  (defun temp-circles-make (pt-list radius rgb / layer color obj lst)
    (setq
      color (vla-get-TrueColor (vla-Item (vla-get-layers doc) "0"))
      lst (vlax-ldata-get "URBASOLAR" "temp-obj")
    )
    (apply 'vla-setRGB (cons color rgb))
    (mapcar
      '(lambda (p)
        (setq obj (vla-AddCircle (vla-get-ModelSpace doc) (vlax-3D-point p) radius))
        (vla-put-TrueColor obj color)
        (vla-put-Linetype obj "Continuous")
        (setq lst (cons obj lst))
        (vlax-ldata-put "URBASOLAR" "temp-obj" lst)
      )
      pt-list
    )
    lst
  )
  (defun temp-circles-delete (/ lst)
    (setq lst (vlax-ldata-get "URBASOLAR" "temp-obj"))
    (mapcar '(lambda (x) (vl-catch-all-apply 'vla-delete (list x))) lst)
    (vlax-ldata-delete "URBASOLAR" "temp-obj")
  )
  (defun temp-circles-update (name i)
    (cond
      ( (= i -1)
        (temp-circles-make (list (vlax-curve-getStartPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
      )
      ( (null i)
        (temp-circles-make (list (vlax-curve-getEndPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
      )
      ( T
        (temp-circles-make
          (list (vlax-curve-getPointAtParam name i) (vlax-curve-getPointAtParam name (1- i)))
          (/ (getvar "VIEWSIZE") 100.)
          (CrosshairColor->RGB)
        )
      )
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq tmp "$temp$")
  (layer-get-or-create tmp (CrosshairColor->RGB) "Continuous" 0 0)
  (if (= tmp (setq layer (getvar "CLAYER")))
    (setq layer "0")
  )
  (setvar "CLAYER" tmp)
  (while (not (setq jsel (ssget "_:L" '((0 . "LWPOLYLINE")))))
    (princ (LgT "\nNo object selected..." "\nAucun objet sélectionné..." nil))
  )
  (cond
    ( (= 1 (sslength jsel))
      (setq
        name (ssname jsel 0)
        mode
          (getkdh
            (quote (getpoint msg))
            (LgT
              "\nSpecify the previous vertex or"
              "\nSpécifiez le sommet précédent ou"
              nil
            )
            (list 4 (LgT "Start End _Start End" "Début Fin _Start End" nil))
            " : "
            (LgT "End" "Fin" nil)
            nil
          )
      )
      (cond
        ( (listp mode)
          (setq pt (trans mode 1 0))
          (setq i (1+ (LM:round (vlax-curve-getparamatpoint name (vlax-curve-getclosestpointto name pt)))))
          (temp-circles-make (list (vlax-curve-getpointatparam name i)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
          (temp-circles-make (list (vlax-curve-getpointatparam name (1- i))) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
        )
        ( (= mode "Start")
          (setq i -1)
          (temp-circles-make (list (vlax-curve-getpointatparam name 0)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
        )
        ( (= mode "End")
          (setq i nil)
          (temp-circles-make (list (vlax-curve-getpointatparam name (vlax-curve-getendparam name))) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
          (if (bit 1 (cdr (assoc 70 (entget name))))
            (temp-circles-make (list (vlax-curve-getpointatparam name 1)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
          )
        )
      )
      (vla-EndUndoMark doc)
      (while
        (and
          (not (= pt "Close"))
          (setq pt
            (getkdh
              (quote (getpoint pt msg))
              (LgT "\nSpecify a new point" "\nSpécifiez un nouveau point" nil)
              (if (and (= mode "End") (not (bit 1 (cdr (assoc 70 (entget name))))))
                (list 4 (LgT "Close _Close" "Clore _Close" nil))
                nil
              )
              " : "
              nil
              nil
            )
          )
        )
        (vla-StartUndoMark doc)
        (if (= pt "Close")
          (entmod (subst (cons 70 (SwapBit 1 (cdr (assoc 70 (entget name))))) (assoc 70 (entget name)) (entget name)))
          (lwpoly-AddVertex name (trans pt 1 0) i)
        )
        (temp-circles-delete)
        (if (not (or (null i) (minusp i)))
          (setq i (1+ i))
        )
        (setvar "LASTPOINT" pt)
        (temp-circles-update name i)
        (vla-EndUndoMark doc)
      )
    )
    ( T
      (setq mode (getpoint (LgT "\nSpecify the previous vertex : " "\nSpécifiez le sommet précédent : " nil)))
      (setq mode (trans mode 1 0))
      (repeat (setq n (sslength jsel))
        (setq
          name (ssname jsel (setq n (1- n)))
          i (LM:round (vlax-curve-getParamAtPoint name (vlax-curve-getClosestPointTo name mode)))
        )
        (cond
          ( (equal i (vlax-curve-getEndParam name))
            (temp-circles-make (list (vlax-curve-getEndPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
            (setq i nil)
          )
          ( (equal i (vlax-curve-getStartParam name))
            (temp-circles-make (list (vlax-curve-getStartPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
            (setq i -1)
          )
          ( T
            (temp-circles-make
              (list (vlax-curve-getPointAtParam name i) (vlax-curve-getPointAtParam name (1+ i)))
              (/ (getvar "VIEWSIZE") 100.)
              (CrosshairColor->RGB)
            )
            (setq i (1+ i))
          )
        )
        (setq pts (cons (cons name i) pts))
      )
      (vla-EndUndoMark doc)
      (while (setq pt (getpoint (LgT "\nSpecify a new point : " "\nSpécifiez un nouveau point : " nil)))
        (vla-StartUndoMark doc)
        (setvar "LASTPOINT" pt)
        (setq pt (trans pt 1 0))
        (temp-circles-delete)
        (mapcar
          '(lambda (x / name i)
            (setq
              name (car x)
              i (cdr x)
            )
            (lwpoly-AddVertex name pt i)
            (if (not (or (null i) (minusp i)))
              (setq pts (subst (cons name (setq i (1+ i))) (assoc name pts) pts))
            )
            (temp-circles-update name i)
            x
           )
          pts
        )
        (vla-EndUndoMark doc)
      )
    )
  )
  (temp-circles-delete)
  (setvar "CLAYER" layer)
  (command "_-LAYDEL" "_Name" tmp "" "_Yes")
  (princ)
)