
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  SIMPLAREA  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] SIMPLAREA []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

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
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (SIMPLAREA) returns [...]                                                                                                           ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:SIMPLAREA (/ Spa-princ-param doc spc layer llock osm coAdd coSup filter break mode jsel i name lst)
  (defun Spa-princ-param (lst flag / u sel cof arl add sub n)
    (setq
      u (units (getvar "INSUNITS"))
      sel (car lst)
      cof (car sel)
      arl (mapcar 'cadr (cdr sel))
      add (vl-remove-if-not '(lambda (x) (= +1 (car x))) lst)
      sub (vl-remove-if-not '(lambda (x) (= -1 (car x))) lst)
      add (apply 'append (mapcar 'cdr add))
      sub (apply 'append (mapcar 'cdr sub))
      add (* +1 (mapcar 'cadr add))
      sub (* -1 (mapcar 'cadr sub))
      n (length lst)
    )
    (princ (LgT (strcat "\nSelection n°" (itoa n) ":") (strcat "\nSélection n°" (itoa n) ":") nil))
    (if (bit 1 flag)
      (princ
        (strcat
          (cdr (assoc 1 param))
          (ThsdSpace (rtos (* cof (apply '+ arl)) 2 2) " ") u "²"
        )
      )
    )
    (if (bit 3 flag) (princ ",  "))
    (if (bit 2 flag)
      (princ
        (strcat
          (cdr (assoc 2 param))
          (itoa (length (cdr sel))) "u"
        )
      )
    )
    (if (bit 4 flag)
      (princ
        (strcat
          (cdr (assoc 4 param))
          (ThsdSpace (rtos (+ (apply '+ add) (apply '+ sub)) 2 2) " ") u "²"
        )
      )
    )
    (if (bit 12 flag) (princ ",  "))
    (if (bit 8 flag)
      (princ
        (strcat
          (cdr (assoc 8 param))
          (itoa (apply '+ (mapcar '(lambda (x) (length (cdr x))) lst))) "u"
        )
      )
    )
    (if (bit 16 flag)
      (princ
        (strcat
          (cdr (assoc 16 param))
          (lst2str arl (strcat u "²,  "))
        )
      )
    )
    (princ)
  )
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    spc (vla-get-ModelSpace doc)
    layer (vla-item (vla-get-Layers doc) (getvar "CLAYER"))
    llock (vla-get-Lock layer)
    osm (getvar "OSMODE")
    coAdd (apply 'vla-setRGB (cons (vla-get-TrueColor layer) '(0 255 0)))
    coSup (apply 'vla-setRGB (cons (vla-get-TrueColor layer) '(255 0 0)))
    filter '((0 . "ARC,CIRCLE,HATCH,LWPOLYLINE,SPLINE"))
    param
      (list
        (cons 1   (LgT "\nArea = " "\nAire = " nil))                      ; (bit 0) → Displays total area of last selection set from 'lst (car lst)
        (cons 2   (LgT "Object's number = " "Nombre d'objets = " nil))    ; (bit 1) → Displays the number of objects for last selection set (car lst)
        (cons 4   (LgT "\nTotal area = " "\nAire totale = " nil))         ; (bit 2) → Displays total area of 'lst
        (cons 8   (LgT "Total number = " "Nombre total = " nil))          ; (bit 3) → Displays the total number of objects
        (cons 16  (LgT "\nArea's listing :" "\nListe des aires :" nil))   ; (bit 4) → Details areas of last selection set from 'lst (car lst)
      )
  )
  (vla-put-Lock layer :vlax-false)
  (vla-put-LayerOn layer :vlax-true)
  (while (not break)
    (setq mode
      (getkdh
        (quote (getpoint msg))
        (LgT
          "\nSpecify first corner point or"
          "\nSpécifiez le premier coin ou"
          nil
        )
        (list
          (LgT
            "Object Add Substract eXit _Object Add Substract eXit"
            "Objet Ajouter Soustraire Quitter _Object Add Substract eXit"
            nil
          )
        )
        " : "
        "Object"
        nil
      )
    )
    (cond
      ( (listp mode)
;-;        (setq
;-;          name (vla-addHatch spc acHatchPatternTypePredefined "SOLID" :vlax-false acHatchObject)
;-;          pt (trans mode 1 0)
;-;        )
      )
      ( (= "Object" mode)
        (while
          (and
            (not break)
            (setq jsel (ssget "_+.:E:S" filter))
          )
          (setq
            name (ssname jsel 0)
            obj (vlax-ename->VLA-Object name)
          )
          (if (vlax-property-available-p obj 'Area)
            (setq
              lst (cons (cons +1 (list (cons nil (list (vlax-get obj 'Area) (vlax-get obj 'Handle))))) lst)
              flag 1
              break T
            )
            (princ
              (LgT
                "\nThe selected object doesn't have an area..."
                "\nL'objet sélectionné n'a pas d'aire..."
                nil
              )
            )
          )
        )
      )
      ( (= "Add" mode)
        (setq mode
          (getkdh
            (quote)
          )
        )
      )
      ( (= "Substract" mode)
      )
      ( (= "eXit" mode) (setq break T))
    )
    (Spa-princ-param lst)
  )
  (princ)
)