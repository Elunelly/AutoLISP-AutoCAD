
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                         --{  layer-get-or-create  }--                                                         | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] layer-get-or-create []-----------------------[]                                    ;
;--- Date of creation       > 13/06/2024                                                                                                            ;
;--- Last modification date > 13/06/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSyt"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Create a new layer if it doesn't exist or modify the layer's properties if found.                                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (layer-get-or-create) have 5 argument(s) :                                                                                           ;
;   --•  name                   > correspond to the name of the layer                                                                               ;
;     (type name) = 'STR                        | Ex. : "0", "$temp$", ...                                                                          ;
;   --•  color                  > correspond to the color of the layer. It can be an ACI color (Integer from 1 to 254) or an RGB color (list of 3   ;
;                                 Integer from 1 to 254)                                                                                            ;
;     (type color) = 'INT or 'LIST              | Ex. : 1, 114, '(254 15 36), (CrosshairColor->RGB), ...                                            ;
;   --•  ltype                  > correspond to the linetype of the layer                                                                           ;
;     (type ltype) = 'STR                       | Ex. : "Continuous", "Hidden", ...                                                                 ;
;   --•  lwght                  > correspond to the lineweight of the layer                                                                         ;
;     (type lwght) = 'INT                       | Ex. : 0, 13, 9, ...                                                                               ;
;   --•  plot                   > correspond to the property of plotting of the layer                                                               ;
;     (type plot) = 'INT                        | Ex. : 0 (= Not-Plottable) or 1 (= Plottable)                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:RGB->ACI                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;   --•  "BaAri" ---> LM:RGB->True                                  | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (layer-get-or-create) returns the DXF list of the layer                                                                            ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun layer-get-or-create ( name color ltype lwght plot / layer entlist color62 color42 color43)
  (cond
    ( (= 'INT (type color))
      (setq color62 (cons 62 color))
      (setq color42 nil)
      (setq color43 nil)
    )
    ( (= 'LIST (type color))
      (setq color62 (cons 62 (LM:RGB->ACI (car color) (cadr color) (caddr color))))
      (setq color42 (cons 420 (LM:RGB->True (car color) (cadr color) (caddr color))))
      (setq color43 nil)
    )
  )
  (setq
    ltype (if ltype (cons 6 ltype))
    lwght (if lwght (cons 370 lwght))
    plot (if plot (cons 290 plot))
  )
  (if (setq layer (tblobjname "LAYER" name))
    (setq
      entlist (entget layer)
      entlist (entmod (subst '(70 . 0) (assoc 70 entlist) entlist))
      entlist (if color (entmod (subst color62 (assoc 62 entlist) entlist)) entlist)
      entlist
        (if color
          (entmod
            (if (assoc 420 entlist)
              (if color42
                (subst color42 (assoc 420 entlist) entlist)
                (vl-remove (assoc 420 entlist) entlist)
              )
              (if color42
                (append entlist (list color42))
                entlist
              )
            )
          )
          entlist
        )
      entlist
        (if color
          (entmod
            (if (assoc 430 entlist)
              (if color43
                (subst color43 (assoc 430 entlist) entlist)
                (vl-remove (assoc 430 entlist) entlist)
              )
              (if color43
                (append entlist (list color43))
                entlist
              )
            )
          )
          entlist
        )
      entlist (if ltype (entmod (subst ltype (assoc 6 entlist) entlist)) entlist)
      entlist (if lwght (entmod (subst lwght (assoc 370 entlist) entlist)) entlist)
      entlist (if plot (entmod (subst plot (assoc 290 entlist) entlist)) entlist)
    )
    (entmake
      (vl-remove
        nil
        (list
          '(0 . "LAYER")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbLayerTableRecord")
          (cons 2 name)
          '(70 . 0)
          color62
          color42
          color43
          (cond (ltype) ('(6 . "Continuous")))
          (cond (lwght) ('(370 . 0)))
          (cond (plot) ('(290 . 1)))
        )
      )
    )
  )
)