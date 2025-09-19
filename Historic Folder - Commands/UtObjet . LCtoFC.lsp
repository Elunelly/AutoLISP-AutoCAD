
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  LCtoFC  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] LCtoFC []-----------------------[]                                           ;
;--- Date of creation       > 10/12/2021                                                                                                            ;
;--- Last modification date > 22/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Use the Layer Color (LC) of an object to use Forced Color (FC) on a set of objects.                                                             ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "DtObj" ---> color#                                        | v1.0.0 - 10/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (LCtoFC) returns nothing.                                                                                                           ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Add (LgT) for FR/EN version                                                                                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:LCtoFC (/ color# ent-s ent-t layer color62 color420 color430 jsel i)
  (defun color# (color key name)
    (cond
      ( (and
          color
          (assoc key (entget name))
        )
        (entmod (subst color (assoc key (entget name)) (entget name)))
      )
      ( (and
          color
          (not (assoc key (entget name)))
        )
        (entmod (append (entget name) (list color)))
      )
      ( (and
          (not color)
          (assoc key (entget name))
        )
        (entmod (vl-remove (assoc key (entget name)) (entget name)))
      )
    )
  )

  (while
    (and
      (setq ent-s (car (entsel (LgT "\nSelect source object : " "\nSélectionner l'objet source : " nil))))
      (if (not (assoc 62 (entget ent-s)))
        (setq
          layer (entget (tblobjname "LAYER" (cdr (assoc 8 (entget ent-s)))))
          color420 (assoc 420 layer)
          color430 (assoc 430 layer)
          color62 (assoc 62 layer)
        )
        (setq
          color420 (assoc 420 (entget ent-s))
          color430 (assoc 430 (entget ent-s))
          color62 (assoc 62 (entget ent-s))
        )
      )
    )
    (if
      (progn
        (princ (LgT "\nSelect target object(s) : " "\nSélectionner le(s) objet(s) cible(s) : " nil))
        (setq jsel (ssget))
      )
      (repeat (setq i (sslength jsel))
        (setq ent-t (ssname jsel (setq i (1- i))))
        (color# color62 62 ent-t)
        (color# color420 420 ent-t)
        (color# color430 430 ent-t)
      )
    )
  )
  (princ)
)