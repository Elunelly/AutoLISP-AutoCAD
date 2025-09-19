
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                                 --{  GRUE  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] GRUE []-----------------------[]                                           ;
;--- Date of creation       > 27/03/2025                                                                                                            ;
;--- Last modification date > 02/04/2025                                                                                                            ;
;--- Author                 > barbichette/Luna                                                                                                      ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtUse" ---> getkdh                                        | v2.2.0 - 15/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### ()                                                        ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (GRUE) returns nothing but draws a diagram for crane max weight based on their length. This diagram is only made with circle        ; 
;   (= position of the hook) and the corresponding text indicating the radius (in meters) and the maximum weight (in tons).                         ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Re-writting the whole program with a bit more solid structure (developpers params) and more user-friendly                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:GRUE (/ capacities craneType craneCapacities craneBoom craneInsertionPoint values dimtxt angtxt jsel)
  ;;                                 ---------[  DEVELOPPER PARAMS  ]---------                                 ;;
  ;; This section allows the developper to change quickly the parameters used by the program based on their need
  ;; Each parameters can be set as constants, ignored (= nil) or develop a full question if it needs to be set
  ;; by the user instead.
  (setq 
    dimtxt nil          ; Defines the text height (nil → use DIMTXT value instead)
    angtxt nil          ; Defines the axis angle on which the text will be aligned on (nil → 0 (East))
    algtxt nil          ; Defines the text alignment mode (cd. MTEXT(DXF), code 71) (nil → 5 (Middle Center))
    radiusPrec nil      ; Defines the precision (number of decimal) for the crane boom length (nil → 2)
    weightPrec nil      ; Defines the precision (number of decimal) for the weight (nil → 2)
    maskBoxScale 1.2    ; Defines the fill box scale value for the text background mask (nil → no mask)
    capacities          ; Defines the different capacities based on the crane model and their length
      '(
        ;;            ---------[  GRUE_DIAG219  ]---------            ;;
        ("MDT219"
          (25 (20 20) (25 8.4))
          (30 (20 10) (25 8.6) (30 7))
          (35 (21 10) (25 8.5) (30 6.9) (35 5.8))
          (40 (20 10) (25 8.4) (30 6.9) (35 5.8) (40 5))
          (45 (20 10) (25 8.3) (30 6.8) (35 5.7) (40 4.9) (45 4.3))
          (50 (20 10) (25 8.3) (30 6.7) (35 5.6) (40 4.7) (45 4) (50 3.5))
          (55 (20 10) (25 7.9) (30 6.4) (35 5.4) (40 4.5) (45 3.9) (50 3.3) (55 2.95))
          (60 (20 9.4) (25 7.3) (30 6) (35 4.6) (40 4.2) (45 3.6) (50 3.1) (55 2.6) (60 2.2))
          (65 (20 8.88) (25 6.9) (30 5.6) (35 4.6) (40 3.9) (45 3.3) (50 2.85) (55 2.4) (60 1.95) (65 1.6))
        ) ; End GRUE_DIAG219 params
      )
  ) ; End of DEVELOPPER PARAMS
  ;; You can change the default value below if needed also
  (setq
    dimtxt (cond (dimtxt) ((getvar "DIMTXT")))
    angtxt (cond (angtxt) (0))
    algtxt (cond (algtxt) (5))
    radiusPrec (cond (radiusPrec) (2))
    weightPrec (cond (weightPrec) (2))
  ) ; End default params values
  
  
  ;;                                   ---------[  PROGRAM CODE  ]---------                                    ;;
  (sssetfirst)
  (while
    (not
      (member
        (setq craneType
          (getkdh
            (quote (getkword msg))
            "\nVeuillez sélectionner le type de grue"
            (list (apply 'strcat (mapcar '(lambda (x) (strcat (vl-princ-to-string (car x)) " ")) capacities)))
            ": "
            (caar capacities)
            "" ;You can add any help message for this question in order to guide the users on the options
          )
        )
        (mapcar 'car capacities)
      )
    )
    (princ "\nErreur sur le type de grue renseigné...")
  )
  (setq craneCapacities (cdr (assoc craneType capacities)))
  (while
    (not
      (member
        (setq craneBoom
          (getkdh
            (quote (getint msg))
            "\nVeuillez sélectionner la longueur de flèche installée"
            (list (apply 'strcat (mapcar '(lambda (x) (strcat (vl-princ-to-string (car x)) " ")) craneCapacities)))
            ": "
            (caar craneCapacities)
            "" ;You can add any help message for this question in order to guide the users on the options
          )
        )
        (mapcar 'car craneCapacities)
      )
    )
    (princ "\nErreur sur la longueur de flèche renseigné...")
  )
  (setq
    craneInsertionPoint (trans (getpoint "\nSpécifiez le point d'insertion : ") 1 0)
    values (cdr (assoc craneBoom craneCapacities))
    jsel (ssadd)
  )
  (foreach item values
    (setq
      radius (car item)
      weight (cadr item)
      pt0txt (polar craneInsertionPoint angtxt radius)
    )
    (setq
      entCircle
        (entmakex
          (vl-remove nil
            (list
              '(0 . "CIRCLE")
              (cons 10 craneInsertionPoint)
              (cons 40 radius)
            )
          )
        )
      entText
        (entmakex
          (vl-remove nil
            (append
              (list
                '(0 . "MTEXT")
                '(100 . "AcDbEntity")
                '(100 . "AcDbMText")
                (cons 10 pt0txt)
                (cons 40 dimtxt)
                (cons 50 (- angtxt (/ pi 2.)))
                (cons 71 algtxt)
                (cons 1 (strcat (rtos radius 2 radiusPrec) "m - " (rtos weight 2 weightPrec) "t")) ; ← if you need to change the text output
                ; ex: "25.00m - 7.90t"
              )
              (if maskBoxScale 
                (list
                  '(90 . 3)
                  '(63 . 256)
                  '(441 . 0) 
                  (cons 45 maskBoxScale)
                )
              )
            )
          )
        )
    )
    (ssadd entCircle jsel)
    (ssadd entText jsel)
  )
  (princ "\nFin de la génération du diagramme de charge...")
  (sssetfirst nil jsel)
  (princ)
)