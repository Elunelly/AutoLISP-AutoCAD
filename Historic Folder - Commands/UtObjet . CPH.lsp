
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                                 --{  CPH  }--                                                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] CPH []-----------------------[]                                            ;
;--- Date of creation       > 13/06/2024                                                                                                            ;
;--- Last modification date > 23/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   The command CPH (= CoPyHandle) is meant to help you build excel extracts and get information about the handle of an object. It will copy the    ;
;   string representing the handle of the selected object into the clipboard and show you some information about the object (layer, color, length)  ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:True->RGB                                  | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LGT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "DtObj" ---> color->str                                    | v1.0.0 - 13/06/2024 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (CPH) returns the handle and displays the following informations in the historic of command line.                                   ;
;     Ex. :                                                                                                                                         ;
;           The handle "5B3523" has been copied into the clipboard...                                                                               ;
;           Properties :                                                                                                                            ;
;             • Layer    = PVcase AlignmentLine                                                                                                     ;
;             • Object   = LWPOLYLINE                                                                                                               ;
;             • Color    = 130 (ByLayer)                                                                                                            ;
;             • Length   = 141,02m                                                                                                                  ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Added a loop for multiple selection, temporary turning invisible the objects already selected during the command and return    | ;
; |            |   the handle + informations if only one object is selected during the command, a list of VLA-Object if several, nil otherwise    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Convert the "." format (real) to a "," format for better use in Excel                                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:CPH (/ *error* color->str htmlfile name obj entlist handle layer object color lst)
  (defun *error* (msg)
    (mapcar '(lambda (x) (vla-put-Visible x :vlax-true)) lst)
    (vlax-release-object htmlfile)
    (princ msg)
    (princ)
  )
  (defun color->str (dxf / c)
    (cond
      ( (cdr (assoc 430 dxf)))
      ( (setq c (cdr (assoc 420 dxf))) (lst2str (LM:True->RGB c) ", "))
      ( (setq c (cdr (assoc 62 dxf))) (itoa c))
      ( (strcat (color->str (entget (tblobjname "LAYER" (cdr (assoc 8 dxf))))) (LgT " (ByLayer)" " (DuCalque)" nil)))
    )
  )

  (setq htmlfile (vlax-get-or-create-object "htmlfile"))
  (while (setq name (car (entsel)))
    (setq
      obj (vlax-ename->vla-object name)
      entlist (entget name)
      handle (cdr (assoc 5 entlist))
      layer (cdr (assoc 8 entlist))
      object (cdr (assoc 0 entlist))
      color (color->str entlist)
      lst (cons obj lst)
    )
    (vlax-invoke
      (vlax-get
        (vlax-get htmlfile 'ParentWindow)
        'ClipBoardData
      )
      'SetData
      "Text"
      handle
    )
    (princ
      (LgT
        (strcat "\nThe handle \"" handle "\" has been copied into the clipboard...")
        (strcat "\nLe handle \"" handle "\" a été copié dans le presse-papier...")
        nil
      )
    )
    (vla-put-Visible obj :vlax-false)
  )
  (cond
    ( (null lst)
      (vlax-invoke
        (vlax-get
          (vlax-get htmlfile 'ParentWindow)
          'ClipBoardData
        )
        'SetData
        "Text"
        ""
      )
      (vlax-release-object htmlfile)
      (princ (LgT "\nNo object selected...\n" "\nAucun objet sélectionné...\n" nil))
      nil
    )
    ( (= 1 (length lst))
      (vlax-release-object htmlfile)
      (vla-put-Visible obj :vlax-true)
      (princ
        (LgT
          (strcat
            "\nProperties : "
            "\n  • Layer    = " layer
            "\n  • Object   = " object
            "\n  • Color    = " color
            (if (= "LWPOLYLINE" object)
              (strcat
                "\n  • Length   = "
                (vl-string-translate "." "," (rtos (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 2 2))
                (units (getvar "INSUNITS"))
              )
              ""
            )
            "\n"
          )
          (strcat
            "\nPropriétés : "
            "\n  • Calque   = " layer
            "\n  • Objet    = " object
            "\n  • Couleur  = " color
            (if (= "LWPOLYLINE" object)
              (strcat
                "\n  • Longueur = "
                (vl-string-translate "." "," (rtos (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 2 2))
                (units (getvar "INSUNITS"))
              )
              ""
            )
            "\n"
          )
          nil
        )
      )
      handle
    )
    ( (< 1 (length lst))
      (vlax-release-object htmlfile)
      (mapcar '(lambda (x) (vla-put-Visible x :vlax-true)) lst)
      (princ (LgT "\nRestoring visibility for all objects...\n" "\nRestauration de la visibilité des objets...\n" nil))
      lst
    )
  )
)