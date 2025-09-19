
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                         --{  PreViewBoundingBox  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] PreViewBoundingBox []-----------------------[]                                     ;
;--- Date of creation       > 04/07/2022                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.1                                                                                                                 ;
;--- Class                  > "BbBound"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allow the user to check the bounding box of a selection set and its representation in the drawing. A magenta object means it's a polyline (2D   ;
;   selection set) and a cyan object means it's a 3D solid (3D selection set). The objet representing the bounding box will be deleted at the end   ;
;   of the program and the selection set used for the command will remain as active selection set.                                                  ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves an implied selection set or ask for a new selection set                                                               ;
; Step n°2        : Creates the bounding box object                                                                                                 ;
; Step n°3        : Sets the selection set as active selection set                                                                                  ;
; Step n°4        : Displays a message for user to indicate the object type and its relation with the color                                         ;
; Step n°5        : Asks the user to erase the created object or not (Yes by default)                                                               ;
; Step n°6        : Deletes the bounding box object if answered Yes                                                                                 ;
; Step n°7        : Prompts the bottom left and upper right corner of the bounding box                                                              ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> PVBB-Draw                                     | v1.4.0 - 07/10/2022 (LeeMac/Luna)                                             ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.1.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (PreViewBoundingBox) returns the bottom left and the upper right corner of the bounding box if successful, nil otherwise            ;
;     Ex. :                                                                                                                                         ;
;       Command: PREVIEWBOUNDINGBOX                                                                                                                 ;
;       Select objects: Specify opposite corner: 3 found(s)                                                                                         ;
;       Select objects:                                                                                                                             ;
;       Object type : 3D Solid                                                                                                                      ;
;       Object color: Cyan                                                                                                                          ;
;       ENTER to end the command and delete the object :                                                                                            ;
;       ((-20.7107 -20.7107 0.0) (120.711 120.711 50.0))                                                                                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Update (PVBB-Draw) function to v1.4.0, allowing to draw a 3DPolyline if coplanar with XZ or YZ plan                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Ask if the user wants to delete the object or not                                                                              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:PreViewBoundingBox (/ *error* box jsel ent mode)
  (defun *error* (msg)
    (if ent (vla-delete ent))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )

  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (and
    (or
      (setq jsel (ssget "_I"))
      (setq jsel (ssget))
    )
    (setq ent (PVBB-Draw jsel))
    (setq box (cdr ent))
    (setq ent (car ent))
    (sssetfirst nil jsel)
    (cond
      ( (= "AcDbPolyline" (vla-get-ObjectName ent))
        (vla-put-color ent 6)
        (princ
          (LgT
            (strcat
              "\nObject type : Polyline"
              "\nObject color: Magenta"
            )
            (strcat
              "\nType d'objet   : Polyligne"
              "\nCouleur d'objet: Magenta"
            )
            nil
          )
        )
      )
      ( (= "AcDb3dPolyline" (vla-get-ObjectName ent))
        (vla-put-color ent 1)
        (princ
          (LgT
            (strcat
              "\nObject type : 3D Polyline"
              "\nObject color: Red"
            )
            (strcat
              "\nType d'objet   : Polyligne 3D"
              "\nCouleur d'objet: Rouge"
            )
            nil
          )
        )
      )
      ( (= "AcDb3dSolid" (vla-get-ObjectName ent))
        (vla-put-color ent 4)
        (princ
          (LgT
            (strcat
              "\nObject type : 3D Solid"
              "\nObject color: Cyan"
            )
            (strcat
              "\nType d'objet   : Solide 3D"
              "\nCouleur d'objet: Cyan"
            )
            nil
          )
        )
      )
    )
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nErase created object ?"
          "\nEffacer l'objet créé ?"
          nil
        )
        (list
          (LgT
            "Yes No _Yes No"
            "Oui Non _Yes No"
            nil
          )
        )
        ": "
        "Yes"
        nil
      )
    )
  )
  (if (and ent (= "Yes" mode)) (vla-delete ent))
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
  box
)