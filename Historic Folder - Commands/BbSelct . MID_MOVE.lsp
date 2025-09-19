
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  MID_MOVE  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] MID_MOVE []-----------------------[]                                         ;
;--- Date of creation       > 30/09/2021                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.2                                                                                                                 ;
;--- Class                  > "BbSelct"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allows the user to move a selection set from the center of its BoundingBox.                                                                     ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves a selection set to move                                                                                               ;
; Step n°2        : Creates the bounding box object and changes its Color and Transparency properties                                               ;
; Step n°3        : Retrieves the center point coordinates (UCS)                                                                                    ;
; Step n°4        : Uses the command "_MOVE" to move the selected objects with the starting point corresponding to the center of BoundingBox frame  ;
;                   and asks the user to specify the second point                                                                                   ;
; Step n°5        : Deletes the bounding box object                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> LM:ssBoundingBoxMidPt                         | v1.0.1 - 09/06/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> PVBB-Draw                                     | v1.4.0 - 07/10/2022 (LeeMac/Luna)                                             ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.1.1 - 03/10/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (MID_MOVE) returns nothing.                                                                                                         ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.2   |   Update (PVBB-Draw) function to v1.4.0, allowing to draw a 3DPolyline if coplanar with XZ or YZ plan                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Fix an issue due to "OSMODE" using the function (command-s) and random osnap point                                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Add the visibility of the bounding box thanks to the function (PVBB-Draw) and add an error-handling function                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Renamming (Get-MidBoundingBox) as (LM:ssBoundingBoxMidPt)                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:MID_MOVE (/ *error* osm cde jsel ent obj pt pt2)
  (defun *error* (msg)
    (if ent (vla-delete ent))
    (setvar "OSMODE" osm)
    (setvar "CMDECHO" cde)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if
    (and
      (setq osm (getvar "OSMODE"))
      (setq cde (getvar "CMDECHO"))
      (or
        (setq jsel (ssget "_I"))
        (setq jsel (ssget))
      )
      (setq ent (car (PVBB-Draw jsel)))
      (setq obj (vla-get-ObjectName ent))
      (null
        (cond
          ( (= "AcDbPolyline" obj) (vla-put-Color ent 6))
          ( (= "AcDb3dPolyline" obj) (vla-put-Color ent 1))
          ( (= "AcDb3dSolid" obj) (vla-put-Color ent 4))
        )
      )
      (null (vla-put-EntityTransparency ent 50))
      (setq pt (LM:ssBoundingBoxMidPt jsel))
      (ssadd (vlax-VLA-Object->ename ent) jsel)
      (setq pt2 (getpoint pt "\nSecond point : "))
    )
    (progn
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (command-s "_MOVE" jsel "" pt pt2)
      (setvar "OSMODE" osm)
      (setvar "CMDECHO" cde)
      (vla-delete ent)
    )
    (princ
      (LgT
        "\nNo object(s) selected..."
        "\nAucun objet(s) sélectionné(s)..."
        nil
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)