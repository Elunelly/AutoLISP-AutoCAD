
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  RADPURGE  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] RADPURGE []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.2                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Deletes all layout tab from a drawing, restores the properties "LayerOn" = :vlax-true, "Freeze" = :vlax-false and "Lock" = :vlax-false for all  ;
;   layers, purges all the unused objects or styles defined in the drawing and set the current layer on "0".                                        ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Creation of a temporary layer named "$tmp$" and set it as the current layer                                                     ;
; Step n°2        : Sets the "Model" layout tab as current                                                                                          ;
; Step n°3        : Deletes all layout tabs except the "Model" found in the 'layouts collection                                                     ;
; Step n°4        : For all layer that doesn't contain the "|" characters (= Xref layers) or the "$tmp$" layer, do                                  ;
;   Step n°4.a    :   Puts the property "LayerOn" at the value :vlax-true. It means that the layer is active                                        ;
;   Step n°4.b    :   Puts the property "Freeze" at the value :vlax-false. It means that the layer is thawed                                        ;
;   Step n°4.c    :   Puts the property "Lock" at the value :vlax-false. It means that the layer is unlocked                                        ;
; Step n°5        : Sets the layer "0" as current                                                                                                   ;
; Step n°6        : Delete the temporary layer named "$tmp$"                                                                                        ;
; Step n°7        : Purges the drawing of all unused object and styles in the drawing                                                               ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (RADPURGE) returns only the result of "_-PURGE" command because the "CMDECHO" system variable is still active for this command.     ;
;     Ex. of return :                                                                                                                               ;
; Command: RADPURGE                                                                                                                                 ;
; Regeneration of the object - cached windows.                                                                                                      ;
; _-PURGE                                                                                                                                           ;
; Nested elements = Off Non-referenced data = Off                                                                                                   ;
; Enter the type of unused objects to be purged [blocks/detailviewstyles/dimensionstyles/groups/layers/linetypes/materials/multileaderstyles/       ;
; plotstyles/shapes/textstyles/multilinestyles/sectionviewstyles/tablestyles/visualstyles/registeredapplications/zero-lengthgeometry/               ;
; emptytextobjects/orphanedDGNlinestyledata/all]: _All Enter name(s) to be purged <*>: * Check each name to be purged? [Yes/No] <O>: _No            ;
; No blocks element without reference found.                                                                                                        ;
; No layers elements without reference found.                                                                                                       ;
; No line types elements without reference found.                                                                                                   ;
; No text styles elements without reference found.                                                                                                  ;
; No shape files element found without reference.                                                                                                   ;
; No dimension styles items found without a reference.                                                                                              ;
; No Multiline styles elements found without a reference.                                                                                           ;
; No plotter style elements found without a reference.                                                                                              ;
; No table styles items found without a reference.                                                                                                  ;
; No materials element found without a reference.                                                                                                   ;
; No visual styles elements found without a reference.                                                                                              ;
; No multiple cue line styles elements found without a reference.                                                                                   ;
; No Groups element found without reference.                                                                                                        ;
; No Detail View Styles elements found without a reference.                                                                                         ;
; No cross-sectional view styles elements without a reference found.                                                                                ;
; Command:                                                                                                                                          ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.2   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.1   |   Using (vlax-put) functions instead of (vla-put-*) functions to avoid any problem                                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Supression of the (command), except for "_-PURGE" command and replace it with the collection-handling 'layouts and 'layers     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:RADPURGE (/ *error* doc $ layout layer)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    $ (vla-add (vla-get-layers doc) "$tmp$")
  )
  (setvar "CLAYER" "$tmp$")
  (setvar "CTAB" "Model")
  (vlax-for layout (vla-get-layouts doc)
    (if (/= (vla-get-name layout) "Model")
      (vla-delete layout)
    )
  )
  (vlax-for layer (vla-get-layers doc)
    (if (not (wcmatch (vla-get-name layer) "*|*,$tmp$"))
      (progn
        (vlax-put layer 'LayerOn -1)
        (vlax-put layer  'Freeze 0)
        (vlax-put layer 'Lock 0)
      )
    )
  )
  (setvar "CLAYER" "0")
  (setq $ (vla-delete $))
  (command "_-PURGE" "_All" "*" "_No")
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)