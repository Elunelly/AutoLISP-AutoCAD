
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                             --{  FNTAERIENNE  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] FNTAERIENNE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.1                                                                                                                 ;
;--- Class                  > "PsViewp"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Defines the PV field and Skyview as default layer ON for the specified viewports.                                                               ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieve the value of "CMDECHO" and "NOMUTT" to keep their initial value (if an error occurs)                                   ;
; Step n°2        : Define the list of layers name that will be thawed in the viewports (relative's names)                                          ;
; Step n°3        : Check if the current layout tab is "Model"                                                                                      ;
; Step n°3.a        : If :True, then prompt a dialog box to ask the user to set another layout tab as current                                       ;
; Step n°4        : Check if the layers are found in the drawing                                                                                    ;
; Step n°4.a        : If :False, then display a message and stop the program                                                                        ;
; Step n°4.b        : If :True, then the program can continue                                                                                       ;
; Step n°5        : Ask the user to select viewport(s) with a filtered selection based on object type                                               ;
; Step n°5.a        : If :False, then display a message and stop the program                                                                        ;
; Step n°5.b        : If :True, then the program can continue                                                                                       ;
; Step n°6        : Set "NOMUTT" as 1 to suppress the returns of (command)                                                                          ;
; Step n°7        : For each selected viewport                                                                                                      ;
; Step n°7.a        : Set it as current viewport and activate the Model Space to get in                                                             ;
; Step n°7.b        : Freeze all the layers, then Thaw only the layer corresponding to the filter in the current viewport                           ;
; Step n°7.c        : Go back in Paper Space                                                                                                        ;
; Step n°8        : Reset the initial value of "NOMUTT", display the information (number of viewports, name of layout tab and layer's list) and     ;
;                   reset the initial value of "CMDECHO"                                                                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtCom" ---> SetVarList                                    | v2.0.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> MuteSel                                       | v1.0.0 - 16/06/2022 (Luna)                                                    ;
;   --•  "DtSyt" ---> flt_tbl                                       | v4.0.0 - 20/06/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> get-layouts-pos                               | v2.0.0 - 24/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.1.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (FNTAERIENNE) returns the number of viewport modified, the name of layout tab concerned and the list of layers thawed.              ;
;     Ex. :                                                                                                                                         ;
;       Please select one or several viewport(s) :                                                                                                  ;
;       The following layers has been set for 1 viewport(s) on "C3560_E_3050-01_RACCORDEMENT AU RESEAU PUBLIC DE DISTRIBUTION" :                    ;
;         • UBS-100-Champ PV                                                                                                                        ;
;         • UBS-900-XREF Photo aérienne                                                                                                             ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:FNTAERIENNE ( / *error* CMDECHO NOMUTT lst jsel i name )
  (defun *error* (msg)
    (SetVarList (list (list "CMDECHO" nil CMDECHO) (list "NOMUTT" nil NOMUTT)))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("CMDECHO" CMDECHO 0) ("NOMUTT" NOMUTT nil)))
  (setq lst '("UBS-900-*AERIENNE*" "UBS-100-CHAMP*PV*"))
  (if (= "Model" (getvar "CTAB"))
    (setvar
      "CTAB"
      (ListBox
        (LgT
          "FNTAERIENNE: Current layout tab selection"
          "FNTAERIENNE: Sélection de la présentation courante"
          nil
        )
        (LgT
          "The command FNTAERIENNE can't work from Model space, please select a layout tab :"
          "La commande FNTAERIENNE ne peut pas fonctionner depuis l'espace Objet, sélectionner une présentation :"
          nil
        )
        (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
        nil
        0
        nil
      )
    )
  )
  (command "_PSPACE")
  (cond
    ( (null (setq lst (flt_tbl "LAYER" (lst2str lst ",") nil)))
      (princ (LgT "No layer(s) found." "Aucun calque(s) trouvé(s)." nil))
    )
    ( (null
        (setq jsel
          (MuteSel
            (LgT
              "\nPlease select one or several viewport(s) : "
              "\nSélectionner une ou des fenêtre(s) de présentation : "
              nil
            )
            (quote (ssget '((0 . "VIEWPORT"))))
          )
        )
      )
      (princ (LgT "No object(s) found." "Aucun objet(s) trouvé(s)." nil))
    )
    ( T
      (setvar "NOMUTT" 1)
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (command "_-VPORTS" "_ON" name "")
        (command "_MSPACE")
        (command "_VPLAYER" "_Freeze" "*" "_Current" "_Thaw" (lst2str lst ",") "_Current" "")
        (command "_PSPACE")
      )
      (setvar "NOMUTT" NOMUTT)
      (princ
        (strcat
          (LgT
            "\nThe following layers has been set for "
            "\nLes calques suivants ont été définis pour "
            nil
          )
          (itoa (sslength jsel))
          (LgT
            " viewport(s) on \""
            " fenêtre(s) de présentation sur \""
            nil
          )
          (getvar "CTAB")
          "\" :"
          "\n  • "
          (lst2str lst "\n  • ")
        )
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setvar "CMDECHO" CMDECHO)
  (princ)
)