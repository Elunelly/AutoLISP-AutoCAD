
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                             --{  VP-RADPURGE  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] VP-RADPURGE []-----------------------[]                                        ;
;--- Date of creation       > 23/12/2019                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.1.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Uses the frozen layers list of a viewport object to delete all of them from the drawing and purge unreferenced entities. Very useful to create  ;
;   an E-transmit based on a specific layout tab.                                                                                                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Unselects the current objects to avoid any problems                                                                             ;
; Step n°2        : Creates a "_Mark" from the "_UNDO" command to allow the user and/or user to cancel the deletion of the layers and layouts       ;
; Step n°3        : Adds a new and temporary layer to the layers's collection, named "$tmp$" and sets it as the current layer                       ;
; Step n°4        : Checks if the current layout tab equals to "Model"                                                                              ;
; Step n°4.a      :   If :True, opens a dialog box to select a layout tab within the (layoutlist) and sets it as the current tab                    ;
; Step n°5        : Forces the user to be in PaperSpace, to be sure to be able to select the viewport object                                        ;
; Step n°6        : Asks the user to select a single viewport (filtered selection). Will ask again until a viewport is selected                     ;
; Step n°7        : Retrieves the frozen layers list of the selected viewport and open a dialog box (multiple selection) to modify this list if     ;
;                   needed. All the layers are displayed in the list box, and only the frozen ones are pre-selected, so the user can add or remove  ;
;                   a layer from the list of layers that will be deleted                                                                            ;
; Step n°8        : Modify the properties of all layers as follow ('Lock = False, 'Freeze = False, 'LayerOn = True) to avoid any problem later      ;
; Step n°9        : Sets the system variables as follow ("CLAYER" = "0", "CTAB" = "Model", "CECOLOR" = "DuCalque", "CELTYPE" = "DuCalque")          ;
; Step n°10       : Deletes the layer "$tmp$"                                                                                                       ;
; Step n°11       : Starts the "_-LAYDEL" command and foreach layer needed to me removed, add their name to the "_-LAYDEL" command                  ;
; Step n°12       : Alerts the user with the list of all deleted layers (only 50 layers per alert's messages)                                       ;
; Step n°13       : Asks the user if the drawing is coherent with the expected result                                                               ;
; Step n°13.a     :   If :False, then go "_Back" to the "_UNDO" "_Mark" and ends the command                                                        ;
; Step n°13.b     :   If :True, then delete all the layouts, "_-PURGE" the drawing of unreferenced entities and prompt the list of deleted layers   ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> fixdiv                                        | v1.0.1 - 25/06/2020 (Luna)                                                    ;
;   --•  "BaLst" ---> divlist                                       | v2.0.0 - 16/06/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtCom" ---> SetVarList                                    | v2.0.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlCol" ---> vla-collection->list                          | v2.0.0 - 19/08/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.2.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> VP-get-layer-list                             | v1.0.0 - 10/05/2022 (Luna)                                                    ;
;   --•    "VlPrp" ---> VP-get-frozen-layers                        | v1.0.0 - 10/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (VP-RADPURGE) returns the list of all layers only if you agreed with the final result, otherwise it will undo the whole command     ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.1   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Correct (*error*) to handle the "$tmp$" layer if current, delete layer "0" from the list, add "NOMUTT" system variable,        | ;
; |            |   update (divlist) function to v2.0.0 and add (SetVarList)                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Update (LgT) function to v1.1.0                                                                                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:VP-RADPURGE (/ *error* VP-get-layer-list doc CMDECHO NOMUTT CLAYER $ jsel name layer-list t0 i)
  (defun *error* (msg)
    (if layer-list (command-s "_UNDO" "_Back"))
    (setvar "CMDECHO" CMDECHO)
    (setvar "NOMUTT" NOMUTT)
    (if $
      (progn
        (setvar "CLAYER" CLAYER)
        (vla-delete $)
      )
    )
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun VP-get-layer-list (name flag / VP-get-frozen-layers lst)
    (defun VP-get-frozen-layers (vp / typ val)
      (vla-getXdata vp "ACAD" 'typ 'val)
      (cddr (reverse (cdr (member "{" (cdr (member "{" (mapcar 'vlax-variant-value (vlax-safearray->list val))))))))
    )
    (setq lst (VP-get-frozen-layers (vlax-ename->vla-object name)))
    (if flag
      (vl-remove-if '(lambda (x) (member x lst)) (mapcar 'car (vla-collection->list nil 'layers 1)))
      lst
    )
  )
  
  (sssetfirst nil nil)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("CMDECHO" CMDECHO 0) ("NOMUTT" NOMUTT nil)))
  (command-s "_UNDO" "_Mark")
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    $ (vla-add (vla-get-layers doc) "$tmp$")
  )
  (SetVarList '(("CLAYER" CLAYER "$tmp$")))
  (if (= "Model" (getvar "CTAB"))
    (setvar
      "CTAB"
      (ListBox
        (LgT
          "VP-RADPURGE: Current layout tab selection"
          "VP-RADPURGE: Sélection de la présentation courante"
          nil
        )
        (LgT
          "The command VP-RADPURGE can't work from Model space, please select a layout tab :"
          "La commande VP-RADPURGE ne peut pas fonctionner depuis l'espace Objet, sélectionner une présentation :"
          nil
        )
        (vl-remove-if '(lambda (l) (member (strcase l) '("TOOLKIT" "TRAVAIL"))) (layoutlist))
        nil
        0
        nil
      )
    )
  )
  (command "_PSPACE")
  (while
    (progn
      (princ
        (LgT
          "\nPlease, select a single viewport :"
          "\nVeuillez sélectionner une seule fenêtre de présentation :"
          nil
        )
      )
      (setvar "NOMUTT" 1)
      (setq jsel (ssget "_+.:E:S" '((0 . "VIEWPORT"))))
      (setvar "NOMUTT" NOMUTT)
      (null jsel)
    )
  )
  (setq
    name (ssname jsel 0)
    layer-list (VP-get-layer-list name nil)
  )
  (if
    (setq layer-list
      (ListBox
        (LgT
          "VP-RADPURGE : Layer(s) selection"
          "VP-RADPURGE : Sélection des calques"
          nil
        )
        (LgT
          "Please, select layer(s) to be deleted :"
          "Sélectionner le(s) calque(s) à supprimer :"
          nil
        )
        (vl-sort (vl-remove-if '(lambda (l) (wcmatch l "*|*,$tmp$,0")) (mapcar 'car (vla-collection->list nil 'layers 0))) '<)
        layer-list
        2
        50
      )
    )
    (progn
      (setq t0 (getvar "MILLISECS"))
      (vlax-for layer (vla-get-layers doc)
        (if (not (wcmatch (vla-get-Name layer) "*|*,$tmp$"))
          (progn
            (vlax-put layer 'LayerOn -1)
            (vlax-put layer 'Freeze 0)
            (vlax-put layer 'Lock 0)
          )
        )
      )
      (SetVarList '(("CLAYER" nil "0") ("CECOLOR" nil "256") ("CELTYPE" nil "ByLayer") ("CTAB" nil "Model") ("NOMUTT" nil 1)))
      (setq $ (vla-delete $))
      (command "_-LAYDEL")
      (foreach layer layer-list
        (command "_Name" layer)
      )
      (command "" "_Yes")
      (setvar "NOMUTT" NOMUTT)
      (setq i 0)
      (repeat (1- (length (setq layer-list (divlist (vl-sort layer-list '<) (fixdiv (length layer-list) 50)))))
        (alert
          (strcat
            (LgT
              "See below, the list of layers that have been purged from the drawing :"
              "Ci-dessous la liste des calques ayant été purgé de ce dessin :"
              nil
            )
            "\n  - "
            (lst2str (nth i layer-list) "\n  - ")
            "\n"
            "\n( page "
            (itoa (setq i (1+ i)))
            " / "
            (itoa (length layer-list))
            " )"
          )
        )
      )
      (alert
        (strcat
          (LgT
            "See below, the list of layers that have been purged from the drawing :"
            "Ci-dessous la liste des calques ayant été purgé de ce dessin :"
            nil
          )
          "\n  - "
          (lst2str (nth i layer-list) "\n  - ")
          "\n"
          (LgT
            "\nThe command took "
            "\nLa commande a pris "
            nil
          )
          (rtos (* (- (getvar "MILLISECS") t0) 0.001) 2 1)
          (LgT
            " seconds to delete "
            " secondes pour supprimer "
            nil
          )
          (itoa (length (apply 'append layer-list)))
          (LgT
            " layer(s)..."
            " calque(s)..."
            nil
          )
          "\n"
          "\n( page "
          (itoa (setq i (1+ i)))
          " / "
          (itoa (length layer-list))
          " )"
        )
      )
      (if
        (=
          "No"
          (getkdh
            (quote (getkword msg))
            (LgT
              "\nDoes the drawing make sense to you"
              "\nLe dessin vous semble-t-il cohérent"
              nil
            )
            '(1 "Oui Non _Yes No")
            " ?"
            nil
            (LgT
              (strcat
                "\nVP-RADPURGE : User confirmation after the purge"
                "\nDefault value:     <none>"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | You are agree with the actual state of the drawing. The program     |"
                "\n  |             | will delete all the layout tabs, purge unreferenced objet(s) and    |"
                "\n  |     Yes     | display the list of deleted layer(s)                                |"
                "\n  |             | You still have the possibility to undo the command by using the     |"
                "\n  |             | 'Back' option of UNDO command to go back to the previous 'Mark'     |"
                "\n  |             | set at the begining of VP-RADPURGE                                  |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | You are not agree with the actual state of the drawing. The program |"
                "\n  |     No      | will go back to the previous 'Mark' (command UNDO) to reset the     |"
                "\n  |             | drawing to its state at the begining of VP-RADPURGE                 |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              (strcat
                "\nVP-RADPURGE : Confirmation utilisateur après la purge"
                "\nValeur par défaut: <aucune>"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Vous êtes d'accord avec l'état actuel du dessin. Le programme va    |"
                "\n  |             | supprimer tous les onglets de présentation, purger les objet(s)     |"
                "\n  |     Oui     | non référencés et afficher la liste des calque(s) supprimé(s)       |"
                "\n  |             | Vous avez toujours la possibilité d'annuler la commande en          |"
                "\n  |             | utilisant l'option 'Retour' de la commande ANNULER pour retourner   |"
                "\n  |             | à la 'Marque' précédente définie au début de VP-RADPURGE            |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Vous n'êtes pas d'accord avec l'état actuel du dessin. Le programme |"
                "\n  |     Non     | va retourner à la 'Marque' (commande ANNULER) précédente pour       |"
                "\n  |             | redéfinir le dessin à son état tel qu'au début de VP-RADPURGE       |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              nil
            )
          )
        )
        (progn
          (command-s "_UNDO" "_Back")
          (setq layer-list nil)
          (princ
            (LgT
              "\nThe command VP-RADPURGE has been undo..."
              "\nLa commande VP-RADPURGE a été annulée..."
              nil
            )
          )
        )
        (progn
          (setvar "NOMUTT" 1)
          (vlax-for layout (vla-get-layouts doc)
            (if (/= "Model" (vla-get-Name layout))
              (vla-delete layout)
            )
          )
          (command "_-PURGE" "_All" "*" "_No")
          (setvar "NOMUTT" NOMUTT)
          (princ
            (strcat
              (LgT
                "See below, the list of "
                "Ci-dessous la liste des "
                nil
              )
              (itoa (length (apply 'append layer-list)))
              (LgT
                " layers that have been purged from the drawing :"
                " calques ayant été purgé de ce dessin :"
                nil
              )
              "\n  - "
              (lst2str (apply 'append layer-list) "\n  - ")
            )
          )
        )
      )
    )
    (*error* "")
  )
  (setvar "CMDECHO" CMDECHO)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)