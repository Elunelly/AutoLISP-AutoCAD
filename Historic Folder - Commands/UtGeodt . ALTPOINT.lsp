
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  ALTPOINT  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ALTPOINT []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 19/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.1.0                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Controls and/or modifies the Z coordinates of block references used for topographic surveys. Uses the default block reference "TCPOINT" with 2  ;
;   attributes named "ALT" and "MAT" but the user can use any other block and attribute names. It uses the attribute ("ALT") value to define the Z  ;
;   coordinate for each selected block reference. You can also use a matricule attribute ("MAT") to know the name of the block reference. The user  ;
;   can chose to export the data on a .csv file to check the value or to modify the Z coordinates for block references.                             ;
;   At the end of the program, only the modified block reference will remain selected to visualize which block were wrong or not.                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Sets the default values on "ALT" (Altitude attribute name), "MAT" (Matricule attribute name), Select all block reference, modify;
;                   the block references without creating a .csv file, display the 5% lower/higher top values of the list and no default value for  ;
;                   block's name                                                                                                                    ;
; Step n°2        : Asks the user to pick a block reference within the drawing, or a list or the default value "TCPOINT"                            ;
; Step n°2.a      :   Option "TCpoint", uses the name "TCPOINT" as default value with "ALT" and "MAT" attribute. Will end the choice menu so be     ;
;                     sure to define your settings before using "TCPOINT"                                                                           ;
; Step n°2.b      :   Option "List", opens a dialog box to select one or more block definition's name within a list. Will not end the choice menu   ;
; Step n°2.c      :   Option "Settings", opens a dialog box to define how the command should work as the name(s) of attributes for Altitude and     ;
;                     Matricule, if you want to consider the Matricule or not, if you want to select all block references based on their name or if ;
;                     you want to select them manually (or with pre-selection), if you want to edit a .csv file to control the values or to modify  ;
;                     the block references, and the percentage for lower/higher top values of the list. For the attribute names, it's possible to   ;
;                     define more than one name, using the comma (,) to separate each tag (you can use the list on the right to help you). The order;
;                     of the tags will define the priority between each tag (if more than 1 tag are common for 1 block reference). Will not end the ;
;                     choice menu                                                                                                                   ;
; Step n°2.d      :   Option "?", will prompt the help in command line historic to explain each option. Will not end the choice menu                ;
; Step n°2.e      :   If you click an entity and it's a block reference with attributes, it will be added to the list of block names. But be very   ;
;                     careful because if you miss the click, the default option ("TCPOINT") will be used and you'll not be able to modify the       ;
;                     settings ("TCPOINT" ends the choice menu). Will not end the choice menu                                                       ;
; Step n°2.f      :   Option "eXit", allows the user to end the choice menu and continue the program with the current selection (settings)          ;
; Step n°3        : Selects block references based on their name and attribute names                                                                ;
; Step n°4        : For each block reference...                                                                                                     ;
; Step n°4.a      :   Retrieves their coordinates (X Y Z), their attributes values and check if the Altitude attribute value is different from Z    ;
; Step n°4.a.1    :     If :True, add the block reference to the selection set that will be used at the end of the command                          ;
; Step n°4.b      :   If the settings allow to modify the block reference, the Z coordinate will be modified with the Altitude value                ;
; Step n°5        : Sorts the list of altitude value from the lower to the higher value                                                             ;
; Step n°6        : Check if the settings about modifiying objects is set or not                                                                    ;
; Step n°6.a      :   If :True, display some informations about the resulting objects as maximum, minimum and average altitude and the lower/higher ;
;                     top values of the list, depending on the value defined in the settings. Matricules are displayed too if you wanted it         ;
; Step n°6.b      :   If :False, asks the user for a file location to create a .csv file for every block reference to control their values          ;
; Step n°7        : Only block references with a difference between Altitude attribute value and Z coordinate value, or an Altitude attribute value ;
;                   equals to 0 will remains selected                                                                                               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaApp" ---> Excel:IsOpenedWorkBook                        | v1.1.0 - 19/09/2022 (Ranjit Singt/Luna)                                       ;
;   --•  "BaLst" ---> remove-duplicates                             | v2.0.0 - 29/04/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> sublist                                       | v1.1.0 - 24/06/2020 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;   --•  "UtCom" ---> SetVarList                                    | v2.0.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlCol" ---> vla-collection->list                          | v2.0.0 - 19/08/2021 (Luna)                                                    ;
;   --•  "VlObj" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.1.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> AlP-Settings-DCL                              | v2.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•    "BaErr" ---> *error*                                     | v1.1.0 - 08/08/2022 (Luna)                                                    ;
;   --•    "BaStr" ---> AlP-CheckAttributes                         | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "BaStr" ---> AlP-Check%                                  | v1.1.0 - 05/08/2022 (Luna)                                                    ;
;   --•    "DbTil" ---> AlP-init                                    | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "DbOpc" ---> AlP-Accept                                  | v1.1.0 - 05/08/2022 (Luna)                                                    ;
;   --•      "DbTil" ---> f                                         | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> AlP-get-MATALT                                | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "UtDis" ---> AlP-princ-param                               | v1.1.0 - 05/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (ALTPOINT) returns the minimum, maximum and average altitude for the selected block references and the lower/higher top list of     ;
;   values if the user want to modify the block references. If you only wanna check the value for each block references, a .csv file will be        ;
;   created.                                                                                                                                        ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.1.0   |   Add a question when creating a .csv file to open it or not at the end, using (startapp) and (Excel:IsOpenedWorkBook) functions | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Add the "ALTPOINT_Settings" environment variable to save them and add the possibility to display a fix number of values        | ;
; |            |   instead of a percentage value for the higher/lower top list (as a setting)                                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.2   |   Fix an issue when the Altitude is an empty string (attribute exists, but not values defined)                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.1   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Replace (select-filter) with (ssget) due to a significant slow down of the program, modify DIMZIN + LUPREC and use the         | ;
; |            |   function (vl-string-translate) for the .csv file to change the "." into ","                                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.2   |   Update (LgT) function to v1.1.0                                                                                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Update (ListBox) function to v4.0.0                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:ALTPOINT
  ( /
    *error* AlP-Settings-DCL AlP-get-MATALT AlP-princ-param
    DIMZIN LUPREC param break mode bloc-list layer-list jsel i name n mod-sel pt att-list ALT MAT lst filename file
  )
  (defun *error* (msg)
    (if file (close file))
    (setvar "DIMZIN" DIMZIN)
    (setvar "LUPREC" LUPREC)
    (sssetfirst)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun AlP-Settings-DCL (/ *error* AlP-CheckAttributes AlP-Check% AlP-Init AlP-Accept filename file DCL_ID bl al)
    (defun *error* (msg)
      (if file (close file))
      (if DCL_ID (unload_dialog DCL_ID))
      (if filename (vl-file-delete filename))
      (princ msg)
    )
    (defun AlP-CheckAttributes (str)
      (cond
        ( (vl-string-search str " ")
          (alert
            (LgT
              "The attribute(s) can't contained space (\" \")..."
              "Les attribut(s) ne peuvent pas contenir d'espace (\" \")..."
              nil
            )
          )
        )
        ( (/= str (lst2str (vl-remove nil (mapcar '(lambda (x) (if (member x (mapcar 'car al)) x)) (str2lst str ","))) ","))
          (alert
            (LgT
              "One or more attribute(s) doesn't correspond to the list of available attributes (use \",\" to separate each attribute name)..."
              "Un ou plusieurs attribut(s) ne corresponde(nt) pas à la liste des attribut(s) disponible(s) (utiliser \",\" pour séparer chaque nom d'attribut)..."
              nil
            )
          )
        )
      )
    )
    (defun AlP-Check% (str)
      (cond
        ( (null (setq str (distof str)))
          (alert
            (LgT
              "Only numbers are allowed..."
              "Uniquement les nombres sont autorisés..."
              nil
            )
          )
        )
        ( (minusp str)
          (alert
            (LgT
              "Only positive numbers are allowed..."
              "Uniquement les nombres positifs sont autorisés..."
              nil
            )
          )
        )
        ( (= "1" (get_tile "NBV")) str)
        ( (< 50 str)
          (alert
            (LgT
              "The value must be between 0 and 50..."
              "La valeur doit être comprise entre 0 et 50..."
              nil
            )
          )
        )
        (str)
      )
    )
    (defun AlP-Init (tile value flag / f)
      (setq f (null (cdr (assoc "LST" param))))
      (and
        (if flag (not f) f)
        (mode_tile tile value)
      )
    )
    (defun AlP-Accept (/ f)
      (defun f (str val)
        (if (= "1" (get_tile str)) val)
      )
      (list
        (cons "ALT" (get_tile "ALT"))
        (cons "MAT" (f "Toggle_MAT" (get_tile "MAT")))
        (cons "SEL" (f "SEL" T))
        (cons "CHK" (f "CHK" T))
        (cons "NBV" (f "NBV" T))
        (cons "%" (if (f "NBV" T) (atoi (get_tile "%")) (/ (atof (get_tile "%")) 100.0)))
        (assoc "LST" param)
      )
    )
    (setq
      filename (vl-filename-mktemp "ALTPOINT_SettingsBox.dcl")
      file (open filename "W")
    )
    (mapcar
      '(lambda (l) (write-line l file))
      (list
        "ALTPOINT_SettingsBox:dialog {"
        (strcat "  label = \"" (LgT "ALTPOINT : Command settings" "ALTPOINT : Paramètres de commande" nil) "\" ;")
        "  :row {"
        "    :column {"
        "      :boxed_column {"
        (strcat "        label = \"" (LgT "Attributes settings" "Paramètres des attributs" nil) "\" ;")
        "        :text {"
        "          label = \"Altitude: \" ;"
        "          alignment = \"right\" ;"
        "        }"
        "        :edit_box {"
        "          key = \"ALT\" ;"
        "          alignment = \"left\" ;"
        "        }"
        "        spacer ;"
        "        spacer ;"
        "        :toggle {"
        "          label = \"Matricule: \" ;"
        "          key = \"Toggle_MAT\" ;"
        "          alignment = \"right\" ;"
        "          value = \"1\" ;"
        "        }"
        "        :edit_box {"
        "          key = \"MAT\" ;"
        "          alignment = \"left\" ;"
        "        }"
        "      }"
        "      :boxed_column {"
        (strcat "        label = \"" (LgT "Selection settings" "Paramètres de sélection" nil) "\" ;")
        "        :toggle {"
        (strcat "          label = \"" (LgT "Select all block references from Model" "Sélectionner toutes les références de bloc en Objet" nil) "\" ;")
        "          key = \"SEL\" ;"
        "        }"
        "        :toggle {"
        (strcat "          label = \"" (LgT "Only controls values, without modifying properties" "Contrôle uniquement les valeurs, sans modifier les propriétés" nil) "\" ;")
        "          key = \"CHK\" ;"
        "        }"
        "        :radio_row {"
        "          :radio_button {"
        (strcat "            label = \"" (LgT "Percentage" "Pourcentage" nil) "\" ;")
        "            key = \"PCT\" ;"
        "          }"
        "          :radio_button {"
        (strcat "            label = \"" (LgT "Number of value(s)" "Nombre de valeur(s)" nil) "\" ;")
        "            key = \"NBV\" ;"
        "          }"
        "        }"
        "        :text{"
        "          key = \"TXT\" ;"
        "        }"
        "        :edit_box {"
        "          key = \"%\" ;"
        "        }"
        "      }"
        "    }"
        "    :boxed_column {"
        "      width = 42 ;"
        (strcat "      label = \"" (LgT "Block(s) informations" "Informations sur le(s) bloc(s)" nil) "\" ;")
        "      :list_box {"
        (strcat "        label = \"" (LgT "Block(s) list :" "Liste des bloc(s) :" nil) "\" ;")
        "        key = \"LST\" ;"
        "        height = 13 ;"
        "      }"
        "      :list_box {"
        (strcat "        label = \"" (LgT "Attribute(s) list :" "Liste des attribut(s) :" nil) "\" ;")
        "        key = \"ATT\" ;"
        "        height = 13 ;"
        "      }"
        "    }"
        "  }"
        "  spacer ;"
        "  ok_cancel ;"
        "}"
      )
    )
    (setq file (close file))
    (setq DCL_ID (load_dialog filename))
    (if (not (new_dialog "ALTPOINT_SettingsBox" DCL_ID))
      (exit)
    )
    (set_tile "ALT" (cond ((cdr (assoc "ALT" param))) ("")))
    (AlP-Init "ALT" 1 nil)
    (set_tile "Toggle_MAT" (if (cdr (assoc "MAT" param)) "1" "0"))
    (set_tile "MAT" (cond ((cdr (assoc "MAT" param))) ("")))
    (AlP-Init "MAT" 1 nil)
    (set_tile "SEL" (if (cdr (assoc "SEL" param)) "1" "0"))
    (set_tile "CHK" (if (cdr (assoc "CHK" param)) "1" "0"))
    (set_tile "NBV" (if (cdr (assoc "NBV" param)) "1" "0"))
    (set_tile "PCT" (if (cdr (assoc "NBV" param)) "0" "1"))
    (set_tile
      "TXT"
      (if (cdr (assoc "NBV" param))
        (LgT "Number of lower and higher top values (u)" "Nombre de valeurs inférieures et supérieures (u)" nil)
        (LgT "Lower and higher top percentage value (%)" "Valeur du pourcentage des valeurs inférieures et supérieures (%)" nil)
      )
    )
    (set_tile "%" (if (cdr (assoc "NBV" param)) (itoa (fix (cdr (assoc "%" param)))) (rtos (* (cdr (assoc "%" param)) 100) 2 1)))
    (setq bl
      (mapcar
        '(lambda (b / o a)
          (setq o (tblobjname "BLOCK" b))
          (setq o (entnext o))
          (while o
            (if (= "ATTDEF" (cdr (assoc 0 (entget o))))
              (setq a (cons (cdr (assoc 2 (entget o))) a))
            )
            (setq o (entnext o))
          )
          (cons b (reverse a))
         )
        (cdr (assoc "LST" param))
      )
    )
    (start_list "LST")
    (mapcar
      '(lambda (x)
        (add_list (strcat (car x) " [" (itoa (length (cdr x))) (LgT " attribute(s)" " attribut(s)" nil) "]"))
        (mapcar '(lambda (a) (add_list (strcat "  → " a))) (cdr x))
        (add_list "")
       )
      bl
    )
    (end_list)
    (setq al
      (mapcar
        '(lambda (a) (cons a (vl-remove nil (mapcar '(lambda (b) (if (member a b) (car b))) bl))))
        (remove-duplicates (apply 'append (mapcar 'cdr bl)))
      )
    )
    (start_list "ATT")
    (mapcar
      '(lambda (x)
        (add_list (strcat (car x) " [" (itoa (length (cdr x))) " / " (itoa (length bl)) (LgT " block(s)" " bloc(s)" nil) "]"))
        (mapcar '(lambda (b) (add_list (strcat "  → " b))) (cdr x))
        (add_list "")
       )
      al
    )
    (end_list)
    (action_tile "ALT" "(AlP-CheckAttributes $value)")
    (action_tile "MAT" "(AlP-CheckAttributes $value)")
    (action_tile "Toggle_MAT" "(AlP-Init \"MAT\" (- 1 (atoi $value)) T)")
    (action_tile "PCT" "(set_tile \"TXT\" (LgT \"Lower and higher top percentage value (%)\" \"Valeur du pourcentage des valeurs inférieures et supérieures (%)\" nil))")
    (action_tile "NBV" "(set_tile \"TXT\" (LgT \"Number of lower and higher top values (u)\" \"Nombre de valeurs inférieures et supérieures (u)\" nil))")
    (action_tile "%" "(AlP-Check% $value)")
    (action_tile "accept" "(setq param (AlP-Accept)) (done_dialog)")
    (action_tile "cancel" "(done_dialog)")
    (start_dialog)
    (unload_dialog DCL_ID)
    (setenv
      "ALTPOINT_Settings"
      (lst2str (list (apply '+ (mapcar '(lambda (b s) (if (cdr (assoc s param)) b 0)) '(1 2 4) '("SEL" "CHK" "NBV"))) (cdr (assoc "%" param))) ",")
    )
    (vl-file-delete filename)
    param
  )
  (defun AlP-get-MATALT (val lst)
    (cond
      ( (null val) nil)
      ( (assoc (car val) lst))
      ( (AlP-get-MATALT (cdr val) lst))
    )
  )
  (defun AlP-princ-param ()
    (princ
      (strcat
        (LgT
          "\nCurrent settings: Select all = "
          "\nParamètres courant: Sélectionner tout = "
          nil
        )
        (if (cdr (assoc "SEL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil))
        (LgT
          "  Block(s) modification = "
          "  Modification des bloc(s) = "
          nil
        )
        (if (cdr (assoc "CHK" param)) (LgT "No" "Non" nil) (LgT "Yes" "Oui" nil))
        (if (cdr (assoc "NBV" param)) (LgT "  Number of values = " "  Nombre de valeurs = " nil) (LgT "  Percentage = " "  Pourcentage = " nil))
        (if (cdr (assoc "NBV" param))
          (strcat (itoa (fix (cdr (assoc "%" param)))) "u")
          (strcat (rtos (* 100.0 (cdr (assoc "%" param))) 2 1) "%")
        )
        "\nAltitude = " (cond ((cdr (assoc "ALT" param))) (""))
        "  Matricule = " (cond ((cdr (assoc "MAT" param))) ((LgT "<none>" "<aucun>" nil)))
        (LgT "  Layer(s) = " "  Calque(s) = " nil) (cond ((lst2str layer-list ", ")) ("*"))
        (LgT
          "\nBlock(s) list = "
          "\nListe des bloc(s) = "
          nil
        )
        (cond ((lst2str (cdr (assoc "LST" param)) ", ")) (""))
      )
    )
    nil
  )

  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("DIMZIN" DIMZIN 0) ("LUPREC" LUPREC 2)))
  (cond
    ( (getenv "ALTPOINT_Settings") (setq param (str2lst (getenv "ALTPOINT_Settings") ",")))
    ( (setq param (str2lst (setenv "ALTPOINT_Settings" (lst2str (list (apply '+ '(1 2 4)) 24) ",")) ",")))
  )
  (setq param
    (list
      (cons "ALT" "ALT")
      (cons "MAT" "MAT")
      (cons "SEL" (bit 1 (atoi (car param))))
      (cons "CHK" (bit 2 (atoi (car param))))
      (cons "NBV" (bit 4 (atoi (car param))))
      (cons "%" (atof (cadr param)))
      (cons "LST" '())
    )
  )
  (while
    (and
      (null break)
      (null (AlP-princ-param))
      (setq mode
        (getkdh
          (quote (entsel msg))
          (LgT
            "\nSelect a block or"
            "\nSélectionner un bloc ou"
            nil
          )
          (list
            (LgT
              "TCpoint List Settings eXit _TCpoint List Settings eXit"
              "TCpoint Liste Paramètres Quitter _TCpoint List Settings eXit"
              nil
            )
          )
          " : "
          "eXit"
          (LgT
            (strcat
              "\nALTPOINT : Selection mode for blocks"
              "\nDefault value:     eXit"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Select    | Select manually a block with attributes corresponding to the        |"
              "\n  |             | defined settings. You can select one or more blocks                 |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   TCpoint   | Selects only blocks named \"TCPOINT\", usually used for topographic   |"
              "\n  |             | datas, with 2 attributes named \"ALT\" and \"MAT\"                      |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    List     | Opens a dialog box with the list of all block's name and let you    |"
              "\n  |             | select one or more blocks within the list                           |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Opens a dialog box to set the settings for the command as the       |"
              "\n  |  Settings   | name of the attribute for the altitude and the matricule, if you    |"
              "\n  |             | want to select all occurences or select them manually, and if you   |"
              "\n  |             | want to check the topographic points or correct them                |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            (strcat
              "\nALTPOINT : Mode de sélection des blocs"
              "\nValeur par défaut: Quitter"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Vous sélectionnez manuellement un bloc possédant des attributs      |"
              "\n  |  Sélection  | correspondant aux paramètres définis. Vous pouvez sélectionner un   |"
              "\n  |             | ou plusieurs blocs                                                  |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Sélectionne uniquement les blocs nommés \"TCPOINT\", habituellement   |"
              "\n  |   TCpoint   | utilisés pour les données topographiques, possédant 2 attributs     |"
              "\n  |             | nommés \"ALT\" et \"MAT\" (remplace la liste de bloc existante)         |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    Liste    | Ouvre une boîte de dialogue avec la liste de l'ensemble des noms de |"
              "\n  |             | blocs et vous permet de sélectionner un ou plusieurs blocs          |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Ouvre une boîte de dialogue pour définir les paramètres de la       |"
              "\n  |             | commande tels que le nom des attributs pour l'altitude et le        |"
              "\n  | Paramètres  | matricule, si vous souhaitez sélectionner toutes les références de  |"
              "\n  |             | blocs ou bien les sélectionner manuellement, et si vous désirez     |"
              "\n  |             | simplement vérifier les points topographiques ou bien les corriger  |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            nil
          )
        )
      )
    )
    (cond
      ( (= "TCpoint" mode)
        (setq
          break T
          bloc-list '("TCPOINT*")
          param (subst (cons "LST" bloc-list) (assoc "LST" param) param)
        )
      )
      ( (= "List" mode)
        (setq
          bloc-list
            (ListBox
              (LgT
                "ALTPOINT: Block(s) selection"
                "ALTPOINT: Sélection des bloc(s)"
                nil
              )
              (LgT
                "Please, select one or more block(s) :"
                "Veuillez sélectionner un ou plusieurs bloc(s) :"
                nil
              )
              (vl-sort (vl-remove-if '(lambda (x) (wcmatch x "`**")) (mapcar 'car (vla-collection->list nil 'blocks 1))) '<)
              nil
              2
              nil
            )
          param (subst (cons "LST" bloc-list) (assoc "LST" param) param)
        )
      )
      ( (and
          (listp mode)
          (setq mode (vlax-ename->vla-object (car mode)))
          (= "AcDbBlockReference" (vlax-get mode 'ObjectName))
          (vlax-property-available-p mode 'EffectiveName)
          (= -1 (vlax-get mode 'HasAttributes))
        )
        (if (not (member (vlax-get mode 'EffectiveName) bloc-list))
          (setq
            bloc-list (cons (vlax-get mode 'EffectiveName) bloc-list)
            param (subst (cons "LST" bloc-list) (assoc "LST" param) param)
          )
        )
        (if (not (member (vlax-get mode 'Layer) layer-list))
          (setq layer-list (cons (vlax-get mode 'Layer) layer-list))
        )
      )
      ( (= "Settings" mode)
        (setq param (AlP-Settings-DCL))
      )
      ( (= "eXit" mode) (setq break T))
      ( T (princ (LgT "\nInvalid selection..." "\nSélection invalide..." nil)))
    )
  )
  (if
    (and
      param
      (setq bloc-list (cdr (assoc "LST" param)))
      (setq jsel
        (apply
          'ssget
          (vl-remove
            nil
            (list
              (if (cdr (assoc "SEL" param)) "_X")
              (list
                '(0 . "INSERT")
                '(66 . 1)
                '(410 . "Model")
                (cons 2 (lst2str bloc-list ","))
                (if layer-list (cons 8 (lst2str layer-list ",")) '(8 . "*"))
              )
            )
          )
        )
      )
    )
    (progn
      (repeat (setq n 0 mod-sel (ssadd) i (sslength jsel))
        (and
          (setq name (ssname jsel (setq i (1- i))))
          (setq pt (cdr (assoc 10 (entget name))))
          (setq att-list (get-att-list name))
          (setq
            MAT (if (cdr (assoc "MAT" param)) (AlP-get-MATALT (str2lst (cdr (assoc "MAT" param)) ",") att-list))
            ALT (cdr (AlP-get-MATALT (str2lst (cdr (assoc "ALT" param)) ",") att-list))
          )
          (setq lst
            (cons
              (cons
                (cond ((cdr MAT)) (""))
                (list
                  (atof ALT)
                  pt
                  (vlax-get (vlax-ename->vla-object name) 'EffectiveName)
                  (cond ((car MAT)) (""))
                  (cdr (assoc 5 (entget name)))
                )
              )
              lst
            )
          )
          (if
            (or
              (null (distof ALT))
              (not (equal (last pt) (distof ALT) 1E-3))
              (zerop (distof ALT))
            )
            (ssadd name mod-sel)
            T
          )
          (null (cdr (assoc "CHK" param)))
          (not (null (distof ALT)))
          (entmod
            (subst
              (cons 10 (list (car pt) (cadr pt) (distof ALT)))
              (assoc 10 (entget name))
              (entget name)
            )
          )
          (setq n (1+ n))
        )
      )
      (setq
        lst (vl-sort lst '(lambda (a1 a2) (< (cadr a1) (cadr a2))))
        i
          (if (cdr (assoc "NBV" param))
            (if (< (* 2 (cdr (assoc "%" param))) (length lst))
              (fix (cdr (assoc "%" param)))
              (/ (length lst) 2)
            )
            (atoi (rtos (* (length lst) (cdr (assoc "%" param))) 2 0))
          )
      )
      (sssetfirst)
      (if (null (cdr (assoc "CHK" param)))
        (princ
          (strcat
            (LgT
              "\nA total of "
              "\nUn total de "
              nil
            )
            (itoa n)
            " / "
            (itoa (sslength jsel))
            (LgT
              (strcat
                " block(s) have been redefined successfully."
                "\nHere's an overview of the results :"
                "\n  - Maximum altitude = "
              )
              (strcat
                " bloc(s) ont été redéfinis avec succès."
                "\nVoici un aperçu des résultats :"
                "\n  - Altitude maximale = "
              )
              nil
            )
            (rtos (cadr (last lst)) 2 2)
            "m"
            (if (cdr (assoc "MAT" param))
              (strcat
                " (MAT: "
                (lst2str (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cadr x) (cadr (last lst)))) lst)) ", ")
                ")"
              )
              ""
            )
            (LgT
              "\n  - Minimum altitude = "
              "\n  - Altitude minimale = "
              nil
            )
            (rtos (cadar lst) 2 2)
            "m"
            (if (cdr (assoc "MAT" param))
              (strcat
                " (MAT: "
                (lst2str (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cadr x) (cadar lst))) lst)) ", ")
                ")"
              )
              ""
            )
            (LgT
              "\n  - Average altitude = "
              "\n  - Altitude moyenne  = "
              nil
            )
            (rtos (/ (apply '+ (mapcar 'cadr lst)) (float (length lst))) 2 2)
            "m"
            (if (not (zerop (cdr (assoc "%" param))))
              (strcat
                (if (cdr (assoc "NBV" param))
                  (LgT
                    (strcat "\nList of " (itoa i) " values in the lower top ")
                    (strcat "\nListe des " (itoa i) " valeurs inférieures")
                    nil
                  )
                  (strcat
                    (LgT
                      "\nList of values in the lower top "
                      "\nListe des valeurs inférieures à "
                      nil
                    )
                    (rtos (* 100 (cdr (assoc "%" param))) 2 1)
                    "% (" (itoa i) "u)"
                  )
                )
                " :"
                (apply
                  'strcat
                  (mapcar
                    '(lambda (x / c)
                      (setq c (vl-position x (sublist lst 1 i)))
                      (strcat
                        (if (= 0 (rem c 3)) "\n|  " ", ")
                        (rtos (cadr x) 2 2)
                        "m"
                        (if (cdr (assoc "MAT" param))
                          (strcat
                            " (" (cadddr (cdr x)) ": "
                            (car x)
                            ")"
                          )
                          ""
                        )
                      )
                    )
                    (sublist lst 1 i)
                  )
                )
                "\n|"
                (if (cdr (assoc "NBV" param))
                  (LgT
                    (strcat "\nList of " (itoa i) " values in the higher top ")
                    (strcat "\nListe des " (itoa i) " valeurs supérieures")
                    nil
                  )
                  (strcat
                    (LgT
                      "\nList of values in the higher top "
                      "\nListe des valeurs supérieures à "
                      nil
                    )
                    (rtos (* 100 (cdr (assoc "%" param))) 2 1)
                    "% (" (itoa i) "u)"
                  )
                )
                " :"
                (apply
                  'strcat
                  (mapcar
                    '(lambda (x / c)
                      (setq c (vl-position x (sublist lst (- (length lst) i) nil)))
                      (strcat
                        (if (= 0 (rem c 3)) "\n|  " ", ")
                        (rtos (cadr x) 2 2)
                        "m"
                        (if (cdr (assoc "MAT" param))
                          (strcat
                            " (" (cadddr (cdr x)) ": "
                            (car x)
                            ")"
                          )
                          ""
                        )
                      )
                    )
                    (sublist lst (- (length lst) i) nil)
                  )
                )
              )
            )
          )
        )
        (and
          (setq filename
            (getfiled
              (LgT
                "Selection of .csv export file"
                "Sélection du fichier .csv d'export"
                nil
              )
              (strcat (getvar "DWGPREFIX") (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "_ALTPOINT Export")
              "csv"
              1
            )
          )
          (cond
            ( (setq file (Excel:IsOpenedWorkBook filename))
              (vla-Close (cdr file) :vlax-false)
              (princ
                (LgT
                  (strcat "\nThe file " (car file) " was opened. It will be closed and overwritten...")
                  (strcat "\nLe fichier " (car file) " était ouvert. Il sera fermé et réécrit...")
                  nil
                )
              )
            )
            ( (not file))
          )
          (setq file (open filename "W"))
          (write-line "Handle;Block;Attribute;Matricule;X;Y;Z;Altitude;Delta" file)
          (mapcar
            '(lambda (x / h b e m p x y z a)
              (setq
                m (car x)
                h (cdr x)
                a (car h)
                p (cadr h)
                b (caddr h)
                e (cadddr h)
                h (last h)
                x (car p)
                y (cadr p)
                z (last p)
              )
              (write-line
                (strcat
                  "'"
                  h
                  ";"
                  b
                  ";"
                  e
                  ";"
                  m
                  ";"
                  (vl-string-translate "." "," (rtos x 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos y 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos z 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos a 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos (- a z) 2 2))
                )
                file
              )
             )
            lst
          )
          (not (setq file (close file)))
          (not (alert (LgT "The file has been created successfully !" "Le fichier a été créé avec succès !" nil)))
          (princ
            (strcat
              (LgT
                "\nFile name: "
                "\nNom du fichier: "
                nil
              )
              filename
            )
          )
          (=
            "Yes"
            (getkdh
              (quote (getkword msg))
              (LgT
                "\nWould you like to open the export file ?"
                "\nSouhaitez-vous ouvrir le fichier d'export ?"
                nil
              )
              (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
              " "
              "Yes"
              nil
            )
          )
          (startapp "C:\\Program Files\\Microsoft Office\\root\\Office16\\Excel.exe" (strcat "\"" filename "\""))
        )
      )
      (sssetfirst nil mod-sel)
    )
  )
  (SetVarList '(("DIMZIN" nil DIMZIN) ("LUPREC" nil LUPREC)))
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)