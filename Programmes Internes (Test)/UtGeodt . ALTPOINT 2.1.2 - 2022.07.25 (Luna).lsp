
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  ALTPOINT  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ALTPOINT []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 25/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.1.2                                                                                                                 ;
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
;   --•  "UtUse" ---> AlP-Settings-DCL                              | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "BaErr" ---> *error*                                     | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "BaStr" ---> AlP-CheckAttributes                         | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "BaStr" ---> AlP-Check%                                  | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "DbTil" ---> AlP-init                                    | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•    "DbOpc" ---> AlP-Accept                                  | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•      "DbTil" ---> f                                         | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> AlP-get-MATALT                                | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "UtDis" ---> AlP-princ-param                               | v1.0.0 - 27/04/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (ALTPOINT) returns the minimum, maximum and average altitude for the selected block references and the lower/higher top list of     ;
;   values if the user want to modify the block references. If you only wanna check the value for each block references, a .csv file will be        ;
;   created.                                                                                                                                        ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
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
    DIMZIN LUPREC param break mode bloc-list jsel i name n mod-sel pt att-list ALT MAT lst filename file
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
        ( (zerop str)
          (alert
            (LgT
              "The value can't be set to zero..."
              "La valeur ne peut pas être définie à zéro..."
              nil
            )
          )
        )
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
        (cons "%" (/ (distof (get_tile "%")) 100.0))
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
        "        spacer ;"
        "        :edit_box {"
        (strcat "          label = \"" (LgT "Lower and higher top percentage value (%)" "Valeur du pourcentage des valeurs inférieures et supérieures (%)" nil) "\" ;")
        "          key = \"%\" ;"
        "          width = 2 ;"
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
        "        is_enabled = false ;"
        "      }"
        "      :list_box {"
        (strcat "        label = \"" (LgT "Attribute(s) list :" "Liste des attribut(s) :" nil) "\" ;")
        "        key = \"ATT\" ;"
        "        height = 13 ;"
        "        is_enabled = false ;"
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
    (set_tile "%" (rtos (* (cdr (assoc "%" param)) 100) 2 1))
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
    (action_tile "%" "(AlP-Check% $value)")
    (action_tile "accept" "(setq param (AlP-Accept)) (done_dialog)")
    (action_tile "cancel" "(done_dialog)")
    (start_dialog)
    (unload_dialog DCL_ID)
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
        (LgT
          "  Percentage = "
          "  Pourcentage = "
          nil
        )
        (rtos (* 100.0 (cdr (assoc "%" param))) 2 1) "%"
        "\nAltitude = " (cond ((cdr (assoc "ALT" param))) (""))
        "  Matricule = " (cond ((cdr (assoc "MAT" param))) ((LgT "<none>" "<aucun>" nil)))
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
  (setq param
    (list
      (cons "ALT" "ALT")
      (cons "MAT" "MAT")
      (cons "SEL" T)
      (cons "CHK" nil)
      (cons "%" 0.05)
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
              "\n  |             | nommés \"ALT\" et \"MAT\"                                               |"
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
        i (atoi (rtos (* (length lst) (cdr (assoc "%" param))) 2 0))
      )
      (sssetfirst nil jsel)
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
            (LgT
              "\nList of values in the lower top "
              "\nListe des valeurs inférieures à "
              nil
            )
            (rtos (* 100 (cdr (assoc "%" param))) 2 1)
            "% ("
            (itoa i)
            "u) :"
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
            (LgT
              "\nList of values in the higher top "
              "\nListe des valeurs supérieures à "
              nil
            )
            (rtos (* 100 (cdr (assoc "%" param))) 2 1)
            "% ("
            (itoa i)
            "u) :"
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
        (if
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
          )
          (princ
            (strcat
              (LgT
                "File name: "
                "Nom du fichier: "
                nil
              )
              filename
            )
          )
        )
      )
      (sssetfirst nil mod-sel)
    )
  )
  (SetVarList '(("DIMZIN" nil DIMZIN) ("LUPREC" nil LUPREC)))
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                          --{  remove-duplicates  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] remove-duplicates []-----------------------[]                                     ;
;--- Date of creation       > 18/02/2020                                                                                                            ;
;--- Last modification date > 29/04/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to delete duplicates within a list.                                                                                                  ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (remove-duplicates) have 1 argument(s) :                                                                                             ;
;   --•  lst                    > corresponds to the list we want to modify                                                                         ;
;     (type lst) = 'LST                         | Ex. : '("A" "B" "A" "C" "D" "F" "B" "D" "E" "C" "A" "F" "E"), ...                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> DXF_List                                      | v1.2.2 - 06/05/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (remove-duplicates) returns the modified list from which all duplicate values have been removed, except for the first occurrence.  ;
;   The returned list keeps the order of appearance of the values with respect to their first occurrence.                                           ;
;     Ex. : (remove-duplicates '("A" "B" "A" "C" "D" "F" "B" "D" "E" "C" "A" "F" "E")) returns ("A" "B" "C" "D" "F" "E")                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Using recursive method to remove duplicates and changing the name of the function                                              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun remove-duplicates (lst)
  (if lst
    (cons (car lst) (remove-duplicates (vl-remove (car lst) (cdr lst))))
  )
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  sublist  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] sublist []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Returns a sub-list of a list from the position of an element and a specified length. This is the equivalent function to (substr) but for a list.;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (sublist) have 3 argument(s) :                                                                                                       ;
;   --•  lst                    > corresponds to the list we want to cut                                                                            ;
;     (type lst) = 'LST                         | Ex. : '(1 2 3 4 1 3 7 5 8 1 3 6 8), '("label1" "label2" "label3" "label4"), ...                   ;
;   --•  s                      > corresponds to the starting position of the list (positive value). The first list element is index 1              ;
;     (type s) = 'INT                           | Ex. : 1, 5, 52, ...                                                                               ;
;   --•  l                      > is the length of the desired sub-list. If nil, the sub-list continues to the end of the list                      ;
;     (type l) = 'INT                           | Ex. : 2, 10, nil, ...                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> divlist                                       | v1.0.0 - 19/06/2020 (Luna)                                                    ;
;   --•  "DtObj" ---> Add-Poly2D-point                              | v2.0.0 - 10/05/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (sublist) returns the sub-list according to the input data, nil if s is greater than the length of the list.                       ;
;     Ex. : (sublist '(1 2 3 4 5 6 7 8 9) 3 5) returns '(3 4 5 6 7)                                                                                 ;
;           (sublist '(1 2 3 4 5 6 7 8 9) 3 nil) returns '(3 4 5 6 7 8 9)                                                                           ;
;           (sublist '(1 2 3 8 2 4 6 3 7 2 1 8 9) 5 5) returns '(2 4 6 3 7)                                                                         ;
;           (sublist '("label1" "label2" "label3") 4 nil) returns nil                                                                               ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Removal of the (member) and (vl-remove-if) functions, sources of problems when repeating identical elements in the list and    | ;
; |            |   removal of the redefinition of the 'start' variable to 1. Modification of the naming of variables and arguments                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun sublist (lst s l)
  (repeat (1- s) (setq lst (cdr lst)))
  (setq lst (reverse lst))
  (if
    (or
      (null l)
      (minusp l)
      (not (<= l (- (length lst) (1- s))))
    )
    lst
    (repeat (- (length lst) l)
      (setq lst (cdr lst))
    )
  )
  (reverse lst)
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  lst2str  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] lst2str []-----------------------[]                                          ;
;--- Date of creation       > 05/03/2021                                                                                                            ;
;--- Last modification date > 01/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a list of items into string format by adding a separator between each item in the list. It's just the inverse function of (str2lst).   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (lst2str) have 2 argument(s) :                                                                                                       ;
;   --•  lst                    > corresponds to the list of elements you want to convert into a string                                             ;
;     (type lst) = 'LST                         | Ex. : (layoutlist), '(0 1 2 3 4 5 6 7 8 9), ...                                                   ;
;   --•  sep                    > corresponds to the string separator you want to add between each item in the list                                 ;
;     (type sep) = 'STR                         | Ex. : "", " ", ",", "/", ...                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> Select-Filter                                 | v3.2.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.3 - 08/07/2022 (Luna)                                                    ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (lst2str) returns a string with each element of the 'lst' concatened by a string separator, nil if 'lst' is null.                  ;
;     Ex. : (lst2str (layoutlist) ",") returns "Model,Layout1,Layout2"                                                                              ;
;           (lst2str '("Ceci" "est" "un" "test" "!") " ") returns "Ceci est un test !"                                                              ;
;           (lst2str '("A" 0 "B" 1) " = ") returns "A = 0 = B = 1"                                                                                  ;
;           (lst2str nil " ") returns nil                                                                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Checks if 'lst' is not null and returns nil if false                                                                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun lst2str (lst sep)
  (if lst
    (vl-string-left-trim
      sep
      (apply
        'strcat
        (mapcar '(lambda (x) (strcat sep (vl-princ-to-string x))) lst)
      )
    )
  )
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  str2lst  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] str2lst []-----------------------[]                                          ;
;--- Date of creation       > 15/04/2017                                                                                                            ;
;--- Last modification date > 15/04/2017                                                                                                            ;
;--- Author                 > (gile)                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a string representing a list of items into list format by using a separator detection in the string. If the separator is not found,    ;
;   the list will only contains one element.                                                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (str2lst) have 2 argument(s) :                                                                                                       ;
;   --•  str                    > corresponds to the string representing a list of elements you want to convert into a list                         ;
;     (type str) = 'STR                         | Ex. : "label1,label2,label3", "Scale Lineweight Length Width", ...                                ;
;   --•  sep                    > corresponds to the string separator that will be use to separate each element from the string into the list       ;
;     (type sep) = 'STR                         | Ex. : "", " ", ",", "/", ...                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> Select-Filter                                 | v3.2.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (str2lst) returns a list with each element detected in the 'str' each time a separator is found in the string.                     ;
;     Ex. : (str2lst "label1,label2,label3" ",") returns '("label1" "label2" "label3")                                                              ;
;           (str2lst "Scale Lineweight Length Width" " ") returns '("Scale" "Lineweight" "Length" "Width")                                          ;
;           (str2lst "label1,label2,label3" "/") returns '("label1,label2,label3")                                                                  ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons
      (substr str 1 pos)
      (str2lst (substr str (+ (strlen sep) pos 1)) sep)
    )
    (list str)
  )
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  SetVarList  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] SetVarList []-----------------------[]                                        ;
;--- Date of creation       > 13/06/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Gets and/or sets a list of system variables values in a specific order and can store current value into AutoLISP variables.                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (SetVarList) have 1 argument(s) :                                                                                                    ;
;   --•  lst                    > correspond to the list of system variables, in a specific format (VarName VarSym Value). Each element of the list ;
;                               is a list of 3 elements:                                                                                            ;
;                                 VarName,  is the name of the system variable you want to get and/or modify, as a string                           ;
;                                 VarSym,   is the AutoLISP variable name you want to use to store the current value of VarName (nil if you don't   ;
;                                           want to store its value)                                                                                ;
;                                 Value,    is the new value you want to set VarName (nil if you don't want to set a new value)                     ;
;     (type lst) = 'LST                         | Ex. : '(("CTAB" nil "Layout1") ("PSLTSCALE" nil 0)), (list (list "CMDECHO" 'ce 0)), ...           ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (SetVarList) returns the same list as entered with some changes for VarSym and Value. If VarSym is not a symbol (type), a variable ;
;   name to store the current value, VarSym will be replaced by the current value of the system variable. If Value is not set, it will be replaced  ;
;   by the current value of the system variable. If Value is set but the new value generates an error, it will be replaced by the error entity. You ;
;   can use the function (vl-catch-all-error-message) to know precisely why it didn't work.                                                         ;
;     Ex. : (SetVarList (list (list "CTAB" nil nil))) returns (("CTAB" "Model" "Model"))                                                            ;
;           (SetVarList '(("ctab" nil "Layout1"))) returns (("CTAB" "Model" "Layout1"))                                                             ;
;           (SetVarList '(("CTAB" ct "Layout3") ("CmdEcho" ce nil))) returns (("CTAB" CT #<%catch-all-apply-error%>) ("CMDECHO" CE 1)) with !CT     ;
;           equals "Layout1" and !CE equals 1.                                                                                                      ;
;           (SetVarList '(("CTA" nil "Model"))) returns (("CTA" nil #<%catch-all-apply-error%>))
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Checking if VarSym is a symbol or not, adding the error-handling with (setvar) and adding (strcase) for system variable name   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun SetVarList (lst)
  (mapcar
    '(lambda (x / var sym val)
      (setq
        var (car x)
        sym (cadr x)
        val (caddr x)
      )
      (if (vl-symbolp sym)
        (set sym (getvar var))
        (setq sym (getvar var))
      )
      (if val
        (setq val (vl-catch-all-apply 'setvar (list var val)))
        (setq val (getvar var))
      )
      (list (strcase var) sym val)
     )
    lst
  )
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  getkdh  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] getkdh []-----------------------[]                                          ;
;--- Date of creation       > 26/01/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.2.0                                                                                                                 ;
;--- Class                  > "UtUse"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Makes a user request using the getXXX functions that honor keywords (getint, getreal, getdist, getangle, getorient, getpoint, getcorner,        ;
;   getkword, entsel, nentsel and nentselp). You can set a default value if you want and display or not a message (= "?" as keyword) to help the    ;
;   users if needed.                                                                                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (getkdh) have 6 argument(s) :                                                                                                        ;
;   --•  fun                    > is the (quote) list of the getXXX functions (see above for the list of working functions) and have to be set like ;
;                               this -> (quote (getXXX [] msg [])), with :                                                                          ;
;                                       the (quote) function which is necessary for good working of the program                                     ;
;                                       the getXXX function corresponding to one of the authorized functions which work with (initget)              ;
;                                       the [] corresponding to the optional arguments of the specified getXXX function                             ;
;                                       and MSG which is also necessary to be define like this for good working of the program                      ;
;     (type fun) = 'LST                         | Ex. : (quote (getkword msg)), (quote (getpoint (0.0 0.0 0.0) msg)), (quote (nentselp msg pt)), ...;
;   --•  pfx                    > is the prefix of the [msg] for (getXXX) function, and will be prompt before the keyword list                      ;
;     (type pfx) = 'STR                         | Ex. : "\nQuel format d'écriture souhaitez vous", "\nWhich writing format do you want", nil, ...   ;
;   --•  arg                    > is the list of arguments for the initget function, like ([bit] [keywords]) (cf. (initget) help page)              ;
;     (type arg) = 'LST                         | Ex. : '(1 "Select "), '(), nil, '(5), '("annUler ? _Undo ?"),  ...                                ;
;   --•  sfx                    > is the suffix of the [msg] for (getXXX) function, and will be prompt after the keyword list and/or default value  ;
;     (type sfx) = 'STR                         | Ex. : "? ", ": ", nil, ...                                                                        ;
;   --•  dft                    > is the default value (if specified) used as a keyword, allowing the user to press ENTER to select it. The default ;
;                               as no needs to be one of the specified keyword, but if it does, it will returns the language-independent value (if  ;
;                               specified)                                                                                                          ;
;     (type dft) = ANY                          | Ex. : 1, "Simplified", '(0 . "INSERT"), "eXit", nil, ...                                          ;
;   --•  hlp                    > is the message you want to display in the command line historic to help user to understand the issue for each     ;
;                               keyword.                                                                                                            ;
;     (type hlp) = 'STR or 'LST                 | Ex. : "", "\nSimplified : ... \nDetailed", '(help "" "PSLTSCALE"), nil, ...                       ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "UtUse" ---> get                                           | v1.0.0 - 02/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.3 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtObjet" ---> C:GETLAYER                                  | v3.0.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (getkdh) returns the value specified by user. If the 'hlp' argument is specified and the user choose this value, the function will ;
;   display the help message and ask again for a keyword different of "?".                                                                          ;
;     Ex. : (getkdh (quote (getkword msg)) "\nWhich format do you want" '("A4 A3 A2 A0") " : " nil "\nHelp message...")                             ;
;           returns "A4", "A3", "A2" or "A0" if the user choose one of those or nil if the user press ENTER. If the user choose "?", the program    ;
;           will ask again the same question after displaying the help message. See below for the exemple of output in the command line :           ;
;     command: Which format do you want [A4/A3/A2/A0/?] : A1                                                                                        ;
;     command: Incorrect choice of option.                                                                                                          ;
;     command: Which format do you want [A4/A3/A2/A0/?] : ?                                                                                         ;
;     command: Help message...                                                                                                                      ;
;     command: Which format do you want [A4/A3/A2/A0/?] : A3                                                                                        ;
;     command: "A3"                                                                                                                                 ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.2.0   |   Extending the usability of 'hlp' to can be used as a string or quoted function aswell                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Extending the usability of 'dft' to be any type of value to match properly with all the getXXX function's value type           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Changing the function name, deleting the use of (LgT) function internally and extending the usability with every getXXX        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Correcting the 'hlp' argument usability with (LgT)                                                                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun getkdh (fun pfx arg sfx dft hlp / get bit kwd msg val)
  (defun get (msg / v)
    (apply 'initget arg)
    (if (null (setq v (apply (car fun) (vl-remove nil (mapcar '(lambda (x) (if (vl-symbolp x) (vl-symbol-value x) x)) (cdr fun))))))
      (setq v (cdr dft))
      v
    )
  )
  
  (and
    (member (car fun) (list 'getint 'getreal 'getdist 'getangle 'getorient 'getpoint 'getcorner 'getkword 'entsel 'nentsel 'nentselp))
    (= 'STR (type (cond (pfx) (""))) (type (cond (sfx) (""))))
    (listp arg)
    (if (null (setq bit (car (vl-remove-if-not '(lambda (x) (= 'INT (type x))) arg)))) (setq bit 0) bit)
    (if (null (setq kwd (car (vl-remove-if-not '(lambda (x) (= 'STR (type x))) arg))))
      (not kwd)
      (if (vl-string-search "_" kwd)
        (setq kwd (mapcar 'cons (str2lst (car (str2lst kwd "_")) " ") (str2lst (cadr (str2lst kwd "_")) " ")))
        (setq kwd (mapcar 'cons (str2lst kwd " ") (str2lst kwd " ")))
      )
    )
    (if hlp
      (if (not (assoc "?" kwd))
        (setq kwd (append kwd '(("?" . "?"))))
        T
      )
      T
    )
    (cond
      ( (null dft) (not dft))
      ( (member dft (mapcar 'car kwd))
        (setq dft (assoc dft kwd))
      )
      ( (member dft (mapcar 'cdr kwd))
        (setq dft (nth (vl-position (car (member dft (mapcar 'cdr kwd))) (mapcar 'cdr kwd)) kwd))
      )
      ( T (setq dft (cons (vl-princ-to-string dft) dft)))
    )
    (if (and dft (= 1 (logand 1 bit))) (setq bit (1- bit)) T)
    (setq arg (vl-remove nil (list bit (lst2str (vl-remove nil (list (lst2str (mapcar 'car kwd) " ") (lst2str (mapcar 'cdr kwd) " "))) "_"))))
    (if (or pfx kwd dft sfx)
      (setq
        msg
          (strcat
            (cond (pfx) (""))
            (if kwd (strcat " [" (lst2str (mapcar 'car kwd) "/") "]") "")
            (if dft (strcat " <" (car dft) ">") "")
            (cond (sfx) (""))
          )
      )
      (not (setq msg nil))
    )
    (if hlp
      (while (= "?" (setq val (get msg)))
        (cond
          ( (listp hlp) (eval hlp))
          ((princ hlp))
        )
      )
      (setq val (get msg))
    )
  )
  val
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                 --{  LgT  }--                                                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] LgT []-----------------------[]                                            ;
;--- Date of creation       > 10/01/2022                                                                                                            ;
;--- Last modification date > 12/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtWin"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Depending on the value of "LOCALE" system variable ("FR" or "EN", equals any other language), it will returns the french version of a string or ;
;   the english version. There's no possibilities to translate in a smart way any string, so you have to specify the french and english version each;
;   time you wanna use this function properly. You can force the prompting if you want.                                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LgT) have 3 argument(s) :                                                                                                           ;
;   --•  en                     > represents the english version of the string to prompt                                                            ;
;     (type en) = 'STR                          | Ex. : "Select object", ...                                                                        ;
;   --•  fr                     > represents the french version of the string to prompt                                                             ;
;     (type fr) = 'STR                          | Ex. : "Sélectionner un objet", ...                                                                ;
;   --•  flag                   > controls if it will check the value of "LOCALE" system variable, or force the english / french version            ;
;     (type flag) = 'INT                        | Ex. : 0 or anything (= if "LOCALE" equals "FR", prompt 'fr', prompt 'en' otherwise),              ;
;                                                       1 (= prompt 'en') or 2 (= prompt 'fr'), nil (= check the value of (getenv "FORCEDLANGUAGE"));
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.3 - 08/07/2022 (Luna)                                                    ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsLaytb" ---> c:QuickLayoutSwitch                         | v3.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtObjet" ---> C:GETLAYER                                  | v3.0.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "BbBound" ---> c:PreViewBoundingBox                        | v1.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "BbSelct" ---> c:MID_MOVE                                  | v2.0.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LgT) returns the french or english version of the string depending of the 'flag' value and the "LOCALE" value (if 'flag' = 0).    ;
;     Ex. : (LgT "Select object" "Sélection d'un objet" 0) returns "Sélection d'un objet" if "LOCALE" equals "FR", "Select object" otherwise        ;
;           (LgT "Select object" "Sélection d'un objet" 1) returns "Select object"                                                                  ;
;           (LgT "Select object" "Sélection d'un objet" 2) returns "Sélection d'un objet"                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Add the possibility of nil for 'flag' value, to use the environment variable "FORCEDLANGUAGE" value                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LgT (en fr flag)
  (cond
    ( (= flag 1) en)
    ( (= flag 2) fr)
    ( T (LgT en fr (if (= (getvar "LOCALE") "FR") 2 1)))
  )
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  ListBox  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ListBox []-----------------------[]                                          ;
;--- Date of creation       > 15/04/2017                                                                                                            ;
;--- Last modification date > 11/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.0.0                                                                                                                 ;
;--- Class                  > "DbLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Generates a temporary .dcl file for a single/multiple item selection dialog box in a list passed as an argument.                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ListBox) have 5 argument(s) :                                                                                                       ;
;   --•  title                  > corresponds to the title of the dialog box                                                                        ;
;     (type title) = 'STR                       | Ex. : "Layers selection", "VP-RADPURGE: Layouts selection", "", ...                               ;
;   --•  msg                    > corresponds to the message you wish to display above the popup list                                               ;
;     (type msg) = 'STR                         | Ex. : "Multiple choice :", "Please, select one or more layer in the list below...", "", ...       ;
;   --•  lst                    > corresponds to the list you wish to display in the popup-list. Each element of the list have to be a string       ;
;     (type lst) = 'LST                         | Ex. : '(label1 label2 label3 ... labeln), (layoutlist), ...                                       ;
;   --•  value                  > is the default value to be selected when the dialog box is opened or list of default values                       ;
;     (type value) = 'STR or 'LST               | Ex. : (getvar "CLAYER"), "1030-01", ...                                                           ;
;   --•  flag                   > corresponds to the type of popup-list between a drop-down list, a single or multiple choice list                  ;
;     (type flag) = 'INT                        | Ex. : 0 = drop-down list, 1 = single choice list or 2 = multiple choice list                      ;
;   --•  h                      > corresponds to the number of lines prompted in the list for list_box tiles                                        ;
;     (type h) = 'INT                           | Ex. : nil, 8, 15, 30, 23, ...                                                                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2007 ((gile))                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> vl-list-search                                | v1.0.0 - 13/12/2021 (Luna)                                                    ;
;   --•  "BaStr" ---> LB-select                                     | v1.0.0 - 11/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.3 - 08/07/2022 (Luna)                                                    ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsLaytb" ---> c:QuickLayoutSwitch                         | v3.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (ListBox) returns the value ('flag' = 0 or 1) or the list of values ('flag' = 2) chosen by the user.                               ;
;     Ex. : (ListBox "Layout Selection" "Select one layout :" (layoutlist) (getvar "CTAB") 1) returns the layout name selected or nil if canceled   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.0.0   |   Add the 'h' argument to adjust the height of the list (= flag equals 1 or 2) and add the number of selected line if flag = 2   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.1.0   |   Add the possibility to set several default values if 'flag' = 2 and 'value' is a list type                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Add the (vl-list-search) function as a local function, for (ListBox) use only                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Added an edit_box adding the possibility to filter the list from a search (cf. (wcmatch) wildcard characters). To display the  | ;
; |   v3.0.0   |   complete list, you must put a "*" in the search bar. The number of items displayed as well as the total number of items in     | ;
; |            |   the list are displayed below the list as information. Added a local variable 'tlst' to avoid losing the original list data     | ;
; |            |   while returning the correct results when a filter is applied. Added the (vl-list-search) function to the (ListBox) function.   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Removed the association list to work directly with a list of atoms and returns directly the value(s) selected by the user.     | ;
; |   v2.0.0   |   Also added a default value to initialize the dialog box's list (if specified). Adapted the global width of the dialog box      | ;
; |            |   according to the longest text in the list.                                                                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun ListBox (title msg lst value flag h / vl-list-search LB-select tmp file DCL_ID choice tlst)
  (defun vl-list-search (p l)
    (vl-remove-if-not '(lambda (x) (wcmatch x p)) l)
  )
  
  (defun LB-select (str)
    (if (= "" str)
      "0 selected"
      (strcat (itoa (length (str2lst str " "))) " selected")
    )
  )
  
  (setq
    tmp (vl-filename-mktemp "tmp.dcl")
    file (open tmp "w")
    tlst lst
  )
  (write-line
    (strcat "ListBox:dialog{width=" (itoa (+ (apply 'max (mapcar 'strlen (mapcar 'vl-princ-to-string lst))) 5)) ";label=\"" title "\";")
    file
  )
  (write-line
    ":edit_box{key=\"filter\";}"
    file
  )
  (if (and msg (/= msg ""))
    (write-line (strcat ":text{label=\"" msg "\";}") file)
  )
  (write-line
    (cond
      ( (= 0 flag) "spacer;:popup_list{key=\"lst\";}")
      ( (= 1 flag) (strcat "spacer;:list_box{height=" (itoa (1+ (cond (h) (15)))) ";key=\"lst\";}"))
      ( T (strcat "spacer;:list_box{height=" (itoa (1+ (cond (h) (15)))) ";key=\"lst\";multiple_select=true;}:text{key=\"select\";}"))
    )
    file
  )
  (write-line ":text{key=\"count\";}" file)
  (write-line "spacer;ok_cancel;}" file)
  (close file)
  (setq DCL_ID (load_dialog tmp))
  (if (not (new_dialog "ListBox" DCL_ID))
    (exit)
  )
  (set_tile "filter" "*")
  (set_tile "count" (strcat (itoa (length lst)) " / " (itoa (length lst))))
  (start_list "lst")
  (mapcar 'add_list lst)
  (end_list)
  (set_tile
    "lst"
    (cond
      ( (and
          (= flag 2)
          (listp value)
        )
        (apply 'strcat (vl-remove nil (mapcar '(lambda (x) (if (member x lst) (strcat (itoa (vl-position x lst)) " "))) value)))
      )
      ( (member value lst) (itoa (vl-position value lst)))
      ( (itoa 0))
    )
  )
  (if (= flag 2)
    (progn
      (set_tile "select" (LB-select (get_tile "lst")))
      (action_tile "lst" "(set_tile \"select\" (LB-select $value))")
    )
  )
  (action_tile
    "filter"
    "(start_list \"lst\")
     (mapcar 'add_list (setq tlst (vl-list-search $value lst)))
     (end_list)
     (set_tile \"count\" (strcat (itoa (length tlst)) \" / \" (itoa (length lst))))"
  )
  (action_tile
    "accept"
    "(or 
      (= (get_tile \"lst\") \"\")
      (if (= 2 flag)
        (progn
          (foreach n (str2lst (get_tile \"lst\") \" \")
            (setq choice (cons (nth (atoi n) tlst) choice))
          )
          (setq choice (reverse choice))
        )
        (setq choice (nth (atoi (get_tile \"lst\")) tlst))
      )
    )
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete tmp)
  choice
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                         --{  vla-collection->list  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] vla-collection->list []-----------------------[]                                   ;
;--- Date of creation       > 05/03/2021                                                                                                            ;
;--- Last modification date > 19/08/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlCol"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Transforms a VLA collection into a list of pointed pairs with the name and ename of each object in the collection. It is an improved version of ;
;   the (flt_tbl) function because it can access more objects unlike the Symbol Tables (more limited) but without the search tool to filter the     ;
;   search on a pattern.                                                                                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (vla-collection->list) have 3 argument(s) :                                                                                          ;
;   --•  doc                    > corresponds to the VLA-Object of the search document. If nil, matches the ActiveDocument                          ;
;     (type doc) = 'VLA-OBJECT                  | Ex. : nil, (vlax-get-acad-object), (vla-get-ActiveDocument (vlax-get-acad-object)), ...           ;
;   --•  col                    > corresponds to the name of the collection with a quote (the name of a collection is always in the plural)         ;
;     (type col) = 'SUBR                        | Ex. : 'materials, 'linetypes, 'blocks, 'layers, ...                                               ;
;   --•  flag                   > allows to define if the name of the retrieved object corresponds to the ename or the VLA-object                   ;
;     (type flag) = 'BOOLEAN                    | Ex. : 0 for entity name and 1 for VLA-Object                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "VlPrp" ---> set-layout-name                               | v1.1.0 - 24/01/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (vla-collection->list) returns the list of pointed pairs each composed of the name of the object in string format and the ename or ;
;   VLA-Object of the object to access its DXF list or its properties.                                                                              ;
;     Ex. : (vla-collection->list nil 'linetypes 0) returns                                                                                         ;
;              ( ("ByBlock" . <Entity name: 3b058940>)                                                                                              ;
;                ("ByLayer" . <Entity name: 3b058950>)                                                                                              ;
;                ("Continuous" . <Entity name: 3b058960>)                                                                                           ;
;              )                                                                                                                                    ;
;           (vla-collection->list nil 'linetypes 1) returns                                                                                         ;
;              ( ("ByBlock" . #<VLA-OBJECT IAcadLineType 0000018b15e7ada8>)                                                                         ;
;                ("ByLayer" . #<VLA-OBJECT IAcadLineType 0000018b15e79f08>)                                                                         ;
;                ("Continuous" . #<VLA-OBJECT IAcadLineType 0000018b15c51728>)                                                                      ;
;              )                                                                                                                                    ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Add an argument to specify the VLA-Object corresponding to the search document to extend the functionnalities                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Simplification of writing the collection (ActiveDocument only) and addition of the flag variable to modify the return          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun vla-collection->list (doc col flag / lst item i)
  (if
    (null
      (vl-catch-all-error-p
        (setq
          i 0
          col (vl-catch-all-apply 'vlax-get (list (cond (doc) ((vla-get-activedocument (vlax-get-acad-object)))) col))
        )
      )
    )
    (vlax-for item col
      (setq lst
        (cons
          (cons
            (if (vlax-property-available-p item 'Name)
              (vla-get-name item)
              (strcat "Unnamed_" (itoa (setq i (1+ i))))
            )
            (cond
              ( (= flag 0) (vlax-vla-object->ename item))
              (item)
            )
          )
          lst
        )
      )
    )
  )
  (reverse lst)
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  get-att-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] get-att-list []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the complete list of attribute names and their values for a given block reference. The list is composed of a set of pointed pairs in  ;
;   the form (TagName . TagValue).                                                                                                                  ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-att-list) have 1 argument(s) :                                                                                                  ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have attributes                                       ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtSel" ---> Select-Filter                                 | v3.2.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.3 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-att-list) returns the association list of ((<tag> . <value>) ...) of the block reference, nil otherwise.                      ;
;     Ex. : (get-att-list (vlax-ename->vla-object (car (entsel)))) returns                                                                          ;
;             ( ("C:COLOR" . "144")                                                                                                                 ;
;               ("C:LAYER" . "Test2")                                                                                                               ;
;               ("C:LINE_TYPE" . "test")                                                                                                            ;
;               ("C:LINE_WEIGHT" . "211")                                                                                                           ;
;               ("C:TRANSPARENCY" . "14")                                                                                                           ;
;               ("C:MATERIAL" . "DuCalque")                                                                                                         ;
;               ("C:POSITION_X" . "-23")                                                                                                            ;
;               ("C:POSITION_Y" . "-59.154")                                                                                                        ;
;               ("C:POSITION_Z" . "2.34")                                                                                                           ;
;               ("C:SCALE_X" . "-1")                                                                                                                ;
;               ("C:SCALE_Y" . "0.43")                                                                                                              ;
;               ("C:SCALE_Z" . "0.8")                                                                                                               ;
;               ("C:ROTATION" . "-100")                                                                                                             ;
;             )                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-att-list (blk)
  (mapcar
    '(lambda (att)
      (cons
        (vla-get-tagstring att)
        (vla-get-textstring att)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
  )
)


; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  ConvName  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ConvName []-----------------------[]                                         ;
;--- Date of creation       > 04/01/2022                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlDtc"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to convert an entity name to VLA-Object and vice versa by choosing the output format. It helps when you don't know the type of output    ;
;   you have, but you know what type of input a function needs.                                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ConvName) have 2 argument(s) :                                                                                                      ;
;   --•  name                   > is the entity name or the VLA-Object you want to convert if needed                                                ;
;     (type name) = 'ENAME or VLA-OBJECT        | Ex. : <Entity name: 273a55ebd40>, #<VLA-OBJECT IAcadLWPolyline 0000027c1aab5868>, ...             ;
;   --•  flag                   > is the type of output you want between 'ENAME or 'VLA-OBJECT                                                      ;
;     (type flag) = 'SYM                        | Ex. : 'ENAME or 'VLA-OBJECT                                                                       ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtObj" ---> GetAnyProperty                                | v2.0.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "DtObj" ---> SetAnyProperty                                | v2.0.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-att-value                                 | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-AllowedValues                         | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-list                                  | v3.0.1 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-value                                 | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-VisibilityName                        | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-att-list                                  | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-att-value                                 | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-FlipState                             | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-list                                  | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-value                                 | v2.1.0 - 30/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (ConvName) returns the entity name or VLA-Object, depending of the 'flag' value.                                                   ;
;     Ex. : (ConvName (car (entsel)) 'ENAME) returns <Entity name: 273a55ebd40>                                                                     ;
;           (ConvName (car (entsel)) 'VLA-OBJECT) returns #<VLA-OBJECT IAcadLWPolyline 0000027c1aab5868>                                            ;
;           (ConvName (vlax-ename->vla-object (car (entsel))) 'ENAME) returns <Entity name: 273a55ebd40>                                            ;
;           (ConvName (vlax-ename->vla-object (car (entsel))) 'VLA-OBJECT) returns #<VLA-OBJECT IAcadLWPolyline 0000027c1aab5868>                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun ConvName (name flag / tp)
  (if (= (setq tp (type name)) flag)
    name
    ((eval (read (strcat "vlax-" (vl-symbol-name tp) "->" (vl-symbol-name flag)))) name)
  )
)