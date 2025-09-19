
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  NAMECART  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] NAMECART []-----------------------[]                                          ;
;--- Date of creation       > 25/05/2019                                                                                                            ;
;--- Last modification date > 21/06/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 6.1.7                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Renames one, several or all layout tabs according to 2 predefined formats.                                                                      ;
;   The first format corresponds to the desired name of the PDF files, thus simplifying the printing of plans in PDF format and is of the form      ;
;   "C####_[SAECXD]_####-##_LAYOUTNAME", with the project code (= "C####"), the design phase for the drawing ("S" = "ESQ", "A" = "APS", "E" = "APD" ;
;   ,"C" = "DCE", "X" = "EXE" or "D" = "DOE"), the index of the plan (= "####-"), the sheet number (= "-##") and then the name of the layout .      ;
;   The second format corresponds to a shorter name in order to show more layout tabs in AutoCAD. The format is "####-##", with the index of the    ;
;   plan (= "####-") and the sheet number (= "-##") only.                                                                                           ;
;   Moreover, this command will set the visibility relative to the design phase of the plan cartridge according to the seventh character in the     ;
;   drawing name. It can be useful when you're changing the design phase of a drawing.                                                              ;
;   In the end, this command will sort all the layout tabs based on their name to have more visibility and organization logic.                      ;
;                                                                                                                                                   ;
;   Here's some information about the format structure :                                                                                            ;
;     The format is used for 's-lst' (= "Simplified" format) and 'd-lst' (= "Detailled" format). It's based on a list structure where each element  ;
;     of the list corresponds to a string. At the end, each string will be concatenates together to create the final name. The type of each element ;
;     define how it works.                                                                                                                          ;
;     If the element is a string, it's just a text string so it's a common string for every name                                                    ;
;     If the element is a list, it means that you want to get the value of an attribute from the title-block. If you have several attribute's name  ;
;     for a single value (different version of title-block, for example), you can specify more than one attribute per list (the order of appearing  ;
;     defines the priority between them, from left to right).                                                                                       ;
;     If you need to modify the value of an attribute before using it in the name, you can use a quoted '(lambda (s) ...) function at the end of    ;
;     the element (if the element is a list)                                                                                                        ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves the DWG name and extract the project code (first to fifth character) and the design phase (next character after the   ;
;                   first occurrence of "_") from the DWG name                                                                                      ;
; Step n°2        : Sets the right string (= visibility name for design phase) for each possible value of the visibility                            ;
; Step n°3        : Ask the user if he want to work on the "Active", a "Selection" or "All" layout tabs, with "Active" as default value             ;
; Step n°4        : For each layout selected by the previous method, do                                                                             ;
;   Step n°4.a    :   Selects any block with an EffectiveName starting with "*Cartouche" on the layout and check if there's only 1 plan cartridge   ;
;   Step n°4.b.1  :     If :True, retieves the list of attributes of the block                                                                      ;
;   Step n°4.b.2  :     Sets the visibility of the block with the design phase previously extracted from the DWG name                               ;
;   Step n°4.b.3  :     Checks if there's any problem with the value of attributes                                                                  ;
;   Step n°4.b.3.a:       If :True, prompt an alert message box with some explanation of why NAMECART will not working on this particular layout    ;
; Step n°5        : Asks the user with wich format he want to rename the layout tabs between "Simplified" or "Detailed"                             ;
;   Step n°5.a    :   Checks if the actual layout name is already fine or if the program have to rename it                                          ;
;   Step n°5.b    :   If "Simplified" (equals default value), use the format "####-##" to rename the layouts                                        ;
;   Step n°5.c    :   If "Detailed", use the format "C####_[SAECXD]_####-##_LAYOUTNAME" to rename the layouts                                       ;
; Step n°6        : Sorts the layout tabs in ascending order according to the index of the plan and sheet number                                    ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.0.0 - 05/03/2021 (Luna)                                                    ;
;   --•  "UtDis" ---> alert-princ                                   | v1.0.0 - 26/01/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.0.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> Select-filter                                 | v3.1.1 - 01/01/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> set-dyn-VisibilityValue                       | v2.0.0 - 04/01/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> get-layouts-pos                               | v2.0.0 - 24/01/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> set-layouts-pos                               | v2.0.0 - 24/01/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> set-layout-name                               | v1.1.1 - 21/06/2024 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> get-cart-name                                 | v1.1.2 - 23/12/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> set-cart-name                                 | v1.0.0 - 30/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (NAMECART) returns for each layout tab(s) selected by the user (depending on the selection mode) if the layout could be renamed or  ;
;   not and if not, explained, as detailed as the program can, why it didn't work.                                                                  ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.7   |   Update (set-layout-name) function to v1.1.1                                                                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.6   |   Fix the visibility issue with new cartridges and before                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.5   |   Correct the filter BlockName of (select-filter) to allow the selection of new cartridges (2023)                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.4   |   Add an error message if the visibility value is not found                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.3   |   Fix an issue for empty attributes                                                                                              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.2   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.1   |   Add (get-layouts-pos) for the (ListBox) function to order the layout tabs as they are in the drawing                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.1.0   |   Add the value "P" = "PRO" for visibility state of blocks                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.0.4   |   Update (LgT) function to v1.1.0                                                                                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.0.3   |   Update (ListBox) function to v4.0.0 and adding the possibility to set several attributes name for a single part of format      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.0.2   |   Adding the $lf$ global variable for "FORCEDLANGUAGE" system variable                                                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.0.1   |   Replacing the name of (getkword-default) by (getkdh) and functionnality of it                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v6.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v5.0.0   |   Adding the "Selection" method by using the (ListBox) function to be able to rename several layouts instead of just "Active"    | ;
; |            |   or "All" and definitively delete the usability of 'SummaryInfo to let M-Files do his work                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.2.0   |   Suppression of the modification of the project code retrieved in the 'SummaryInfo for more usability with M-Files and changing | ;
; |            |   the "Simplified"  format to be "####-##" instead of "####.##"                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.1.0   |   Suppression of the use of (command) for renaming the layout tabs, modify the functions used to retrieves some information,     | ;
; |            |   adding the sort of the layout tabs based on their name in the alphanumerical order and using the (select-filter) function      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Retrieves the project code and the design phase in drawing name, retrieves the 'SummaryInfo of the ActiveDocument, adding the  | ;
; |   v4.0.0   |   modification of the visibility of plan cartridge based on the letter representing the design phase in the drawing name and     | ;
; |            |   modify the attribute relative to the project code of plan cartridge if the value is different of the value in 'SummaryInfo     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Adding the "Simplified" (= "####.##") and "Detailed" (= "C####_[SAECXD]_####-##_LAYOUTNAME") format for the layout's name      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Changing the name of the command, adding local variables, adding the choice "Active" or "All" for layouts selected, adding the | ;
; |            |   auto-selection of the plan cartridge for each layout                                                                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:NAMECART ( / *error* get-cart-name set-cart-name i Phase laylist s-lst d-lst mode-s mode-n layout ll lst)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun get-cart-name (layout format / jsel name att-list lst vis)
    (sssetfirst nil nil)
    (cond
      ( (null (setq jsel (select-filter "BLC" '("*Cartouche*" "`*U*") (list "_X" (list (cons 410 layout) (cons 66 1))) nil nil)))
        (alert-princ
          (LgT
            (strcat
              "An error occurred during the cartridge selection..."
              "\nNo blocks were found on the layout tab \""
              layout
              "\" whose name begins with \"Cartouche\"..."
            )
            (strcat
              "Une erreur est survenue lors de la sélection du cartouche..."
              "\nAucun bloc n'a été trouvé sur la présentation \""
              layout
              "\" dont le nom commence par \"Cartouche\"..."
            )
            nil
          )
        )
      )
      ( (not (equal 1 (sslength jsel)))
        (alert-princ
          (LgT
            (strcat
              "An error occurred during the cartridge selection..."
              "\nThe layout tab \""
              layout
              "\" currently has more than one block whose name begins with \"Cartouche\"..."
            )
            (strcat
              "Une erreur est survenue lors de la sélection du cartouche..."
              "\nLa présentation \""
              layout
              "\" possède actuellement plus d'un bloc dont le nom commence par \"Cartouche\"..."
            )
            nil
          )
        )
      )
      ( (null (setq name (ssname jsel 0))))
      ( (null
          (and
            (setq vis (car (vl-member-if '(lambda (x) (wcmatch x (strcat "*" Phase "*"))) (get-dyn-AllowedValues name (get-dyn-VisibilityName name)))))
            (set-dyn-VisibilityValue name vis)
          )
        )
        (alert-princ
          (LgT
            (strcat
              "\nAn error occurred during the cartridge selection..."
              "\nThe cartridge block reference doesn't have the visibility value \""
              (cond (vis) (Phase))
              "\" on the layout tab \""
              layout
              "\"..."
            )
            (strcat
              "\nUne erreur est survenue lors de la sélection du cartouche..."
              "\nLa référence de bloc cartouche ne possède pas de visbilité \""
              (cond (vis) (Phase))
              "\" sur la présentation \""
              layout
              "\"..."
            )
            nil
          )
        )
      )
      ( (null (setq att-list (get-att-list name)))
        (alert-princ
          (LgT
            (strcat
              "\nAn error occurred during the cartridge selection..."
              "\nThe cartridge block reference has no attributes on the layout tab \""
              layout
              "\"..."
            )
            (strcat
              "\nUne erreur est survenue lors de la sélection du cartouche..."
              "\nLa référence de bloc cartouche n'a pas d'attributs sur la présentation \""
              layout
              "\"..."
            )
            nil
          )
        )
      )
      ( (member
          nil
          (setq lst
            (mapcar
              '(lambda (x / a v)
                (setq a x)
                (cond 
                  ( (not (listp x)) x)
                  ( (while
                      (and
                        (car a)
                        (= 'STR (type (car a)))
                        (or
                          (null (setq v (cdr (assoc (car a) att-list))))
                          (= "" v)
                        )
                        (setq a (cdr a))
                      )
                      (setq v nil)
                    )
                  )
                  ( (and v (listp (last x))) (apply (last x) (list v)))
                  (v)
                )
               )
              format
            )
          )
        )
        (alert-princ
          (LgT
            (strcat
              "\nAn error occurred during the cartridge selection..."
              "\nThe following attributes were not found on the cartridge of the layout tab \""
              layout
              "\"...\n  - "
              (lst2str
                (mapcar
                  'caar
                  (vl-remove-if
                    '(lambda (x) (cdr x))
                    (mapcar 'cons format lst)
                  )
                )
                "\n  - "
              )
            )
            (strcat
              "\nUne erreur est survenue lors de la sélection du cartouche..."
              "\nLes attributs suivant n'ont pas été trouvés dans le cartouche sur la présentation \""
              layout
              "\"...\n  - "
              (lst2str
                (mapcar
                  'caar
                  (vl-remove-if
                    '(lambda (x) (cdr x))
                    (mapcar 'cons format lst)
                  )
                )
                "\n  - "
              )
            )
            nil
          )
        )
        (setq lst nil)
      )
      ( (setq lst (apply 'strcat lst)))
    )
    lst
  )
  
  (defun set-cart-name (layout / str)
    (setq str
      (get-cart-name
        layout
        (cond
          ((= mode-n "Simplified") s-lst)
          ((= mode-n "Detailed") d-lst)
        )
      )
    )
    (if (and str (setq str (set-layout-name (set-layout-name layout (strcat "$temp" (itoa (setq i (1+ i))) "$")) (strcase str))))
      (princ
        (LgT
          (strcat
            "\nThe layout tab \""
            layout
            "\" has been successfully renamed and now has the name \""
            str
            "\"..."
          )
          (strcat
            "\nLa présentation \""
            layout
            "\" a été renommée avec succès et possède désormais le nom \""
            str
            "\"..."
          )
          nil
        )
      )
      (princ
        (LgT
          (strcat
            "\nAn error occurred on the layout tab \""
            layout
            "\" (see above for more details) and couldn't be renamed..."
          )
          (strcat
            "\nUne erreur est survenue sur la présentation \""
            layout
            "\" (voir ci-dessus pour plus de détails) et n'a pas pu être renommée..."
          )
          nil
        )
      )
    )
  )

  (sssetfirst nil nil)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq
    i 100
    Phase (substr (getvar "DWGNAME") (+ 2 (vl-string-position (ascii "_") (getvar "DWGNAME"))) 1)
    laylist (layoutlist)
    s-lst
      (list
        (list "N°DESSIN" "N°_DESSIN" '(lambda (s) (substr s 1 4)))
        "-"
        '("PLANCHE")
      )
    d-lst
      (list
        '("CODE" "CODEPROJET")
        "_"
        Phase
        "_"
        (list "N°DESSIN" "N°_DESSIN" '(lambda (s) (substr s 1 4)))
        "-"
        '("PLANCHE")
        "_"
        (list "TITRE_2" '(lambda (s) (vl-string-translate "/,;\\:?*" ".....  " s)))
      )
  )
  (cond
    ((= Phase "S") (setq Phase "ESQ"))
    ((= Phase "A") (setq Phase "APS"))
    ((= Phase "E") (setq Phase "APD"))
    ((= Phase "P") (setq Phase "PRO"))
    ((= Phase "C") (setq Phase "DCE"))
    ((= Phase "X") (setq Phase "EXE"))
    ((= Phase "D") (setq Phase "DOE"))
    ((setq Phase "Masquer"))
  )
  (setq mode-s
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nWhich layout tab(s) would you like to rename"
        "\nQuelle(s) présentation(s) souhaitez-vous renommer"
        nil
      )
      (list
        (LgT
          "Current Selection All _Current Selection All"
          "Active Sélection Toutes _Current Selection All"
          nil
        )
      )
      " ? "
      "Current"
      (LgT
        (strcat
          "\nNAMECART : Selection mode for layout tabs"
          "\nDefault value:     Current"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Selects only the current presentation, i.e. the layout tab you are  |"
          "\n  |   Current   | currently working on. If you are currently on the \"OBJECT\" tab, the |"
          "\n  |             | last active layout will be considered for the rest of the program   |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Opens a multiple choice selection dialog box in order to specify    |"
          "\n  |  Selection  | the list of layout tabs you want to rename among all layout tabs of |"
          "\n  |             | the drawing. The pre-selected value at the opening of the dialog    |"
          "\n  |             | box corresponds to the current layout tab (except for MODEL space)  |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |     All     | All the layout tabs of the current drawing are selected to be       |"
          "\n  |             | renamed (except the layouts named \"TOOLKIT\")                        |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nNAMECART : Mode de sélection des présentations"
          "\nValeur par défaut: Active"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Sélectionne uniquement la présentation courante, c'est-à-dire la    |"
          "\n  |    Active   | présentation sur laquelle vous êtes actuellement. Si vous êtes      |"
          "\n  |             | actuellement sur l'onglet \"OBJET\", la dernière présentation active  |"
          "\n  |             | sera considérée pour la suite du programme                          |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Ouvre une boîte de dialogue de sélection à choix multiple dans le   |"
          "\n  |             | but de spécifier la liste des présentations que vous souhaitez      |"
          "\n  |  Sélection  | renommer parmi l'ensemble des présentations du dessin. La valeur    |"
          "\n  |             | pré-sélectionnée à l'ouverture correspond à la présentation         |"
          "\n  |             | courante (à l'exception de l'espace OBJET)                          |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Toutes les présentations du dessin en cours sont sélectionnées pour |"
          "\n  |    Toutes   | être renommées (à l'exception des présentations nommées \"TOOLKIT\"   |"
          "\n  |             | et/ou \"TRAVAIL\")                                                    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (setq mode-n
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nSpecify the writing format that will be used to rename the layout tab(s)"
        "\nSpécifiez le format d'écriture qui sera utilisé pour renommer la/les présentation(s)"
        nil
      )
      (list
        (LgT
          "Simplified Detailed _Simplified Detailed"
          "Simplifiée Détaillée _Simplified Detailed"
          nil
        )
      )
      " : "
      "Simplified"
      (LgT
        (strcat
          "\nNAMECART : Writing format"
          "\nDefault value:     Simplified"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Uses the format ####-## with :                                      |"
          "\n  |             | ####     -> corresponds to the index of the plan (attribute of the  |"
          "\n  |             |             cartridge named \"N°_DESSIN\")                            |"
          "\n  |  Simplified | ##       -> corresponds to the sheet number (attribute of the       |"
          "\n  |             |             cartridge named \"PLANCHE\")                              |"
          "\n  |             | The main interest of this format is the short length of the layout  |"
          "\n  |             | name, allowing to display more layout tabs at the bottom            |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Uses the format Cxxxx_[SAECXD]_####-##_... with :                   |"
          "\n  |             | Cxxxx    -> corresponds to the project code (attribute of the       |"
          "\n  |             |             cartridge named \"CODE\")                                 |"
          "\n  |             | [SAECXD] -> corresponds to the letter representing the phase design |"
          "\n  |             |             (retrieved from the .dwg name after the first \"_\")      |"
          "\n  |             | ####     -> corresponds to the index of the plan (attribute of the  |"
          "\n  |   Detailed  |             cartridge named \"N°_DESSIN\")                            |"
          "\n  |             | ##       -> corresponds to the sheet number (attribute of the       |"
          "\n  |             |             cartridge named \"PLANCHE\")                              |"
          "\n  |             | ...      -> corresponds to the title of the plan (attribute of the  |"
          "\n  |             |             cartridge named \"TITRE_2\")                              |"
          "\n  |             | The main interest of this format is that it corresponds to the      |"
          "\n  |             | format used by PDF files, thus simplifying PDF printing             |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nNAMECART : Format d'écriture"
          "\nValeur par défaut: Simplifiée"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Utilise le format ####-## avec :                                    |"
          "\n  |             | ####     -> correspond à l'indice du plan (attribut du cartouche    |"
          "\n  |             |             nommé \"N°_DESSIN\")                                      |"
          "\n  |  Simplifiée | ##       -> correspond au numéro de la planche (attribut du         |"
          "\n  |             |             cartouche nommé \"PLANCHE\")                              |"
          "\n  |             | Le principal intérêt de ce format est la faible longueur du nom,    |"
          "\n  |             | permettant ainsi d'afficher davantage de présentation en bas        |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Utilise le format Cxxxx_[SAECXD]_####-##_... avec :                 |"
          "\n  |             | Cxxxx    -> correspond au code projet (attribut du cartouche nommé  |"
          "\n  |             |             \"CODE\")                                                 |"
          "\n  |             | [SAECXD] -> correspond à la lettre représentant la phase de design  |"
          "\n  |             |             (récupérée dans le nom du .dwg après le premier \"_\")    |"
          "\n  |             | ####     -> correspond à l'indice du plan (attribut du cartouche    |"
          "\n  |  Détaillée  |             nommé \"N°_DESSIN\")                                      |"
          "\n  |             | ##       -> correspond au numéro de la planche (attribut du         |"
          "\n  |             |             cartouche nommé \"PLANCHE\")                              |"
          "\n  |             | ...      -> correspond au titre du plan (attribut du cartouche      |"
          "\n  |             |             nommé \"TITRE_2\")                                        |"
          "\n  |             | Le principal intérêt de ce format est qu'il correspond au format    |"
          "\n  |             | utilisé par les fichiers PDF, simplifiant ainsi l'impression PDF    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (cond
    ( (= mode-s "Current")
      (setvar "TILEMODE" 0)
      (setq layout (getvar "CTAB"))
      (set-cart-name layout)
    )
    ( (member mode-s '("Selection" "All"))
      (setq layout (getvar "CTAB"))
      (cond
        ((= mode-s "Selection")
          (setq ll
            (ListBox
              (LgT
                "NAMECART: Layout tab(s) selection"
                "NAMECART: Sélection des présentation(s)"
                nil
              )
              (LgT
                "Please, select one or more layout tab(s) to be renamed :"
                "Veuillez sélectionner la ou les présentation(s) à renommer :"
                nil
              )
              (vl-remove-if
                '(lambda (x) (member (strcase x) '("MODEL" "TOOLKIT" "TRAVAIL")))
                (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
              )
              layout
              2
              nil
            )
          )
        )
        ((= mode-s "All")
          (setq ll
            (vl-remove-if
              '(lambda (x) (member (strcase x) '("MODEL" "TOOLKIT" "TRAVAIL")))
              (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
            )
          )
        )
      )
      (foreach layout ll
        (set-cart-name layout)
      )
    )
  )
  (sssetfirst nil nil)
  (if (setq lst (vl-remove-if '(lambda (x) (= (car x) (cdr x))) (mapcar 'cons laylist (set-layouts-pos '<))))
    (princ
      (LgT
        (strcat
          "\nThe following layout tab(s) have been moved after the sorting functionality:"
          (apply
            'strcat
            (mapcar
              '(lambda (x)
                (strcat
                  "\n   The layout tab \""
                  (car x)
                  "\" has been replaced by \""
                  (cdr x)
                  "\" at the position "
                  (itoa (vla-get-taborder (vla-item (vla-get-layouts (vla-get-ActiveDocument (vlax-get-acad-object))) (cdr x))))
                  "..."
                )
               )
              lst
            )
          )
        )
        (strcat
          "\nLa ou les présentation(s) suivantes ont été déplacées après la fonctionnalité de tri :"
          (apply
            'strcat
            (mapcar
              '(lambda (x)
                (strcat
                  "\n   La présentation \""
                  (car x)
                  "\" a été remplacée par \""
                  (cdr x)
                  "\" à la position "
                  (itoa (vla-get-taborder (vla-item (vla-get-layouts (vla-get-ActiveDocument (vlax-get-acad-object))) (cdr x))))
                  "..."
                )
               )
              lst
            )
          )
        )
        nil
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)