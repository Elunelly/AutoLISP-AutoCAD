
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  TOTLENGTH  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] TOTLENGTH []-----------------------[]                                         ;
;--- Date of creation       > 02/02/2022                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (TOTLENGTH) returns [...]                                                                                                                 ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:TOTLENGTH (/ TtLg_ResultBox TtLg_PropertyLevels obj-lst ppt-lst jsel i name mode )
  (defun TtLg_ResultBox (lst lvl / *error* checkbox filename file tmp-lst w h DCL_ID str l)
    (defun *error* (msg)
      (if file (close file))
      (if filename (vl-file-delete filename))
      (princ msg)
    )
    (defun checkbox (value tile)
      (mode_tile tile (- 1 (atoi value)))
    )
    (setq
      filename (vl-filename-mktemp "TOTLENGTH-ResultBox.dcl")
      file (open filename "W")
      tmp-lst lst
    )
    (write-line
      (strcat
        " TOTLENGTH_ResultBox:dialog {"
        "   width = " (itoa (setq w 142)) " ;"
        "   height = " (itoa (setq h 61)) " ;"
        "   label = \"" (LgT "TOTLENGTH : Displaying the results" "TOTLENGTH : Affichage des résultats" nil) "\" ;"
        "   :row {"
        "     :column {"
        "       children_fixed_width = true ;"
      )
      file
    )
    (mapcar
      '(lambda (x)
        (write-line
          (strcat
            "       :boxed_column {"
            "         label = \"" x "\" ;"
            "         width = " (itoa (/ w 3)) " ;"
            "         :toggle {"
            "           label = \"" (LgT "Consideration of the property" "Prise en compte de la propriété" nil) "\" ;"
            "           key = \"" x "_chk\" ;"
            "           value = \"1\" ;"
            "         }"
            "         :list_box {"
            "           key = \"" x "_lst\" ;"
            "           height = " (itoa (- (/ (- h 10) (length lvl)) 4)) " ;"
            "           multiple_select = true ;"
            "         }"
            "         :toggle {"
            "           label = \"" (LgT "Select all" "Sélectionner tout" nil) "\" ;"
            "           key = \"" x "_sel\" ;"
            "           value = \"1\" ;"
            "         }"
            "       }"
          )
          file
        )
       )
      lvl
    )
    (write-line
      (strcat
        "     }"
        "     :boxed_column {"
        "       label = \"" (LgT "Informations" "Informations" nil) "\" ;"
        "       width = " (itoa (/ (* 2 w) 3)) " ;"
        "       :radio_row {"
        "         height = 3 ;"
        "         :radio_button {"
        "           label = \"" (LgT "Detailled results" "Résultats détaillés" nil) "\" ;"
        "           key = \"Result_Det\" ;"
        "         }"
        "         :radio_button {"
        "           label = \"" (LgT "Simplified results" "Résultats simplifiés" nil) "\" ;"
        "           key = \"Result_Sim\" ;"
        "           value = \"1\" ;"
        "         }"
        "       }"
        "       :list_box {"
        "         key = \"ResultBox\" ;"
        "         height = " (itoa (- h 20)) " ;"
        "       }"
        "       :row {"
        "         :column {"
        "           :toggle {"
        "             label = \"" (LgT "Select all" "Sélectionner tout" nil) "\" ;"
        "             key = \"Select_All\" ;"
        "           }"
        "           :toggle {"
        "             label = \"" (LgT "Select Informations only" "Sélectionner uniquement Informations" nil) "\" ;"
        "             key = \"Select_Res\" ;"
        "           }"
        "         }"
        "       }"
        "       :row {"
        "         :column {"
        "           :toggle {"
        "             label = \"" (LgT "Select top lower 5%" "Sélectionner 5% des valeurs minimales" nil) "\" ;"
        "             key = \"Select_Inf\" ;"
        "           }"
        "           :toggle {"
        "             label = \"" (LgT "Select top higher 5%" "Sélectionner 5% des valeurs maximales" nil) "\" ;"
        "             key = \"Select_Sup\" ;"
        "           }"
        "         }"
        "       }"
        "     }"
        "   }"
        "   spacer ;"
        "   :row {"
        "     children_alignment = centered ;"
        "     :button {"
        "       label = \"" (LgT "Refresh Informations..." "Rafraîchir Informations..." nil) "\" ;"
        "       key = \"Refresh\" ;"
        "       width = 26 ;"
        "     }"
        "     :button {"
        "       label = \"" (LgT "Re-select objects >" "Re-sélectionner objets >" nil) "\" ;"
        "       key = \"Select\" ;"
        "       width = 26 ;"
        "     }"
        "     :button {"
        "       label = \"" (LgT "Extract to Excel..." "Extraire vers Excel..." nil) "\" ;"
        "       key = \"Extract\" ;"
        "       width = 26 ;"
        "     }"
        "   }"
        "   ok_cancel_help ;"
        " }"
      )
      file
    )
    (close file)
    (setq DCL_ID (load_dialog filename))
    (if (not (new_dialog "TOTLENGTH_ResultBox" DCL_ID))
      (exit)
    )
    (foreach property lvl
      (start_list (strcat property "_lst"))
      (mapcar 'add_list (vl-sort (setq l (Remove-Double (mapcar 'car tmp-lst))) '<))
      (end_list)
      (set_tile (strcat property "_lst") (repeat (setq str "" i (length l)) (setq str (strcat str (itoa (setq i (1- i))) " "))))
      (setq tmp-lst (apply 'append (mapcar 'cdr tmp-lst)))
      (action_tile (strcat property "_chk") "(PropertyToggle_ResultBox property)")
      (action_tile (strcat property "_sel") "(PropertySelect_ResultBox property)")
    )
    (setq tmp-lst lst)
;;    (Refresh_ResultBox (vl-remove-if '(lambda (x) (= "0" (get_tile (strcat x "_chk")))) lvl))
    (action_tile "Refresh" "(Refresh_ResultBox (vl-remove-if '(lambda (x) (= \"0\" (get_tile (strcat x \"_chk\")))) lvl))")
    (action_tile "Select" "(done_dialog 1) (Select_ResultBox)")
    (action_tile "Extract" "(done_dialog 1) (Extract_ResultBox tmp-lst)")
    (action_tile "Accept" "(done_dialog 1)")
    (action_tile "Cancel" "(done_dialog 0)")
    (action_tile "Help" "(Help_ResultBox)")
    (start_dialog)
    (unload_dialog DCL_ID)
    (vl-file-delete filename)
    (princ)
  )
  
  (defun TtLg_PropertyLevels (lst / *error* RadioButton PopupList filename file tmp-lst DCL_ID)
    (defun *error* (msg)
      (if file (close file))
      (if filename (vl-file-delete filename))
      (princ msg)
    )
    (defun RadioButton (value i / n)
      (setq n 0)
      (if (= "1" value)
        (repeat (length lst)
          (setq n (1+ n))
          (mode_tile (strcat "Value_" n) (if (< i n) 1 0))
        )
      )
      i
    )
    (defun PopupList (value i / n l)
      (setq n 0)
      (if (/= "" value)
        (repeat (length lst)
          (setq n (1+ n))
          (start_list (strcat "Value_" n))
          (mapcar 'add_list (vl-remove value lst))
        )
      )
    )
    (setq
      filename (vl-filename-mktemp "TOTLENGTH-PropertyLevels.dcl")
      file (open filename "W")
      tmp-lst lst
    )
    (write-line
      (strcat
        " TOTLENGTH_PropertyLevels:dialog {"
        "   width = 42 ;"
        "   :boxed_row {"
        "     label = \"" (LgT "Selection and order of properties" "Sélection et ordre des propriétés" nil) "\" ;"
        "     :radio_column {"
      )
      file
    )
    (setq i 0)
    (repeat (length lst)
      (setq i (1+ i))
      (write-line
        (strcat
          "       :row {"
          "         :radio_button {"
          "           label = \"" (LgT "Level n°" "Niveau n°" nil) (itoa i) " :\" ;"
          "           key = \"Level_" (itoa i) "\" ;"
          "           width = 13 ;"
          "         }"
          "         :popup_list {"
          "           key = \"Value_" (itoa i) "\" ;"
          "           width = 23 ;"
          "         }"
          "         spacer ;"
          "         :button {"
          "           label = \"" (LgT "Filter..." "Filtre..." nil) "\" ;"
          "           key = \"Filter_" (itoa i) "\" ;"
          "           width = 9 ;"
          "         }"
          "       }"
        )
        file
      )
    )
    (write-line
      (strcat
        "     }"
        "   }"
        "   spacer ;"
        "   ok_cancel_help ;"
        " }"
      )
      file
    )
    (close file)
    (setq DCL_ID (load_dialog filename))
    (if (not (new_dialog "TOTLENGTH_PropertyLevels" DCL_ID))
      (exit)
    )
    (set_tile "Level_1" "1")
    (setq i 0)
    (repeat (length lst)
      (setq i (1+ i))
      (action_tile (strcat "Level" (itoa i)) "(RadioButton $value i)")
      (action_tile (strcat "Value" (itoa i)) "(PopupList $value i)")
    )
  )
  
  (setq
    obj-lst
      (list
        (cons "AcDbArc"         "ARC")
        (cons "AcDbCircle"      "CIRCLE")
        (cons "AcDbEllipse"     "ELLIPSE")
        (cons "AcDbLine"        "LINE")
        (cons "AcDbPolyline"    "LWPOLYLINE")
        (cons "AcDb3dPolyline"  "POLYLINE")
        (cons "AcDbSpline"      "SPLINE")
      )
    all-lst
      (list
        (mapcar 'car (vla-collection->list nil 'layers 0))
        (mapcar '(lambda (s) (substr (car s) 5)) obj-lst)
        nil
        (mapcar 'car (vla-collection->list nil 'linetypes 0))
      )
  )
  (setq ppt-lst
    (list
      (list
        (cons 8 (LgT "Layer" "Calque" nil))
        (quote
          (ListBox
            (LgT "Selection of layers" "Sélection des calques" nil)
            (LgT "Please, choose one or several layers :" "Veuillez sélectionner un ou plusieurs calque :" nil)
            ppl
            (getvar "CLAYER")
            2
          )
        )
      )
      (list
        (cons 0 (LgT "Object type" "Type d'objet" nil))
        (quote
          (ListBox
            (LgT "Selection of object types" "Sélection des types d'objet" nil)
            (LgT "Please, choose one or several object types :" "Veuillez sélectionner un ou plusieurs type d'objet :" nil)
            ppl
            nil
            2
          )
        )
      )
      (list
        (cons 62 (LgT "Color" "Couleur" nil))
        (quote (acad_colordlg 256))
      )
      (list
        (cons 6 (LgT "Line type" "Type de ligne" nil))
        (quote
          (ListBox
            (LgT "Selection of line types" "Sélection des types de ligne" nil)
            (LgT "Please, choose one or several line types :" "Veuillez sélectionner un ou plusieurs type de ligne :" nil)
            ppl
            (getvar "CELTYPE")
            2
          )
        )
      )
    )
  )
  (setq mode
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nSpecify the selection mode between"
        "\nSpécifiez la méthode de sélection entre"
        nil
      )
      (list
        (LgT
          "Layers Objects _Layers Objects"
          "Calques Objets _Layers Objects"
          nil
        )
      )
      " : "
      "Objects"
      (LgT
        (strcat
          "\nTOTLENGTH : Selection mode for objects"
          "\nDefault value:     Objects"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             |   Opens a dialog box with the list of all layers of the drawing     |"
          "\n  |             |   and lets you select one or severall layers withing the list.      |"
          "\n  |             |   The current layer is selected by default at the opening of        |"
          "\n  |    Layers   |   dialog box. If you pre-selects objects before running TOTLENGTH,  |"
          "\n  |             |   the objects pre-selected will be filtered by the entity type      |"
          "\n  |             |   listed below and the selected layers and you will not be able to  |"
          "\n  |             |   select more objects. If not, you will have to select objects      |"
          "\n  |             |   that match the filter (entity type and layers)                    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             |   Allows you to select only POLYLINES (2D and/or 3D), LINES, ARCS,  |"
          "\n  |             |   CIRCLES, ELLIPSES and SPLINES. If you pre-selects objects before  |"
          "\n  |   Objects   |   running TOTLENGTH, the objects pre-selected will be filtered by   |"
          "\n  |             |   the entity type listed above and you will not be able to select   |"
          "\n  |             |   more objects. If not, you will have to select objects that match  |"
          "\n  |             |   the filter (entity type only)                                     |"
          "\n  +-------------+---------------------------------------------------------------------+"
        )
        (strcat
          "\nTOTLENGTH : Mode de sélection des objets"
          "\nValeur par défaut: Objets"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             |   Ouvre une boîte de dialogue avec la liste de l'ensemble des       |"
          "\n  |             |   calques contenus dans le dessin et vous permet de sélectionner    |"
          "\n  |             |   un ou plusieurs calques parmi la liste.                           |"
          "\n  |             |   Le calque courant est par défaut sélectionner à l'ouverture de    |"
          "\n  |   Calques   |   la boîte de dialogue. Si vous pré-sélectionnez des objets avant   |"
          "\n  |             |   de lancer TOTLENGTH, les objets pré-sélectionnés seront filtrés   |"
          "\n  |             |   selon leur nom d'entité listé ci-dessous ainsi que par leur       |"
          "\n  |             |   calques et vous ne pourrez pas sélectionner plus d'objets. Si     |"
          "\n  |             |   non, vous devrez sélectionner des objets qui correspondent au     |"
          "\n  |             |   filtre (type d'objet et calque)                                   |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             |   Vous permet de sélectionner uniquement des POLYLIGNES (2D et/ou   |"
          "\n  |             |   3D), LIGNES, ARCS, CERCLES, ELLIPSES et SPLINES. Si vous          |"
          "\n  |             |   pré-sélectionnez des objets avant de lancer TOTLENGTH, les objets |"
          "\n  |    Objets   |   pré-sélectionnés seront filtrés selon leur type d'entité listé    |"
          "\n  |             |   ci-dessus et vous ne pourrez pas sélectionner plus d'objets. Si   |"
          "\n  |             |   non, vous devrez sélectionner des objets qui correspondent au     |"
          "\n  |             |   filtre (type d'objet uniquement)                                  |"
          "\n  +-------------+---------------------------------------------------------------------+"
        )
        nil
      )
    )
  )
  (cond
    ( (= mode "Layers")
      (setq ll
        (ListBox
          (LgT
            "TOTLENGTH: Layer(s) selection"
            "TOTLENGTH: Sélection des calque(s)"
            nil
          )
          (LgT
            "Please, select one or more layer(s) as filter :"
            "Veuillez sélectionner un ou plusieurs calque(s) comme filtre :"
            nil
          )
          (vl-sort '< (mapcar 'car (vla-collection->list nil 'layers 0)))
          (getvar "CLAYER")
          2
        )
      )
    )
  )
  (setq lst-len
    (loop-a-list-properties
      (setq jsel (ssget (vl-remove nil (list '(0 . "ARC,CIRCLE,ELLIPSE,LINE,LWPOLYLINE,POLYLINE,SPLINE") (if ll (cons 8 (lst2str ll ",")))))))
      (vl-remove nil (list (if ll 8) 0))
      (quote (vlax-curve-getDistAtParam name (vlax-curve-getEndParam name)))
      'cons
      T
    )
  )
  (setq lst-num
    (loop-a-list-properties
      jsel
      (vl-remove nil (list (if ll 8) 0))
      1
      '+
    )
  )
  (setq coeff
    (getkdh
      (quote (getreal msg))
      (LgT
        "\nSpecify a multiplier to apply to the lengths"
        "\nSpécifiez un multiplicateur à appliquer aux longueurs"
        nil
      )
      (list 6)
      " : "
      1.0
      (LgT
        (strcat
          "\nTOTLENGTH : Multiplier's effect explanation"
          "\nDefault value:     1.0"
          "\n  The multiplier corresponds to the margin you want to apply on the given lengths. It   "
          "\n  means that if you want to know the real length of your selected object, you will have "
          "\n  to have a multiplier equal to 1.0. The multiplier can't be null or negative.          "
          "\n  For example:                                                                          "
          "\n    - A multiplier of 1.05 corresponds to a 5% margin added to the real lengths         "
          "\n    - A multiplier of 0.95 corresponds to a 5% margin solded to the real lengths        "
          "\n  You can't add a constant value for each length, it has to be a multiplier.            "
        )
        (strcat
          "\nTOTLENGTH : Explication de l'effet du multiplicateur"
          "\nValeur par défaut: 1.0"
          "\n  Le multiplicateur correspond à la marge que vous souhaitez appliquer au longueurs     "
          "\n  calculées. Cela signifie que si vous voulez connaître la longueur réelle des objets   "
          "\n  sélectionnés, il faut spécifier un multiplicateur égal à 1.0. Le coefficient ne peut  "
          "\n  ni être nul, ni être négatif.                                                         "
          "\n  Par exemple:                                                                          "
          "\n    - Un coefficient de 1.05 correspond à 5% de marge supplémentaire sur les longueurs  "
          "\n    - Un coefficient de 0.95 correspond à 5% de marge soustraite aux longueurs réelles  "
          "\n  Vous ne pouvez pas additionner une longueur constante pour chaque longueur, cela ne   "
          "\n  peut qu'être un coefficient de multiplication.                                        "
        )
        nil
      )
    )
  )
  (TtLg_ResultBox)
)

(defun f (l)
  (mapcar
    '(lambda (i) (nth i l))
    (vl-sort-i
      (mapcar
        '(lambda (s / m)
          (if (setq m (RegExpExecute s "(^.+\\D)(\\D+$)" nil nil))
            (caddar m)
            (list s s)
          )
         )
        l
      )
      '(lambda (a b)
        (if (= (car a) (car b))
          (< (atoi (cadr a)) (atoi (cadr b)))
          (< (car a) (car b))
        )
       )
    )
  )
)