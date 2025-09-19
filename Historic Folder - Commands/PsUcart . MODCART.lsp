
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  MODCART  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] MODCART []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 12/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.2.1                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Modify the attributes of Title blocks related to the modification of the layout. You can reset all the concerned attributes ("MODIFICATION" and ;
;   "AUTEUR") to an empty string (for new design for example) or add a new modification string to a list of layouts (for global modifications for   ;
;   example). Actually the option "-1" is not available. It was able to remove a modification string from the a list of layouts and roll them down. ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> FormAssoc                                     | v1.0.0 - 30/06/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> loop-a-list-properties                        | v2.0.0 - 28/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.0.0 - 05/03/2021 (Luna)                                                    ;
;   --•  "UtDis" ---> alert-princ                                   | v1.0.0 - 26/01/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.0.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> get-date                                      | v1.0.0 - 19/06/2020 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> select-filter                                 | v3.1.1 - 01/01/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> set-att-list                                  | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlPrp" ---> get-dyn-list                                  | v3.0.1 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlPrp" ---> get-layouts-pos                               | v2.0.0 - 24/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (MODCART) returns [...]                                                                                                             ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.2.1   |   Set the search key name for Cartridge on "*Cartouche*" instead of "Cartouche*"                                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.2.0   |   Add (while) to retrieve the modification's text and the author name ("+1" method) and add the information about maximum        | ;
; |            |   characters "allowed" before they're out of the box (depending on the format and the orientation of the block)                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Add the modification of the "DATE" attribute for the modified title blocks thanks to (get-date)                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version (without the option "-1" for now)                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:MODCART ( / param ignore date format lst layout block sep lng tmp laylist mode att blst str-mod str-aut)  
  (sssetfirst)
  (and
    (setq param
      (list
        (list "MODIFICATIONS_IND_F" "AUTEUR_F")
        (list "MODIFICATIONS_IND_E" "AUTEUR_E")
        (list "MODIFICATIONS_IND_D" "AUTEUR_D")
        (list "MODIFICATIONS_IND_C" "AUTEUR_C")
        (list "MODIFICATIONS_IND_B" "AUTEUR_B")
        (list "MODIFICATIONS_IND_A" "AUTEUR_A")
      )
    )
    (setq ignore (list "MODIFICATIONS_IND_A" "AUTEUR_A"))
    (setq date (cons "DATE" (substr (get-date) 1 10)))
    (setq format
      (list
        (append
          (mapcar 'car param)
          (list '(lambda (x) (substr (car x) (strlen (car x)))))
        )
        " > ["
        (list "PHASE" "Visibilité1" "Visibility1" 'cdr)
        "] "
        'layout
;-;        (list '(lambda (s) (mAtom " " (1+ (fix (* 2.5 (- lng (strlen layout))))) 'strcat)))
;-;        (setq sep "  |  ")
;-;        'block
        " ("
        (list "FORMAT" 'cdr)
        " | "
        (append (mapcar 'cadr param) (list 'cdr))
        ")"
      )
    )
    (setq lst
      (loop-a-list-properties
        (select-filter "BLC" '("*Cartouche*" "`*U*") (list "_X" (list (cons 410 (lst2str (layoutlist) ",")))) nil nil)
        (list 410 'EffectiveName)
        (quote (list (cons (cdr (assoc 5 (entget name))) (append (get-att-list name) (get-dyn-list name)))))
        'append
        T
      )
    )
    (null (sssetfirst))
    (setq lst
      (mapcar
        '(lambda (x) (cons x (cdr (assoc x lst))))
        (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
      )
    )
    (setq lst (vl-remove-if '(lambda (x) (null (cdr x))) lst))
    (setq lng (apply 'max (mapcar '(lambda (x) (strlen (car x))) lst)))
    (setq tmp
      (vl-remove
        nil
        (apply
          'append
          (mapcar
            '(lambda (x / layout tmp)
              (setq
                layout (car x)
                tmp (cdr x)
              )
              (apply
                'append
                (mapcar
                  '(lambda (x / block tmp)
                    (setq
                      block (car x)
                      tmp (cdr x)
                    )
                    (apply
                      'append
                      (mapcar
                        '(lambda (x / hdl blst tmp)
                          (setq
                            hdl (car x)
                            blst (cdr x)
                            tmp (FormAssoc format blst 31)
                          )
                          (if tmp (list (cons tmp hdl)))
                        )
                        tmp
                      )
                    )
                  )
                  tmp
                )
              )
            )
            lst
          )
        )
      )
    )
    (setq laylist
      (ListBox
        (LgT
          "MODCART: Layout tab(s) selection"
          "MODCART: Sélection des présentation(s)"
          nil
        )
        (LgT
          "Please, select one or more layout tab(s) to be modified :"
          "Veuillez sélectionner la ou les présentation(s) à modifier :"
          nil
        )
        (mapcar 'car tmp)
        (cond
          ( (= "Model" (getvar "CTAB")) (mapcar 'car tmp))
          ( (car (nth (1- (cdr (assoc (getvar "CTAB") (get-layouts-pos)))) tmp)))
        )
        2
        nil
      )
    )
    (setq lst (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))))
    (princ
      (strcat
        (LgT
          "\nLayout tab(s) list = "
          "\nListe des présentation(s) = "
          nil
        )
        (lst2str
          (mapcar
            '(lambda (s) (vl-string-right-trim " " (substr s 1 (vl-string-search (cond (sep) ("")) s))))
            laylist
          )
          ", "
        )
      )
    )
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nSelect an option"
          "\nSélectionnez une option"
          nil
        )
        (list "Reset +1") ; -1
        " : "
        "+1"
        (LgT
          (strcat
            "\nMODCART : Available methods"
            "\nDefault value:     +1"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | For the selected layout tab(s), deletes every value defined in the  |"
            "\n  |    Reset    | attributes \"MODIFICATIONS_IND_[B-F]\" and \"AUTEUR_[B-F]\" and keep    |"
            "\n  |             | the index A as \"INITIAL DESIGN\" (if english) or \"CREATION DU PLAN\"  |"
            "\n  |             | (if french)                                                         |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | For the selected layout tab(s), adds a new value in the first empty |"
            "\n  |             | attribute \"MODIFICATIONS_IND_[B-F]\" and \"AUTEUR_[B-F]\" or roll      |"
            "\n  |      +1     | down all these attributes's value to insert the new value on top of |"
            "\n  |             | them without deleting the last modification's name (WARNING:        |"
            "\n  |             | Depending on the version of the title block, the letter will not    |"
            "\n  |             | corresponds to the real version of the layout !)                    |"
            "\n  +-------------+---------------------------------------------------------------------+"
;-;            "\n  |             | For the selected layout tab(s), opens a dialog box with every       |"
;-;            "\n  |             | attribute \"MODIFICATIONS_IND_[B-F]\" value and you will be able to   |"
;-;            "\n  |      -1     | delete the selected values for every selected layouts. If the value |"
;-;            "\n  |             | is not on top of the list of attributes, the program will roll down |"
;-;            "\n  |             | the upper values                                                    |"
;-;            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          (strcat
            "\nMODCART : Méthodes disponibles"
            "\nValeur par défaut: +1"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | Pour les présentation(s) sélectionnée(s), supprime toutes les       |"
            "\n  |    Reset    | valeurs définies dans les attributs \"MODIFICATIONS_IND_[B-F]\" et    |"
            "\n  |             | \"AUTEUR_[B-F]\" en conservant l'indice A sur \"INITIAL DESIGN\" (si    |"
            "\n  |             | anglais) ou \"CREATION DU PLAN\" (si français)                        |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | Pour les présentation(s) sélectionnée(s), ajoute une nouvelle       |"
            "\n  |             | valeur dans le premier attribut \"MODIFICATIONS_IND_[B-F]\" et        |"
            "\n  |             | \"AUTEUR_[B-F]\" vide ou effectue un roulement vers le bas pour       |"
            "\n  |      +1     | toutes les valeurs de ces attributs afin d'insérer la nouvelle en   |"
            "\n  |             | haut de la liste sans supprimer le nom de la dernière modification  |"
            "\n  |             | (ATTENTION: Selon la version du cartouche, la lettre peut ne pas    |"
            "\n  |             | correspondre à la version réelle de la présentation !)              |"
            "\n  +-------------+---------------------------------------------------------------------+"
;-;            "\n  |             | Pour les présentation(s) sélectionnée(s), ouvre une boîte de        |"
;-;            "\n  |             | dialogue avec toutes les valeurs de \"MODIFICATIONS_IND_[B-F]\" et    |"
;-;            "\n  |      -1     | vous pourrez ainsi supprimer les valeurs sélectionnées for chaque   |"
;-;            "\n  |             | présentations. Si la valeur n'est pas en haut de la liste des       |"
;-;            "\n  |             | attributs, le programme effectuera un roulement vers le bas des     |"
;-;            "\n  |             | valeurs situées au-dessus des valeurs supprimées                    |"
;-;            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          nil
        )
      )
    )
    (cond
      ( (= mode "Reset")
        (mapcar
          '(lambda (x / hdl name blst att ind)
            (setq
              x (assoc x tmp)
              hdl (cdr x)
              name (handent hdl)
              blst (cdr (assoc hdl lst))
              att
                (set-att-list
                  name
                  (append
                    (list date)
                    (mapcar
                      '(lambda (x) (cons (car x) ""))
                      (vl-remove-if-not
                        '(lambda (x) (and (not (member (car x) ignore)) (member (car x) (apply 'append param))))
                        blst
                      )
                    )
                  )
                )
              blst
                (vl-sort
                  (vl-remove-if-not '(lambda (x) (and (member (car x) (apply 'append param)) (/= (cdr x) ""))) (get-att-list name))
                  '(lambda (e1 e2) (> (car e1) (car e2)))
                )
              ind (chr (apply 'max (mapcar '(lambda (x) (ascii (substr (car x) (strlen (car x))))) blst)))
            )
            (princ
              (strcat
                (LgT
                  "\n  •> The layout tab \""
                  "\n  •> La présentation \""
                  nil
                )
                (car x)
                (LgT
                  "\" is now defined on modification index → "
                  "\" est désormais définie sur l'indice de modification → "
                  nil
                )
                ind
                " ("
                (lst2str (mapcar 'cdr (vl-remove-if-not '(lambda (x) (wcmatch (car x) (strcat "*" ind))) blst)) " - ")
                ")"
              )
            )
           )
          laylist
        )
      )
      ( (= mode "+1")
        (while
          (and
            (princ
              (LgT
                "\nFor information (max. characters): A4/A3 (Portrait) → 29, A4/A3/A2 (Landscape) → 42, A1/A0/A0+ (Landscape) → 125"
                "\nPour information (max. caractères): A4/A3 (Portrait) → 29, A4/A3/A2 (Paysage) → 42, A1/A0/A0+ (Paysage) → 125"
                nil
              )
            )
            (=
              ""
              (setq str-mod
                (strcase
                  (getstring
                    T
                    (LgT
                      "\nSpecify the new text string to add : "
                      "\nSpécifiez le nouveau texte à ajouter : "
                      nil
                    )
                  )
                )
              )
            )
          )
          (alert-princ
            (LgT
              "\nThe text value can't be empty..."
              "\nLe texte ne peut pas être vide..."
              nil
            )
          )
        )
        (while
          (=
            ""
            (setq str-aut
              (strcase
                (getstring
                  (LgT
                    "\nSpecify the author's name of this modification : "
                    "\nSpécifiez le nom de l'auteur de cette modification : "
                    nil
                  )
                )
              )
            )
          )
          (alert-princ
            (LgT
              "\nThe author value can't be empty..."
              "\nL'auteur ne peut pas être vide..."
              nil
            )
          )
        )
        (mapcar
          '(lambda (x / hdl name blst att ind)
            (setq
              x (assoc x tmp)
              hdl (cdr x)
              name (handent hdl)
              blst (cdr (assoc hdl lst))
              att (mapcar '(lambda (a) (mapcar '(lambda (b) (assoc b blst)) a)) (vl-remove-if '(lambda (x) (member (car x) ignore)) (reverse param)))
            )
            (while (and att (not (= "" (cdaar att))))
              (setq att (cdr att))
            )
            (if (setq att (mapcar '(lambda (a s) (cons (car a) s)) (car att) (list str-mod str-aut)))
              (setq
                att (set-att-list name (append (list date) att))
                ind (chr (apply 'max (mapcar '(lambda (x) (ascii (substr (car x) (strlen (car x))))) (cdr att))))
              )
              (setq
                att (mapcar '(lambda (a) (mapcar '(lambda (b) (assoc b blst)) a)) (reverse param))
                blst (append (cdr att) (list (mapcar 'cons (car param) (list str-mod str-aut))))
                att (mapcar '(lambda (o n) (mapcar '(lambda (a b) (cons (car a) (cdr b))) o n)) att blst)
                att (set-att-list name (append (list date) (apply 'append att)))
                ind (strcat (substr (caar param) (strlen (caar param))) (LgT " (rolled down)" " (déroulement)" nil))
              )
            )
            (princ
              (strcat
                (LgT
                  "\n  •> The layout tab \""
                  "\n  •> La présentation \""
                  nil
                )
                (car x)
                (LgT
                  "\" is now defined on modification index → "
                  "\" est désormais définie sur l'indice de modification → "
                  nil
                )
                ind
              )
            )
           )
          laylist
        )
        (princ
          (strcat
            (LgT
              "\nThe listed layout tab(s) (see above) are now defined with the modification text \""
              "\nLa liste des présentation(s) (voir ci-dessus) sont désormais définies avec le texte de modification \""
              nil
            )
            str-mod
            "\" (= " (itoa (strlen str-mod)) "c)"
            (LgT
              " wrote by "
              " écrit par "
              nil
            )
            str-aut
            (LgT
              " on "
              " le "
              nil
            )
            (cdr date)
          )
        )
      )
;-;      ( (= mode "-1")
;-;        (and
;-;          (setq lst
;-;            (mapcar
;-;              '(lambda (x)
;-;                (cons
;-;                  (car x)
;-;                  (vl-sort
;-;                    (vl-remove-if-not '(lambda (a) (member (car a) (apply 'append param))) (cdr x))
;-;                    '(lambda (e1 e2) (< (car e1) (car e2)))
;-;                  )
;-;                )
;-;               )
;-;              lst
;-;            )
;-;          )
;-;          (setq att
;-;            (vl-sort
;-;              (remove-duplicates
;-;                (mapcar
;-;                  'cdr
;-;                  (vl-remove-if
;-;                    '(lambda (x)
;-;                      (or
;-;                        (not (member (car x) (mapcar 'car param)))
;-;                        (member (car x) ignore)
;-;                        (= "" (cdr x))
;-;                      )
;-;                    )
;-;                    (apply 'append (mapcar 'cdr lst))
;-;                  )
;-;                )
;-;              )
;-;              '<
;-;            )
;-;          )
;-;          (setq blst
;-;            (ListBox
;-;              (LgT
;-;                "MODCART: Attribute(s) selection"
;-;                "MODCART: Sélection des attribut(s)"
;-;                nil
;-;              )
;-;              (LgT
;-;                "Please, select one or more attribute's value(s) to delete them :"
;-;                "Veuillez sélectionner une ou plusieurs valeur(s) d'attribut pour les supprimer :"
;-;                nil
;-;              )
;-;              (mapcar
;-;                '(lambda (a / n)
;-;                  (setq n (length (vl-remove-if-not '(lambda (x) (member a (mapcar 'cdr (cdr x)))) lst)))
;-;                  (strcat "[ " (itoa n) " ] " a)
;-;                )
;-;                att
;-;              )
;-;              nil
;-;              2
;-;              nil
;-;            )
;-;          )
;-;          (setq blst (mapcar '(lambda (x) (substr x (+ 4 (vl-string-search " ] " x)))) blst))
;-;        )
;-;      )
    )
    
  )
  (princ)
)