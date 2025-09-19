
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  ADM-Close  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ADM-Close []-----------------------[]                                        ;
;--- Date of creation       > 15/09/2022                                                                                                            ;
;--- Last modification date > 19/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Corresponds to the 'Close method from (ApplyDocsMethods) and c:ADM command. Closes one or several drawing(s) if founded in Documents collection ;
;   and saves the changes or not if specified.                                                                                                      ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.0.0 - 05/03/2021 (Luna)                                                    ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;   --•  "UtFil" ---> ApplyDocsMethods                              | v2.0.0 - 05/07/2022 (Glenn_White/Luna)                                        ;
;   --•  "UtUse" ---> getkdh                                        | v2.2.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlCol" ---> vla-collection->list                          | v2.0.0 - 19/08/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (ADM-Close) returns the name of the selected drawings in the initial drawing before closing them and saving them if specified.      ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Specify the default path for new saved drawings as "C:\\Users\\username\\Documents" instead of 'Path property from ActiveDoc   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:ADM-Close ( / dwg docs doc mode )
  (and
    (setq dwg (getvar "DWGNAME"))
    (setq docs (mapcar 'car (vla-collection->list (vlax-get-acad-object) 'Documents 1)))
    (setq doc
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease enter the drawing name(s) to close or"
          "\nEntrer le nom du/des dessin(s) à fermer ou"
          nil
        )
        (list 128 (LgT "allButActive List _allButActive List" "tousSaufCourant Liste _allButActive List" nil))
        " : "
        (car (vl-remove dwg docs))
        nil
      )
    )
    (cond
      ( (= "allButActive" doc) (setq doc T))
      ( (= "List" doc)
        (setq doc
          (ListBox
            (LgT "ADM-Close: Drawing(s) selection" "ADM-Close: Sélection du/des dessin(s)" nil)
            (LgT "Please select drawing(s) within the list :" "Sélectionner un/des dessin(s) dans la liste :" nil)
            (vl-remove dwg docs)
            dwg
            2
            nil
          )
        )
      )
      ( (setq doc (mapcar '(lambda (x) (if (wcmatch (strcase x) "*.DWG") x (strcat x ".DWG"))) (str2lst doc ","))))
    )
    (if (listp doc)
      (setq doc (vl-remove-if-not '(lambda (x) (member (strcase x) (mapcar 'strcase doc))) docs))
      T
    )
    (setq doc (if (and (listp doc) (member dwg doc)) (vl-remove dwg doc) doc))
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT "\nDo you want to save the changes?" "\nVoulez-vous enregistrer les modifications ?" nil)
        (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
        " "
        "Yes"
        nil
      )
    )
    (cond
      ( (= "Yes" mode) (setq mode :vlax-true))
      ( (= "No" mode) (setq mode :vlax-false))
    )
    (setq doc (ApplyDocsMethods doc (list (cons 'Close mode))))
    (princ
      (strcat
        "\n " (itoa (length (cdr doc))) " / " (itoa (car doc))
        (LgT
          (strcat " drawings has been closed successfully and " (if (= mode :vlax-true) "saved" "not saved") ":")
          (strcat " dessin(s) a/ont été fermé(s) avec succès et " (if (= mode :vlax-true) "enregistré(s)" "pas enregistré(s)") " :")
          nil
        )
        "\n  • "
        (lst2str (mapcar 'car (cdr doc)) "\n  • ")
        (if (= mode :vlax-true)
          (strcat
            "\n"
            (LgT "(for the new drawings saved, check here : \"" "(pour les nouveaux dessins enregistrés, vérifier ici : \"" nil)
            (strcat "C:\\Users\\" (getenv "USERNAME") "\\Documents")
            "\")"
          )
          ""
        )
      )
    )
  )
  (princ)
)