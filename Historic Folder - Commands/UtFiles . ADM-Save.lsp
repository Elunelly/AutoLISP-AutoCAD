
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  ADM-Save  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ADM-Save []-----------------------[]                                         ;
;--- Date of creation       > 15/09/2022                                                                                                            ;
;--- Last modification date > 22/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Corresponds to the 'Save method from (ApplyDocsMethods) and c:ADM command. Saves one or several drawing(s) if founded in Documents collection.  ;
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
;   The command (ADM-Save) returns the name of the selected drawings in the initial drawing after saving them.                                      ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Specify the default path for new saved drawings as "C:\\Users\\username\\Documents" instead of 'Path property from ActiveDoc   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:ADM-Save ( / dwg docs doc )
  (and
    (setq dwg (getvar "DWGNAME"))
    (setq docs (mapcar 'car (vla-collection->list (vlax-get-acad-object) 'Documents 1)))
    (setq doc
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease enter the drawing name(s) to save or"
          "\nEntrer le nom du/des dessin(s) à enregistrer ou"
          nil
        )
        (list 128 (LgT "All allButActive List _All allButActive List" "Tous tousSaufCourant Liste _All allButActive List" nil))
        " : "
        dwg
        nil
      )
    )
    (cond
      ( (= "All" doc) (not (setq doc nil)))
      ( (= "allButActive" doc) (setq doc T))
      ( (= "List" doc)
        (setq doc
          (ListBox
            (LgT "ADM-Save: Drawing(s) selection" "ADM-Save: Sélection du/des dessin(s)" nil)
            (LgT "Please select drawing(s) within the list :" "Sélectionner un/des dessin(s) dans la liste :" nil)
            docs
            dwg
            2
            nil
          )
        )
      )
      ( (setq doc (mapcar '(lambda (x) (if (wcmatch (strcase x) "*.DWG") x (strcat x ".DWG"))) (str2lst doc ","))))
    )
    (if (and doc (listp doc))
      (setq doc (vl-remove-if-not '(lambda (x) (member (strcase x) (mapcar 'strcase doc))) docs))
      T
    )
    (cdr (setq doc (ApplyDocsMethods doc (list (cons 'Save nil)))))
    (princ
      (strcat
        "\n " (itoa (length (cdr doc))) " / " (itoa (car doc))
        (LgT
          " drawings has been saved successfully :"
          " dessin(s) a/ont été enregistré(s) avec succès :"
          nil
        )
        "\n  • "
        (lst2str (mapcar 'car (cdr doc)) "\n  • ")
        "\n\n"
        (LgT "(for the new drawings saved, check here : \"" "(pour les nouveaux dessins enregistrés, vérifier ici : \"" nil)
        (strcat "C:\\Users\\" (getenv "USERNAME") "\\Documents")
        "\")"
      )
    )
  )
  (princ)
)