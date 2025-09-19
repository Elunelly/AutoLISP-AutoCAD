
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                             --{  ADM-Activate  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] ADM-Activate []-----------------------[]                                       ;
;--- Date of creation       > 15/09/2022                                                                                                            ;
;--- Last modification date > 06/10/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.1.0                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Corresponds to the 'Activate method from (ApplyDocsMethods) and c:ADM command. Set a drawing current if founded in Documents collection.        ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
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
;   The command (ADM-Activate) returns the name of the selected drawing in the initial drawing before setting the selected drawing as current.      ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Use the first document which is not the current drawing as default value and check if there's only one drawing opened or not   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Add the environment variable "ADM_Drawing" to save the name of current drawing if using an ADM command                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:ADM-Activate ( / tmp dwg docs doc )
  (and
    (setq docs (mapcar 'car (vla-collection->list (vlax-get-acad-object) 'Documents 1)))
    (if (not (setq tmp (car (vl-remove (getvar "DWGNAME") docs))))
      (not
        (princ
          (LgT
            "\nThere's only one drawing in the current AutoCAD application..."
            "\nIl n'y a qu'un seul dessin dans l'application en cours d'AutoCAD..."
            nil
          )
        )
      )
      tmp
    )
    (setq dwg
      (cond
        ( (setq dwg (getenv "ADM_Drawing"))
          (if (vl-position (strcase dwg) (mapcar 'strcase docs))
            dwg
            tmp
          )
        )
        ( (setenv "ADM_Drawing" tmp))
      )
    )
    (setq doc
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease enter the drawing name to activate or"
          "\nEntrer le nom du dessin à activer ou"
          nil
        )
        (list 128 (LgT "Name _Name" "Nommer _Name" nil))
        " : "
        dwg
        nil
      )
    )
    (cond
      ( (= "Name" doc)
        (setq doc
          (ListBox
            (LgT "ADM-Activate: Drawing selection" "ADM-Activate: Sélection du dessin" nil)
            (LgT "Please select a drawing within the list :" "Sélectionner un dessin dans la liste :" nil)
            docs
            dwg
            0
            nil
          )
        )
      )
      ( (not (wcmatch (strcase doc) "*.DWG")) (setq doc (strcat doc ".DWG")))
      (doc)
    )
    (setq dwg (getvar "DWGNAME"))
    (not
      (if (equal (strcase dwg) (strcase doc))
        (princ
          (LgT
            (strcat "\nThe drawing named \"" dwg "\" is already active...")
            (strcat "\nLe dessin nommé \"" dwg "\" est déjà actif...")
            nil
          )
        )
      )
    )
    (if (setq dwg (vl-position (strcase doc) (mapcar 'strcase docs)))
      (setq doc (nth dwg docs))
      (not
        (princ
          (LgT
            (strcat "\nThe drawing named \"" doc "\" wasn't found in opened drawing's list...")
            (strcat "\nLe dessin nommé \"" doc "\" n'a pas été trouvé dans la liste des dessins ouverts...")
            nil
          )
        )
      )
    )
    (princ doc)
    (setenv "ADM_Drawing" (getvar "DWGNAME"))
    (ApplyDocsMethods (list doc) (list (cons 'Activate nil)))
  )
  (princ)
)