
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                                 --{  ...  }--                                                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] ... []-----------------------[]                                            ;
;--- Date of creation       > ##/##/####                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > #.#.#                                                                                                                 ;
;--- Class                  > "XxXxxxx"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (...) returns [...]                                                                                                                 ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:DoDocs ( / param dwglst lst )
  (and
    (setq param
      (list
        (cons
          "Activate"
          (list
            '("Name" "Nommer")
          )
        )
        (cons
          "Close"
          (list
            '("Current" "Courant")
            '("allButActive" "tousSaufActif")
            '("List" "Liste")
          )
        )
        (cons
          "PurgeAll"
          (list
            '("Current" "Courant")
            '("All" "Tous")
            '("allButActive" "tousSaufActif")
            '("List" "Liste")
          )
        )
        (cons
          "Regen"
          (list
            '("Current" "Courant")
            '("All" "Tous")
            '("allButActive" "tousSaufActif")
            '("List" "Liste")
          )
        )
        (cons
          "Save"
          (list
            '("Current" "Courant")
            '("All" "Tous")
            '("allButActive" "tousSaufActif")
            '("List" "Liste")
          )
        )
;        (cons
;          "savEAs"
;          (list
;            '("Current" "Courant")
;            '("All" "Tous")
;            '("allButActive" "tousSaufActif")
;            '("List" "Liste")
;          )
;        )
        (cons
          "save&close"
          (list
            '("Current" "Courant")
            '("allButActive" "tousSaufActif")
            '("List" "Liste")
          )
        )
      )
    )
    (setq dwglst (vla-collection->list (vlax-get-acad-object) 'Documents 1))
    (setq method
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease, select a method"
          "\nVeuiller sélectionner une méthode"
          nil
        )
        (list (lst2str (mapcar 'car param) " "))
        " : "
        "Save"
        nil
      )
    )
    (setq param (cdr (assoc method param)))
    (setq lst
      (getkdh
        (quote (getkword msg))
        (strcat
          (LgT
            "\nSpecify which drawing(s) do you want to "
            "\nSpécifier quel(s) dessin(s) vous souhaitez "
            nil
          )
          (strcase method)
        )
        (LgT
          (list (lst2str (list (lst2str (mapcar 'car param) " ") (lst2str (mapcar 'car param) " ")) " _"))
          (list (lst2str (list (lst2str (mapcar 'cdr param) " ") (lst2str (mapcar 'car param) " ")) " _"))
          nil
        )
        " : "
        nil
        nil
      )
    )
    (cond
      ( (= "Current" lst) (setq lst (getvar "DWGNAME")))
      ( (= "Name" lst)
        (setq lst
          (ListBox
            (LgT "DoDocs: Drawing selection" "DoDocs: Sélection du dessin" nil)
            (strcat (LgT "Please select the drawing to " "Veuiller sélectionner le dessin à " nil) (strcase method) " :")
            (mapcar 'car dwglst)
            (getvar "DWGNAME")
            0
            nil
          )
        )
      )
      ( (= "All" lst) (setq lst (mapcar 'car dwglst)))
      ( (= "allButActive" lst) (setq lst (vl-remove (getvar "DWGNAME") (mapcar 'car dwglst))))
      ( (= "List" lst)
        (setq lst
          (ListBox
            (LgT "DoDocs: Drawing(s) selection" "DoDocs: Sélection des dessin(s)" nil)
            (strcat (LgT "Please select one or more drawing(s) to " "Veuiller sélectionner un ou plusieurs dessin(s) à " nil) (strcase method) " :")
            (mapcar 'car dwglst)
            (getvar "DWGNAME")
            2
            nil
          )
        )
      )
    )
  )
)