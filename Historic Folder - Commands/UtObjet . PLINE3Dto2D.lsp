
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                             --{  PLINE3Dto2D  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] PLINE3Dto2D []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 19/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Converts multiple 3D polylines into LWPOLYLINES (2D).                                                                                           ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Ask user to select 3D polylines or filter the current selection set with 3D polylines only                                      ;
; Step n°2        : Ask user to select 3D polylines or filter the current selection set with 3D polylines only                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;   --•  "UtGeo" ---> get-pt-list                                   | v3.0.0 - 31/12/2021 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> MuteSel                                       | v1.0.0 - 16/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlPrp" ---> PL32-Properties                               | v1.0.0 - 19/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (PLINE3Dto2D) returns the number of polylines successfully converted and set the new created objects as current selection set.      ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:PLINE3Dto2D (/ PL32-Properties doc param mode default jsel mode-a mode-d i n name plst zlst alt ent)
  (defun PL32-Properties (met)
    (null (vlax-put ent met (vlax-get name met)))
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq param
    (list
      (cons
        0
        (list
          (cons "miNimum" "miNimum")
          (cons "maXimum" "maXimum")
          (cons "Average" "moYenne")
          (cons "Zero" "Zéro")
        )
      )
      (cons
        1
        (list
          (cons "Yes" "Oui")
          (cons "No" "Non")
        )
      )
    )
  )
  (setq default
    (cond
      ((getenv "PLINE3Dto2D_Settings"))
      ((setenv "PLINE3Dto2D_Settings" (LgT "Zero, Yes" "Zéro, Oui" nil)))
    )
  )
  (and
    (setq default (str2lst default ", "))
    (or
      (setq jsel (ssget "_I" '((0 . "POLYLINE"))))
      (setq jsel
        (MuteSel
          (LgT
            "\nPlease select 3D polylines :"
            "\nSélectionner des polylignes 3D :"
            nil
          )
          (quote (ssget '((0 . "POLYLINE"))))
        )
      )
    )
    (setq mode-a
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nDo you want to maintain an average altitude for each polyline ?"
          "\nSouhaitez-vous conserver une altitude moyenne pour chaque polyligne ?"
          nil
        )
        (list
          (LgT
            (lst2str
              (list
                (lst2str (mapcar 'car (cdr (assoc 0 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 0 param))) " ")
              )
              " _"
            )
            (lst2str
              (list
                (lst2str (mapcar 'cdr (cdr (assoc 0 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 0 param))) " ")
              )
              " _"
            )
            nil
          )
        )
        " : "
        (car default)
        (LgT
          (strcat
            "\nPLINE3Dto2D : Polyline's elevation conversion"
            "\nDefault value:     <" (car default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   miNimum   | For each selected 3D polylines, apply the minimum Z coordinate as   |"
            "\n  |             | elevation value for the new substitute polyline object              |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   maXimum   | For each selected 3D polylines, apply the maximum Z coordinate as   |"
            "\n  |             | elevation value for the new substitute polyline object              |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   Average   | For each selected 3D polylines, calculate the average Z coordinate  |"
            "\n  |             | and set it as elevation value for the new substitute polyline       |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Zero    | For all selected 3D polylines, set the elevation value at 0.0 for   |"
            "\n  |             | all the new substitute polylines                                    |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          (strcat
            "\nPLINE3Dto2D : Conversion d'élévation des polylignes"
            "\nValeur par défaut: <" (car default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   miNimum   | Pour chaque polyligne 3D sélectionnée, utilise la coordonnée Z      |"
            "\n  |             | minimale comme valeur d'élévation pour la nouvelle polyligne        |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   maXimum   | Pour chaque polyligne 3D sélectionnée, utilise la coordonnée Z      |"
            "\n  |             | maximale comme valeur d'élévation pour la nouvelle polyligne        |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   moYenne   | Pour chaque polyligne 3D sélectionnée, calcule une coordonnée Z     |"
            "\n  |             | moyenne comme valeur d'élévation pour la nouvelle polyligne         |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Zéro    | Pour toutes les polylignes 3D sélectionnées, défini l'élévation à   |"
            "\n  |             | 0.0 pour toutes les nouvelles polylignes remplaçantes               |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          nil
        )
      )
    )
    (setq mode-d
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nErase source objects ?"
          "\nEffacer les objets source ?"
          nil
        )
        (list
          (LgT
            (lst2str
              (list
                (lst2str (mapcar 'car (cdr (assoc 1 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 1 param))) " ")
              )
              " _"
            )
            (lst2str
              (list
                (lst2str (mapcar 'cdr (cdr (assoc 1 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 1 param))) " ")
              )
              " _"
            )
            nil
          )
        )
        " : "
        (cadr default)
        (LgT
          (strcat
            "\nPLINE3Dto2D : Source objects"
            "\nDefault value:     <" (cadr default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Yes     | Erase all the selected 3D polylines, only 2D polylines remain       |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     No      | The original objects are not erased, so both polylines remain       |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          (strcat
            "\nPLINE3Dto2D : Objets source"
            "\nValeur par défaut: <" (cadr default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Oui     | Efface toutes les polylignes 3D, seules les polylignes 2D existent  |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Non     | Les objets originaux ne sont pas effacés, les polylignes 2D et 3D   |"
            "\n  |             | sont conservées dans le dessin                                      |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          nil
        )
      )
    )
    (setenv
      "PLINE3Dto2D_Settings"
      (lst2str
        (list
          (LgT mode-a (cdr (assoc mode-a (cdr (assoc 0 param)))) nil)
          (LgT mode-d (cdr (assoc mode-d (cdr (assoc 1 param)))) nil)
        )
        ", "
      )
    )
    (repeat (setq n 0 i (sslength jsel))
      (and
        (setq name (ssname jsel (setq i (1- i))))
        (setq plst (get-pt-list name))
        (setq name (vlax-ename->VLA-Object name))
        (if (= -1 (vlax-get name 'Closed)) (setq plst (reverse (cdr (reverse plst)))) T)
        (setq zlst (mapcar 'caddr plst))
        (setq alt
          (cond
            ( (= mode-a "miNimum") (apply 'min zlst))
            ( (= mode-a "maXimum") (apply 'max zlst))
            ( (= mode-a "Average") (/ (apply '+ zlst) (float (length plst))))
            ( (= mode-a "Zero") 0.0)
          )
        )
        (setq ent
          (vlax-invoke
            (vla-get-ModelSpace doc)
            'addLightWeightPolyline
            (apply
              'append
              (mapcar
                '(lambda (p) (list (car p) (cadr p)))
                plst
              )
            )
          )
        )
        (null (vla-put-Elevation ent alt))
        (PL32-Properties 'Closed)
        (PL32-Properties 'EntityTransparency)
        (PL32-Properties 'Layer)
        (PL32-Properties 'LineType)
        (PL32-Properties 'LineTypeScale)
        (PL32-Properties 'TrueColor)
        (ssdel (vlax-VLA-Object->ename name) jsel)
        (ssadd (vlax-VLA-Object->ename ent) jsel)
        (null (if (= mode-d "Yes") (vla-delete name)))
        (setq n (1+ n))
      )
    )
    (null (vla-EndUndoMark doc))
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
          " 3D polylines has been successfully transformed."
          " polylignes 3D ont été transformées avec succès."
          nil
        )
      )
    )
    (null (sssetfirst))
    (sssetfirst nil jsel)
  )
  (princ)
)