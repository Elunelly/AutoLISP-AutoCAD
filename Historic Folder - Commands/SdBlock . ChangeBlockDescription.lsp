
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                       --{  ChangeBlockDescription  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] ChangeBlockDescription []-----------------------[]                                   ;
;--- Date of creation       > 26/03/2024                                                                                                            ;
;--- Last modification date > 26/03/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "SdBlock"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allow the user to update the description of a block in order to follow the version of all of its modification. The description of a block is    ;
;   define as follow :                                                                                                                              ;
;     Created:                                                                                                                                      ;
;        > ##/##/#### - ##:##:##                                                                                                                    ;
;        > 'username1'                                                                                                                              ;
;     Modified:                                                                                                                                     ;
;        > ##/##/#### - ##:##:##                                                                                                                    ;
;        > 'username2'                                                                                                                              ;
;     Version:                                                                                                                                      ;
;      • #.#.#.#                                                                                                                                    ;
;        > ...........................                                                                                                              ;
;      • #.#.#.#                                                                                                                                    ;
;        > ..........                                                                                                                               ;
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
;   The command (CHANGBLOCKDESCRIPTION) returns [...]                                                                                                                 ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:ChangeBlockDescription (/ UpdateBlockDesc blks lst user date dyn att ins drw desc)
  (defun UpdateBlockDesc (blk / CBD-Desc CBD-Version CBD-Create-Desc CBD-Update-Desc name entlist str l0 l2 l3 l5 l6)
;;;    (defun CBD-Desc (name desc / item blst)
;;;      (cond
;;;        ( (assoc 4 (setq blst (entget name)))
;;;          (entmake (subst (cons 4 desc) (assoc 4 blst) blst))
;;;        )
;;;        ( T
;;;          (entmake (append blst (list (cons 4 desc))))
;;;        )
;;;      )
;;;      (setq item (cdr (assoc -2 blst)))
;;;      (while item
;;;        (entmake (entget item))
;;;        (setq item (entnext item))
;;;      )
;;;      (entmake (list (cons 0 "ENDBLK")))
;;;    ) ;; NE PRENDS PAS EN COMPTE LE DYNAMISME DES BLOCS !!!
    (defun CBD-Version (s a b c d / l)
      (setq l (mapcar 'atoi (str2lst s ".")))
      (cond
        (a (setq l (list (1+ (car l)) 0 0 0)))
        (b (setq l (list (car l) (1+ (cadr l)) 0 0)))
        (c (setq l (list (car l) (cadr l) (1+ (caddr l)) 0)))
        (d (setq l (list (car l) (cadr l) (caddr l) (1+ (last l)))))
      )
      (lst2str l ".")
    )
    (defun CBD-Create-Desc (entlist / txt)
      (setq txt
        (strcat
          "URBASOLAR-CBD"
          "\nCreated:"
          "\n   > " date
          "\n   > " user
          "\nModified:"
          "\n   > " date
          "\n   > " user
          "\nVersion:"
          "\n • " (CBD-Version "1.0.0.0" dyn att ins drw)
          "\n   > " desc
        )
      )
;;;      (CBD-Desc name txt)
;;      (setq entlist (append entlist (list (cons 4 txt))))
;;      (entmod entlist)
    )
    (defun CBD-Update-Desc (entlist / lst txt i line v)
      (setq
        lst (str2lst str "\n")
        i 0
      )
      (while (setq line (car lst))
        (cond
          ( (= i 5)
            (setq line (strcat "   > " date))
          ) ;Date of modification
          ( (= i 6)
            (setq line (strcat "   > " user))
          ) ;Author of modification
          ( (= i 8)
            (setq
              v (strcat " • " (CBD-Version (substr line 4) dyn att ins drw))
              txt (cons v txt)
              txt (cons (strcat "   > " desc) txt)
            )
          ) ;Author of modification
        )
        (setq
          txt (cons line txt)
          lst (cdr lst)
          i (1+ i)
        )
      )
      (setq txt (lst2str (reverse txt) "\n"))
;;;      (CBD-Desc name txt)
;;      (entmod (subst (cons 4 txt) (assoc 4 entlist) entlist))
    )
    (setq
      name (tblobjname "BLOCK" blk)
      entlist (entget name)
      str (cdr (assoc 4 entlist))
    )
    (cond
      ( (not str)
        (CBD-Create-Desc entlist)
      ) ;Creation of a brand new description
      ( (wcmatch str "URBASOLAR-CBD*")
        (if (or dyn att ins drw)
          (CBD-Update-Desc entlist)
          (princ (strcat "\n" blk (LgT " isn't modified..." " n'est pas modifié..." nil)))
        )
      ) ;Modification of an existing description from URBASOLAR made by the command CBD
      ( T
        (CBD-Create-Desc (vl-remove (assoc 4 entlist) entlist))
      )
    )
    (princ (strcat "\n" blk (LgT " - Description (see below):\n" " - Description (voir ci-dessous) :\n" nil) (cdr (assoc 4 (entget name)))))
  )
  (and
    (setq blks (vl-sort (flt_tbl "BLOCK" "~`**" nil) '<))
    (setq lst
      (ListBox
        (LgT "Block Description" "Description de bloc" nil)
        (LgT "Select one or several block within the list:" "Sélectionner un ou plusieurs bloc dans la liste :" nil)
        blks
        nil
        2
        15
      )
    )
    (setq user (getenv "USERNAME"))
    (setq date (get-date))
    (setq dyn
      (getkdh
        (quote (getkword msg))
        (LgT "\nDid you add/modify/delete a dynamic parameter/action" "\nAvez-vous ajouté/modifié/supprimé un.e paramètre/action dynamique" nil)
        (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
        " ? "
        "No"
        nil
      )
    )
    (setq att
      (getkdh
        (quote (getkword msg))
        (LgT "\nDid you add/modify/delete an attribute" "\nAvez-vous ajouté/modifié/supprimé un attribut" nil)
        (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
        " ? "
        "No"
        nil
      )
    )
    (setq ins
      (getkdh
        (quote (getkword msg))
        (LgT "\nDid you add/modify/delete the insertion point" "\nAvez-vous ajouté/modifié/supprimé le point d'insertion" nil)
        (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
        " ? "
        "No"
        nil
      )
    )
    (setq drw
      (getkdh
        (quote (getkword msg))
        (LgT "\nDid you add/modify/delete any other object" "\nAvez-vous ajouté/modifié/supprimé tout autre objet" nil)
        (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
        " ? "
        "Yes"
        nil
      )
    )
    (setq desc (getstring T (LgT "\nEnter a description of modifications: " "\nEntrez une description des modifications : " nil)))
    (mapcar 'UpdateBlockDesc lst)
  )
  (princ)
)