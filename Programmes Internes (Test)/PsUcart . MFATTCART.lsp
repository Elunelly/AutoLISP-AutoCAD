
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  MFATTCART  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] MFATTCART []-----------------------[]                                         ;
;--- Date of creation       > 19/12/2022                                                                                                            ;
;--- Last modification date > 22/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Defines or redefines M-Files link for cartridge attributes, automatically (based on attributes tag) or forced (within a shorter list, to allow  ;
;   contributors or automation technicians for example).                                                                                            ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Ask between "Auto" and "Forced" completion for M-Files field                                                                    ;
; Step n°2        : While the user is selecting an object and it's an attribute                                                                     ;
;   Step n°2.a    :   If 'mode' = "Auto", then check the value of the attribute tag and set the correct field based on their name                   ;
;   Step n°2.b    :   If 'mode' = "Forced", then open a dialog box to pick the desired M-Files link and set the corresponding field                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaApp" ---> MFiles-field-generator                        | v1.0.0 - 19/12/2012 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.0.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> LM:outputtext:puttextstring                   | v1.0.0 - 16/01/2016 (LeeMac)                                                  ;
;   --•  "VlObj" ---> LM:outputtext:updatefield                     | v1.0.0 - 16/01/2016 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlObj" ---> MFA-AutoAttributeUpdate                       | v1.0.0 - 22/12/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (MFATTCART) returns the name of attribute and the M-Files link.                                                                     ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesign the whole program to have only MFATTCART, adapting M-Files field expressions for each attribute name                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:MFATTCART (/ MFA-AutoAttributeUpdate param mode ent obj tag str)
  (defun MFA-AutoAttributeUpdate (i)
    (LM:outputtext:puttextstring obj (cdr (nth i param)))
    (LM:outputtext:updatefield ent)
    (princ
      (LgT
        (strcat
          "\nThe attribute \"" tag "\""
          " is now set with the path : "
          (car (nth i param))
        )
        (strcat
          "\nL'attribut \"" tag "\""
          " est désormais défini avec le chemin : "
          (car (nth i param))
        )
        nil
      )
    )
    (princ)
  )

  (setq param
    (mapcar
      'MFiles-field-generator
      (list
        '(0 1)    ;; 00 → Projet(s) #1 > Code
        '(0 2)    ;; 01 → Projet(s) #1 > Nom du Projet
        '(0 3)    ;; 02 → Projet(s) #1 > Commune
        '(0 7)    ;; 03 → Projet(s) #1 > Créateur
        '(0 09 8) ;; 04 → Projet(s) #1 > Dessinateur(s) Projeteur (s) #1 > Triptique
        '(0 10 8) ;; 05 → Projet(s) #1 > Contributeur - Dessinateur(s) Projeteur(s) #1 > Triptique
        '(0 11 8) ;; 06 → Projet(s) #1 > Chargé(s) Affaire(s) #1 > Triptique
        '(0 12 8) ;; 07 → Projet(s) #1 > Contributeur - Chargé(s) Affaire(s) #1 > Triptique
        '(0 13 8) ;; 08 → Projet(s) #1 > Chef(s) de Projets Développement #1 > Triptique
        '(0 14 8) ;; 09 → Projet(s) #1 > Contributeur - Chef(s) de Projets Développement #1 > Triptique
        '(0 15 8) ;; 10 → Projet(s) #1 > Chargé(s) d'Etude(s) #1 > Triptique
        '(0 16 8) ;; 11 → Projet(s) #1 > Contributeur - Chargé(s) d'Etude(s) #1 > Triptique
        '(0 17 8) ;; 12 → Projet(s) #1 > Chef(s) de Projets Construction #1 > Triptique
        '(0 18 8) ;; 13 → Projet(s) #1 > Contributeur - Chef(s) de Projets Construction #1 > Triptique
        '(0 19 8) ;; 14 → Projet(s) #1 > Chargé(s) de Travaux #1 > Triptique
        '(0 20 8) ;; 15 → Projet(s) #1 > Contributeur - Chargé(s) de Travaux #1 > Triptique
        '(0 21 8) ;; 16 → Projet(s) #1 > Automaticien(s) #1 > Triptique
        '(0 22 8) ;; 17 → Projet(s) #1 > Contributeur - Automaticien(s) #1 > Triptique
      )
    )
  )
  (setq mode
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nWhich mode do you want to use"
        "\nQuel mode désirez-vous utiliser"
        nil
      )
      (list
        (LgT
          "Auto Forced _Auto Forced"
          "Auto Forcé _Auto Forced"
          nil
        )
      )
      " ? "
      "Auto"
      (LgT
        (strcat
          "\nMFATTCART : Mode's functionnalities"
          "\nDefault value:     Auto"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Check the attribute tag, and if it matches with programmed tag,     |"
          "\n  |     Auto    | defines the associated M-Files field for it (Ex.: tag = PROJET, use |"
          "\n  |             | \"Projet(s) #1 > Nom du Projet\" as M-Files link).                    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | For each attribute selected, open a dialog box to select the        |"
          "\n  |    Forced   | desired M-Files link within a list (no multiple choice), then apply |"
          "\n  |             | the associated M-Files field to the attribute.                      |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nMFATTCART : Fonctionnalités des modes"
          "\nValeur par défaut: Auto"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Vérifie le nom d'étiquette d'attribut et s'il correspond à un nom   |"
          "\n  |     Auto    | programmé, définie le champ dynamique M-Files associé (Ex.: tag =   |"
          "\n  |             | PROJET, utilise \"Projet(s) #1 > Nom du Projet\" comme lien M-Files). |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Pour chaque attribut sélectionné, ouvre une boîte de dialogue pour  |"
          "\n  |    Forcé    | choisir le lien M-Files souhaité à partir d'une liste (pas de choix |"
          "\n  |             | multiple), puis y applique le champ dynamique M-Files associé.      |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (while
    (and
      (setq ent (car (nentsel (LgT "\nSelect an attribute : " "\nSélectionner un attribut : " nil))))
      (setq obj (vlax-ename->vla-object ent))
      (= "ATTRIB" (cdr (assoc 0 (entget ent))))
      (setq tag (cdr (assoc 2 (entget ent))))
    )
    (cond
      ( (= mode "Auto")
        (cond
          ( (= "CODE" tag) (MFA-AutoAttributeUpdate 0))
          ( (= "PROJET" tag) (MFA-AutoAttributeUpdate 1))
          ( (= "COMMUNE" tag) (MFA-AutoAttributeUpdate 2))
          ( (= "PROJETEUR" tag) (MFA-AutoAttributeUpdate 4))
          ( (= "CHARGE_ETUDE" tag) (MFA-AutoAttributeUpdate 10))
          ( (= "CHEF_PROJET_DEV" tag) (MFA-AutoAttributeUpdate 8))
          ( (= "CHARGE_AFFAIRE" tag) (MFA-AutoAttributeUpdate 6))
          ( (wcmatch tag "CHEF_PROJET_CONS*") (MFA-AutoAttributeUpdate 12))
          ( (= "CONDUCTEUR_TRAVAUX" tag) (MFA-AutoAttributeUpdate 14))
          ( (wcmatch tag "AUTEUR_*") (MFA-AutoAttributeUpdate 4))
          ( (princ
              (LgT
                (strcat "\nNo automatic completion for the attribute \"" tag "\"...")
                (strcat "\nAucune complétion automatique pour l'attribut \"" tag "\"...")
                nil
              )
            )
          )
        )
      )
      ( (= mode "Forced")
        (setq str
          (ListBox
            (LgT
              "MFATTCART: M-Files field expression"
              "MFATTCART: Expression de champs M-Files"
              nil
            )
            (LgT
              (strcat "Please, select the desired expression you want to apply on the attribute " tag " :")
              (strcat "Veuillez sélectionner l'expression de champ que vous souhaitez appliquer à l'attribut " tag " :")
              nil
            )
            (mapcar 'car param)
            nil
            1
            nil
          )
        )
        (MFA-AutoAttributeUpdate (vl-position str (mapcar 'car param)))
      )
    )
  )
  (princ)
)