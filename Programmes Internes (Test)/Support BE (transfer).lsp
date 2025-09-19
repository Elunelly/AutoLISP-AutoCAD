  ; Permet de récupérer les coordonnées et données associées d'un point situé à une distance spécifiée d'un élément linéaire :
;--- La fonction (get-AlignPoint-AtDist) possède 3 arguments
;--- curve-obj correspond au nom d'entité de l'objet servant de référence (peut être le nom d'entité ou le VLA-Object)
;--- dist correspond à l'emplacement du point situé sur la courbe pour une distance donnée depuis le point de départ
;--- e correspond au décalage de la courbe de référence. Une valeur positive placera le point calculé au-dessus de la courbe de référence et une valeur négative
; placera le point calculé au-dessous de la courbe de référence (dans le sens de lecture de la courbe)

;---Renvoie une liste de paire pointée de la forme
;  ((-1 . <Nom d'entité>) (10 . PointOnCurve) (11 . PointOutsideCurve) (50 . Angle-Rad_Tangente) (1041 . DistanceOnCurve))
; si l'objet n'est pas un objet linéaire, retourne nil
(defun get-AlignPoint-AtDist (curve-obj dist e / lg u a b pt Ang Align)

  (cond
    ((= (type curve-obj) 'VLA-OBJECT) (setq curve-obj (vlax-vla-object->ename curve-obj)))
    ((= (type curve-obj) 'ENAME) (setq curve-obj curve-obj))
    (t ((exit) (princ)))
  )
  (if (wcmatch (cdr (assoc 0 (entget curve-obj))) "ARC,CIRCLE,ELLIPSE,*LINE")
    (progn
      (cond
        ((> dist (setq lg (vlax-curve-getdistatparam curve-obj (vlax-curve-getendparam curve-obj))))
          (setq dist lg)
        )
        ((< dist 0)
          (setq dist 0.0)
        )
      )
      (setq u (vlax-curve-getfirstderiv curve-obj (vlax-curve-getparamatdist curve-obj dist))
            a (car u)
            b (cadr u)
            pt (vlax-curve-getpointatdist curve-obj dist)
            Align (polar pt (setq Ang (angle '(0.0 0.0 0.0) (list (- b) a 0.0))) e)
      )
      (list (cons -1 curve-obj) (cons 10 pt) (cons 11 Align) (cons 50 (cond ((not (minusp e)) (- Ang (/ pi 2.0))) ((minusp e) (+ Ang (/ pi 2.0))))) (cons 1041 dist))
    )
  )

)

  ; Permet de dessiner une bulle de référencement autour d'un jeu de sélection (cf. LM:ssboundingbox) pointant un point précis :
; Création le 02/06/2020 v1.0.0
(defun SetBubble (jsel / BdB_Pt Center Ray Bulge Ang Dist Int_1 Int_2 pt)

  (if (and
    (setq BdB_Pt (LM:ssboundingbox jsel))
    (setq Center (trans (polar (car BdB_Pt) (angle (car BdB_Pt) (cadr BdB_Pt)) (setq Ray (/ (distance (car BdB_Pt) (cadr BdB_Pt)) 2.0))) 0 1))
    (setq pt (getpoint Center "\nVeuillez spécifier le point de référencement pour cette sélection : "))
    (setq Dist (distance Center pt))
    (setq Ang (angle Center pt))
    (setq Int_1 (polar Center (+ Ang (angtof "20.0" 1)) (* 1.1 Ray)))
    (setq Int_2 (polar Center (- Ang (angtof "20.0" 1)) (* 1.1 Ray)))
    (setq Bulge (/ (sin (/ (angtof "320.0" 1) 4.0)) (cos (/ (angtof "320.0" 1) 4.0))))
    (setq Bubble   (entmakex
               (list
            '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            '(100 . "AcDbPolyline")
            '(70 . 1)
            '(90 . 3)
            (cons 10 (trans Int_1 1 0))
            (cons 42 Bulge)
            (cons 10 (trans Int_2 1 0))
            (cons 10 (trans Pt 1 0))
            (cons 10 (trans Int_1 1 0))
          )
        )
    )
      )
    (prompt "Succès")
    (prompt "Echec...")
  )

)


;; ============================================================= DEFINITION DES ROUTINES LISP ============================================================= ;;



(defun typ (i)
  (cond
    ( (or (/= 'INT (type i)) (null i)) nil)
    ( (< 47 i 58) 0)
    ( (< 64 i 91) 1)
    ( (< 96 i 123) 2)
    (3)
  )
)



  ; Filtrage des calques dans les fenêtres de présentation (exceptées les Vues aériennes si elles n'ont pas bougées d'après le toolkit) d'après un fichier EXCEL pour harmonisation des rendus des plans :

(defun c:FLTCART (/ Att_List jsel name VP_Ind List_Proj Projet Type_Proj List_Ori Ori Type_Ori Format Presentation VP_Sel VP_Nb VP_Name UBS_Flt UBS_Clq List_Flt i Excel_List)

  (setq jsel (ssget "_X" (list '(0 . "INSERT") (cons 410 (getvar "CTAB")))))
  (setq i 0)
  (while (< i (sslength jsel))
    (setq name (ssname jsel i))
    (if (wcmatch (strcase (getpropertyvalue name "BlockTableRecord/Name")) (strcase "Cartouche*"))
      (setq i (sslength jsel))
      (setq i (1+ i))
    )
  )
  (setq Att_List (Get-att-list name))
  (setq VP_Ind (vl-string-right-trim " -" (cdr (assoc "N°_DESSIN" Att_List))))
  (setq List_Proj (list "*Toiture*" "*CS*" "*Ombrière*" "*Serre*" "*Syn.*"))
  (foreach Projet List_Proj
    (if (wcmatch (strcase (getpropertyvalue name "BlockTableRecord/Name")) (strcase Projet))
      (setq Type_Proj (vl-string-trim "*" Projet))
    )
  )
  (setq List_Ori (list "*Paysage*" "*Portrait*"))
  (foreach Ori List_Ori
    (if (wcmatch (strcase (getpropertyvalue name "BlockTableRecord/Name")) (strcase Ori))
      (setq Type_Ori (vl-string-trim "*" Ori))
    )
  )
  (setq Format (cdr (assoc "FORMAT" Att_List)))
  (setq Presentation (list Type_Ori Format))
  (setq UBS_Flt (strcat (DXF_List (ExcelReader_List "Toolkit - Filtres.xlsm" (strcat Type_Proj " - Filtres") "Tab_H" (anytos VP_Ind) "B" 5 "D" 5) "*," "right" t t) "*"))
  (setq VP_Sel (ssget "_X" (list '(0 . "VIEWPORT") '(8 . "UBS-900-Fenêtre de présentation") (cons 410 (getvar "CTAB")))))
  (if VP_Sel
    (progn
      (setq VP_Sel (DeSel_VP VP_Sel Presentation))
      (setq VP_Nb (sslength VP_Sel))
      (setq i 0)
      (setq UBS_Clq (DXF_List (flt_tbl "LAYER" "UBS*") "," "right" t t))
      (while (< i VP_Nb)
        (setq VP_Name (ssname VP_Sel i))
        (command "_-VPORTS" "A" VP_Name "")
        (command "_MSPACE")
        (command "_VPLAYER" "G" UBS_Clq "C" "L" UBS_Flt (if (not (or (= UBS_Flt "") (= UBS_Flt nil))) "C") "")
        (command "_PSPACE")
        (setq i (1+ i))
      )
    )
    (setq VP_Nb 0)
  )
  (setq List_Flt
    (DXF_List (ExcelReader_List "Toolkit - Filtres.xlsm" (strcat Type_Proj " - Filtres") "Tab_H" (anytos VP_Ind) "B" 5 "D" 5) "\n - " "left" t nil)
  )
  (if VP_Sel
    (alert (strcat "\nVoici la liste des calques UBS ayant été conservés au cours de la commande :\n" List_Flt))
    (alert "Le programme a échoué lors de la sélection des fenêtres de présentation ...")
  )

)



  ; Permet de modifier les attributs "Modification_*" et "Auteur_*" en les incrémentant de 1 indice :

(defun C:MODCART (/ vl-all-position nth-remove laylist jsel i ind name Mod Mod_Ind Auteur att-list lst l mod-list aut-list n)

  (defun vl-all-position (x lst / i p)

    (setq i 0)
    (while (< i (length lst))
      (if (= x (nth i lst))
        (setq p (cons i p))
      )
      (setq i (1+ i))
    )
    (reverse p)

  )

  (defun nth-remove (lst tag / i)

    (setq i 0)
    (vl-remove-if '(lambda (x) (member (setq i (1+ i)) tag)) lst)

  )

  (if (setq laylist (ListBox "Commande MODCART : Sélection des présentations"
           "Veuillez sélectionner les présentations à modifier :"
           (DXF_List (vl-remove "TOOLKIT" (layoutlist)) nil nil t nil)
           (getvar "CTAB")
           2
        )
      )
    (progn
      (setq jsel (Select-filter "BLC" "Cartouche*" "_X" (list (cons 410 (DXF_List laylist "," "right" t t))))
            i 0
      )
      (sssetfirst nil nil)
      (initget "Reset +1 -1")
      (if (null (setq Mod (getkword "Que souhaitez-vous faire [Reset/+1/-1] <+1> ? ")))
        (setq Mod "+1")
      )
    )
  )
  (cond
    ((and
       (= Mod "Reset")
      jsel
      (> (sslength jsel) 0)
     )
      (while (< i (sslength jsel))
        (setq name (ssname jsel i)
              att-list (vl-remove-if-not '(lambda (x) (and (wcmatch (strcase (car x)) (strcase "MODIFICATIONS_IND_[B-F],AUTEUR_[B-F]")) (/= (cdr x) ""))) (get-att-list name))
              lst (cons (cdr (assoc 410 (entget name))) lst)
              i (1+ i)
        )
        (if att-list
          (set-att-list name (append (list '("MODIFICATIONS_IND_A" . "CREATION DU PLAN")) (mapcar '(lambda (x) (cons (car x) "")) att-list)))
          (prompt (strcat "\nLe cartouche \"" (cdr (assoc 410 (entget name))) "\" est déjà à l'indice A."))
        )
      )
      (set-date laylist nil)
      (prompt (strcat "\n"
          (itoa (sslength jsel))
          " cartouches présents sur les présentations \""
          (DXF_List lst "\", \"" "right" t t)
          "\" ont été redéfini à l'indice A : \"CREATION DU PLAN\"."
        )
      )
    )
    ((and
      (= Mod "+1")
      jsel
      (> (sslength jsel) 0)
      (setq Mod (strcase (getstring t (strcat "\nVeuillez spécifier la modification à afficher dans " (itoa (sslength jsel)) " cartouche(s) : "))))
     )
      (if (= "" (setq Auteur (getstring (strcat "\nVeuillez spécifier l'auteur pour cette modification <" (vlax-get-property (vlax-get-property (vla-get-activedocument (vlax-get-acad-object)) 'SummaryInfo) 'Author) "> : "))))
        (setq Auteur (vlax-get-property (vlax-get-property (vla-get-activedocument (vlax-get-acad-object)) 'SummaryInfo) 'Author))
      )
      (set-date laylist nil)
      (while (< i (sslength jsel))
        (setq name (ssname jsel i)
              att-list (get-att-list name)
              Mod_Ind (caar (vl-remove-if-not '(lambda (x) (and (wcmatch (strcase (car x)) (strcase "MODIFICATIONS_IND_*")) (= (cdr x) ""))) att-list))
              lst (cons (cons (cdr (assoc 410 (entget name))) (substr Mod_Ind (strlen Mod_Ind) 1)) lst)
              i (1+ i)
        )
        (set-att-list name (list (cons Mod_Ind Mod) (cons (strcat "AUTEUR_" (substr Mod_Ind (strlen Mod_Ind) 1)) Auteur)))
      )
      (prompt (strcat "\n"
          (itoa (sslength jsel))
          " cartouches présents sur les présentations \""
          (DXF_List (mapcar '(lambda (x) (strcat (car x) " (" (cdr x) ")")) lst) "\", \"" "right" t t)
          "\" ont été modifiés."
        )
      )
    )
    ((and
      (= Mod "-1")
      jsel
      (> (sslength jsel) 0)
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i)))
              att-list (cons (cons name (sort-cons (vl-remove-if-not '(lambda (x) (wcmatch (strcase (car x)) (strcase "MODIFICATIONS_IND_[B-F],AUTEUR_[B-F]"))) (get-att-list name)))) att-list)
        )
      )
      (setq Mod (ListBox "Commande MODCART : Sélection des modifications à retirer"
             (strcat "Veuillez sélectionner les modifications à retirer dans " (itoa (sslength jsel)) " cartouche(s) :")
             (DXF_List (mapcar 'cdr (vl-remove-if '(lambda (x) (or (wcmatch (strcase (car x)) "AUTEUR_*") (wcmatch (strcase (cdr x)) ""))) (apply 'append (mapcar 'cdr att-list)))) nil nil t nil)
             0
             2
          )
      )
     )
      (set-date laylist nil)
      (foreach lst att-list
        (setq name (car lst)
              lst (cdr lst)
              mod-list (mapcar 'cdr (vl-remove-if '(lambda (x) (wcmatch (strcase (car x)) (strcase "AUTEUR_*"))) lst))
              aut-list (mapcar 'cdr (vl-remove-if '(lambda (x) (wcmatch (strcase (car x)) (strcase "MODIFICATIONS_IND_*"))) lst))
              Mod_Ind (apply 'append (mapcar '(lambda (x) (vl-all-position x mod-list)) Mod))
              mod-list (append (nth-remove mod-list (mapcar '1+ Mod_Ind)) (progn (setq l nil) (repeat (length Mod_Ind) (setq l (cons "" l)))))
              aut-list (append (nth-remove aut-list (mapcar '1+ Mod_Ind)) (progn (setq l nil) (repeat (length Mod_Ind) (setq l (cons "" l)))))
              Mod_Ind "A"
              mod-list (mapcar '(lambda (x) (cons (strcat "MODIFICATIONS_IND_" (setq Mod_Ind (Sup-Incr Mod_Ind 1))) (strcase x))) mod-list)
              Mod_Ind "A"
              aut-list (mapcar '(lambda (x) (cons (strcat "AUTEUR_" (setq Mod_Ind (Sup-Incr Mod_Ind 1))) (strcase x))) aut-list)
              lst (append aut-list mod-list)
        )
        (set-att-list name lst)
        (setq att-list (get-att-list name)
              Mod_Ind (substr (caar (vl-remove-if-not '(lambda (x) (and (wcmatch (strcase (car x)) (strcase "MODIFICATIONS_IND_*")) (= (cdr x) ""))) att-list)) (strlen "MODIFICATIONS_IND_*") 1)
              laylist (subst (strcat (cdr (assoc 410 (entget name))) " (" (Sup-Incr Mod_Ind -1) ")") (cdr (assoc 410 (entget name))) laylist)
        )
      )
      (prompt (strcat "\n"
          (itoa (sslength jsel))
          " cartouches présents sur les présentations \""
          (DXF_List laylist "\", \"" "right" t t)
          "\" ont été modifiés."
        )
      )
    )
    ((or (null laylist)
         (null jsel)
         (= (sslength jsel) 0)
     )
      (prompt "\nErreur: Commande MODCART annulée...")
    )
    (prompt (strcat "\nAucune modification apportée pour le(s) "
        (itoa (sslength jsel))
        " cartouche(s) présents sur la/les présentation(s) \""
        (DXF_List laylist "\", \"" "right" t t)
        "\"..."
      )
    )
  )
  (princ)

)


  ; Permet de mettre à jour le gestionnaire de calques du dessin courant en fonction d'une liste de calques jugés obsolètes (liste exhaustive) sur le fichier EXCEL "Toolkit - Filtres.xlsm" et de la nouvelle liste de calques :

(defun c:MAJCALQUE (/ *error* Type_Projet Old_List New_List New_Color_List New_LineType_List Old_name New_name New_color New_LineType Obj_Sel i Nb_Obj Nb_Rename Nb_Old Replace_Obj Layout_List layout Init_layout Calque_List Color_List LineType_List Excel_List)

    (setq OldEcho (getvar "CMDECHO"))
  (setq Init_layout (getvar "CTAB"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)

    (if (/= msg "Fonction annulée")
      (princ (strcat "\nErreur : " msg))
    )
    (setvar "CMDECHO" OldEcho)
    (setvar "CTAB" Init_Layout)

  )

  (initget 1 "Toiture CS Ombrière Serre Synoptique")
  (setq Type_Projet (getkword "\nVeuillez préciser le type de projet : [Toiture/CS/Ombrière/Serre/Synoptique]  "))
  (setq Old_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Calques - " "Anciens") "U" 2 "U" 3))
  (setq New_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Calques - " "Remplacement") "U" 2 "U" 3))
  (setq New_Color_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Couleur - " "Remplacement") "U" 2 "U" 3))
  (setq New_Linetype_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Type Ligne - " "Remplacement") "U" 2 "U" 3))
  (if (open (findfile "UBS 2019 - acadiso.lin") "R")
    (command "_-LINETYPE" "CH" "*" "UBS 2019 - acadiso.lin" (while (= (getvar "CMDACTIVE") 1) (command "")))
    (prompt "\nLe fichier \"UBS 2019 - acadiso.lin\" n'a pas été trouvé.\nRisque d'erreur plus important !!!")
  )
  (Verif_Calque "UBS-900-Calques obsolètes" 7 "Continuous")
  (setq i 0)
  (setq Nb_Obj 0)
  (setq Nb_Rename 0)
  (setq Nb_Old 0)
  (setq Replace_Obj 0)
  (setq Layout_List (cons "Model" (layoutlist)))
  (setq Supp_List nil UndeadLayer nil)
  (setq LineType_List (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))))
  (while (< i (length Old_List))
    (setq Old_name (nth i Old_List))
    (setq New_name (nth i New_List))
    (setq New_color (atoi (nth i New_Color_List)))
    (setq New_LineType (nth i New_Linetype_List))
    (if (tblsearch "LAYER" Old_name)
      (progn
        (cond
          ((= New_name Old_name)
            (command "_LAYER" "CO" New_Color New_name "TL" New_LineType New_name "")
          )
          ((tblsearch "LAYER" New_name)
            (progn
              (foreach layout Layout_List
                (if (setq Obj_Sel (ssget "_X" (list (cons 8 Old_name) (cons 410 layout))))
                  (progn
                    (setvar "CTAB" layout)
                    (setq Nb_Obj (+ Nb_Obj (sslength Obj_Sel)))
                    (if (= New_name "UBS-900-Calques obsolètes")
                      (setq Replace_Obj (+ Replace_Obj (sslength Obj_Sel)))
                    )
                    (command "_CHPROP" Obj_Sel "" "CA" New_name "")
                    (command "_LAYER" "CO" New_Color New_name "TL" New_LineType New_name "")
                  )
                )
              )
              (BlockEntity_Layer "*" Old_name New_name "")
              (if (and (= New_name "UBS-900-Calques obsolètes") (> Ent_Obj 0))
                (setq Replace_Obj (+ Replace_Obj Ent_Obj))
              )
              (setq Nb_Old (1+ Nb_Old))
            )
          )
          ((not (tblsearch "LAYER" New_name))
            (progn
              (command "_LAYER" "R" Old_name New_name "CO" New_Color New_name "TL" New_LineType New_name "")
              (setq Nb_Rename (1+ Nb_Rename))
              (setq Nb_Old (1+ Nb_Old))
            )
          )
        )
        (if (/= New_name Old_name) (setq Supp_List (cons Old_name Supp_List)))
      )
    )
    (setq i (1+ i))
  )
  (command "-PURGER" "CA" "*" "N")
  (setq Calque_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Calques - " Type_Projet) "B" 2 "B" 3))
  (setq Color_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Couleur - " Type_Projet) "B" 2 "B" 3))
  (setq LineType_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Type Ligne - " Type_Projet) "B" 2 "B" 3))
  (setq i 0)
  (while (< i (length Calque_List))
    (Verif_Calque (nth i Calque_List) (atoi (nth i Color_List)) (nth i LineType_List))
    (setq i (1+ i))
  )
  (setq New_Calque_List (DXF_List (flt_tbl "LAYER" "UBS-*") "" "" t nil))
  (command "-CALQUE" "T" "A" (DXF_List '("UBS-100-Limites de prox. onduleurs" "UBS-900-Calques obsolètes" "UBS-900-Fenêtre de Présentation" "UBS-900-Non imprimable") "," "right" t t) "")
  (foreach layer New_Calque_List
    (if (not (member layer Calque_List))
      (setq UndeadLayer (cons layer UndeadLayer))
    )
  )
  (setvar "CTAB" Init_layout)
  (setvar "CMDECHO" OldEcho)
  (alert
    (strcat "Le programme MAJCALQUE a trouvé "
             (itoa Nb_Old)
             " calques obsolètes."
             "\nAu cours de cette opération, "
             (itoa Nb_Rename)
             " anciens calques ont été renommé et "
             (itoa Nb_Obj)
             " objets ont changé de calque de manière forcée."
             "\n"
             "\nParmi ces objets, "
             (itoa Replace_Obj)
             " ont été placé sur le calque \"UBS-900-Calques obsolètes\" en raison de la purge des calques sur lesquels ils se trouvaient."
      "\n"
             "\nVoici la liste complète des calques ayant été jugés obsolètes par le programme :"
      "\n"
             (DXF_List Supp_List "\n  - " "left" t nil)
      "\n"
    )
  )
  (alert
    (strcat "Le programme MAJCALQUE a également trouvé "
      (itoa (length UndeadLayer))
      " calques n'appartenant ni à la liste des anciens calques jugés obsolètes, ni à la liste des nouveaux calques."
      "\nCes calques n'ont pas été supprimé au cours de la commande car il peut s'agir de calques de travail en raison de l'échec de leur PURGE."
      "\n"
      "\nVoici la liste des calques supplémentaires conservés :"
      "\n"
      (DXF_List UndeadLayer "\n  - " "left" t nil)
      "\n"
      "\nA savoir que seul les calques démarrant par \"UBS\" sont pris en compte dans cette liste."
    )
  )
  (prompt
    (strcat "\nLe programme MAJCALQUE a trouvé "
        (itoa Nb_Old)
             " calques obsolètes."
             "\nAu cours de cette opération, "
             (itoa Nb_Rename)
             " anciens calques ont été renommé et "
             (itoa Nb_Obj)
             " objets ont changé de calque de manière forcée."
             "\n"
             "\nParmi ces objets, "
             (itoa Replace_Obj)
             " ont été placé sur le calque \"UBS-900-Calques obsolètes\" en raison de la purge des calques sur lesquels ils se trouvaient."
             "\nVoici la liste complète des calques ayant été jugés obsolètes par le programme :"
             (DXF_List Supp_List "\n  - " "left" t nil)
             "\n"
      "\nLe programme MAJCALQUE a également trouvé "
      (itoa (length UndeadLayer))
      " calques n'appartenant ni à la liste des anciens calques jugés obsolètes, ni à la liste des nouveaux calques."
      "\nCes calques n'ont pas été supprimé au cours de la commande car il peut s'agir de calques de travail en raison de l'échec de leur PURGE."
      "\n"
      "\nVoici la liste des calques supplémentaires conservés :"
      "\n"
      (DXF_List UndeadLayer "\n  - " "left" t nil)
      "\n"
      "\nA savoir que seul les calques démarrant par \"UBS\" sont pris en compte dans cette liste."
    )
  )

)

  ; Version custom de la commande AIRE native d'autoCAD permettant de calculer des sommes et des soustractions d'aires avec une sélection libre et la possibilité d'annuler toute sélection faite :
(defun C:SIMPLAREA (/ *error* msg Echo HPColor Clayer Color AccObj i s m Nb_Obj Undo_Obj Area_Total Area_List All Hatch OpHatch All_List Op_List Hatch_List Tmp_Hatch Option Method jsel name Area)

  (setq i 0
        s 0
        m (list 0)
        Nb_Obj 0
        Area_Total 0
        Method t
        Area_List (list 0)
        Hatch_List (list (ssadd))
        Hatch (ssadd)
        OpHatch (ssadd)
        All (ssadd)
        Undo_Obj (list 0)
        Echo (getvar "CMDECHO")
        HPColor (getvar "HPCOLOR")
        Clayer (getvar "CLAYER")
        layer-lock (vla-get-lock (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) Clayer))
        Color (getvar "CECOLOR")
        AccObj (getvar "OSMODE")
  )
  (setvar "OSMODE" 0)
  (vla-put-lock (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) Clayer) :vlax-false)

  (defun *error* (msg)

    (setvar "CMDECHO" 0)
    (vla-put-lock (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) Clayer) :vlax-false)
    (if (and Hatch (> (sslength Hatch) 0))
      (command-s "_ERASE" Hatch "")
    )
    (if (= layer-lock :vlax-true)
      (vla-put-lock (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) Clayer) :vlax-true)
    )
    (setvar "CMDECHO" Echo)
    (setvar "HPCOLOR" HPColor)
    (setvar "CLAYER" Clayer)
    (setvar "CECOLOR" Color)
    (setvar "OSMODE" AccObj)
    msg

  )

  (prompt "\nCe programme est actuellement non fonctionnel avec la sélection de hachures en raison de son mode de fonctionnement.")
  (while (/= Option "Quitter")
    (initget "Ajouter Soustraire annUler Quitter")
    (if (null (setq Option (getkword "\nVeuillez choisir la méthode de calcul de l'aire [Ajouter/Soustraire/annUler/Quitter] <Quitter> : ")))
      (setq Option "Quitter")
    )
    (cond
      ((= Option "Ajouter")
        (setq Method t
              OpHatch (ssadd)
              m (append (list s) m)
        )
        (while (/= Method "Quitter")
          (setvar "OSMODE" 0)
          (initget 128 "Objets annUler Quitter")
          (if (null (setq Method (getpoint "\nAddition -> Veuillez spécifier un point interne ou [Objets/annUler/Quitter] <Quitter> :  ")))
            (setq Method "Quitter")
          )
          (cond
            ((= Method "Objets")
              (setvar "CMDECHO" 0)
              (command "_UNDO" "M")
              (setvar "CMDECHO" Echo)
              (setvar "OSMODE" 0)
              (setq jsel (ssget)
                    i 0
                    s (1+ s)
              )
              (setvar "HPCOLOR" "3")
              (repeat (sslength jsel)
                (setq name (ssname jsel i))
                (if (not (ssmemb name All))
                  (if (and (null (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list name "AREA")))) (> (getpropertyvalue name "AREA") 0) (/= (cdr (assoc 0 (entget name))) "HATCH"))
                    (progn
                      (setq Area (getpropertyvalue name "AREA")
                            Area_Total (+ Area_Total Area)
                      )
                      (setvar "CMDECHO" 0)
                      (command "_-HATCH" "P" "S" "T" "A" "CA" Clayer "TRAN" 80 "S" name "" "")
                      (setvar "CMDECHO" Echo)
                      (and
                        (ssadd (entlast) Hatch)
                        (ssadd (entlast) OpHatch)
                        (ssadd (entlast) All)
                        (ssadd name All)
                      )
                      (setq Nb_Obj (1+ Nb_Obj)
                            Op_List (append (list name (entlast)) Op_List)
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
            )
            ((listp Method)
              (setvar "CMDECHO" 0)
              (command "_UNDO" "M")
              (setvar "HPCOLOR" "3")
              (command "_-HATCH" "P" "S" "T" "A" "CA" Clayer "TRAN" 80 Method "")
              (setvar "CMDECHO" Echo)
              (and
                (ssadd (entlast) Hatch)
                (ssadd (entlast) OpHatch)
                (ssadd (entlast) All)
              )
              (setq s (1+ s)
                    Nb_Obj (1+ Nb_Obj)
                    Op_List (append (list (entlast)) Op_List)
              )
              (if (and (null (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (entlast) "AREA")))) (> (getpropertyvalue (entlast) "AREA") 0))
                (setq Area (getpropertyvalue (entlast) "AREA")
                      Area_Total (+ Area_Total Area)
                )
              )
            )
            ((= Method "annUler")
              (if (and (> s (car m)) (>= s 1))
                (progn
                  (setvar "CMDECHO" 0)
                  (command "_UNDO" "R")
                  (setvar "CMDECHO" Echo)
                  (mapcar '(lambda (x) (ssdel x All)) (car All_List))
                  (setq Area_List (if (null (cdr Area_List)) (list 0) (cdr Area_List))
                        Hatch_List (if (null (cdr Hatch_List)) (list (ssadd)) (cdr Hatch_List))
                        All_List (cdr All_List)
                        Undo_Obj (if (null (cdr Undo_Obj)) (list 0) (cdr Undo_Obj))
                        Area_Total (car Area_List)
                        Nb_Obj (car Undo_Obj)
                        s (1- s)
                  )
                )
                (prompt "\nImpossible d'annuler une nouvelle fois car vous êtes revenu au point de départ.\nVeuillez de nouveau sélectionner des objets ou choisir un point interne, merci.\n")
              )          
            )
          )
          (if (or (= Method "Objets") (listp Method))
            (setq Area_List (append (list Area_Total) Area_List)
                  Undo_Obj (append (list Nb_Obj) Undo_Obj)
                  Hatch_List (append (list OpHatch) Hatch_List)
                  All_List (cons Op_List All_List)
                  Op_List nil
            )
          )
          (prompt (strcat "\nRésultat de la méthode \"Addition\", sélection n°"
              (itoa s)
              " : "
              "\nAire précédente = "
              (rtos (if (> (length Area_List) 1) (nth 1 Area_List) 0) 2 2)
              " m² pour un total de "
              (itoa (if (> (length Undo_Obj) 1) (nth 1 Undo_Obj) 0))
              " objet(s)."
              "\nAire actuelle = "
              (rtos (car Area_List) 2 2)
              " m² pour un total de "
              (itoa (car Undo_Obj))
              " objet(s)."
              "\n"
            )
          )
        )
      )
      ((= Option "Soustraire")
        (setq Method t
              OpHatch (ssadd)
              m (append (list s) m)
        )
        (while (/= Method "Quitter")
          (setvar "OSMODE" 0)
          (initget 128 "Objets annUler Quitter")
          (if (null (setq Method (getpoint "\nSoustraction -> Veuillez spécifier un point interne ou [Objets/annUler/Quitter] <Quitter> :  ")))
            (setq Method "Quitter")
          )
          (cond
            ((= Method "Objets")
              (setvar "CMDECHO" 0)
              (command "_UNDO" "M")
              (setvar "CMDECHO" Echo)
              (setvar "OSMODE" 0)
              (setq jsel (ssget)
                    i 0
                    s (1+ s)
              )
              (setvar "HPCOLOR" "10")
              (repeat (sslength jsel)
                (setq name (ssname jsel i))
                (if (not (ssmemb name All))
                  (if (and (null (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list name "AREA")))) (> (getpropertyvalue name "AREA") 0) (/= (cdr (assoc 0 (entget name))) "HATCH"))
                    (progn
                      (setq Area (getpropertyvalue name "AREA")
                            Area_Total (- Area_Total Area)
                      )
                      (setvar "CMDECHO" 0)
                      (command "_-HATCH" "P" "S" "T" "A" "CA" Clayer "TRAN" 80 "S" name "" "")
                      (setvar "CMDECHO" Echo)
                      (and
                        (ssadd (entlast) Hatch)
                        (ssadd (entlast) OpHatch)
                        (ssadd (entlast) All)
                        (ssadd name All)
                      )
                      (setq Nb_Obj (1+ Nb_Obj)
                            Op_List (append (list name (entlast)) Op_List)
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
            )
            ((listp Method)
              (setvar "CMDECHO" 0)
              (command "_UNDO" "M")
              (setvar "HPCOLOR" "10")
              (command "_-HATCH" "P" "S" "T" "A" "CA" Clayer "TRAN" 80 Method "")
              (setvar "CMDECHO" Echo)
              (and
                (ssadd (entlast) Hatch)
                (ssadd (entlast) OpHatch)
                (ssadd (entlast) All)
              )
              (setq s (1+ s)
                    Nb_Obj (1+ Nb_Obj)
                    Op_List (append (list (entlast)) Op_List)
              )
              (if (and (null (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (entlast) "AREA")))) (> (getpropertyvalue (entlast) "AREA") 0))
                (setq Area (getpropertyvalue (entlast) "AREA")
                      Area_Total (- Area_Total Area)
                )
              )
            )
            ((= Method "annUler")
              (if (and (> s (car m)) (>= s 1))
                (progn
                  (setvar "CMDECHO" 0)
                  (command "_UNDO" "R")
                  (setvar "CMDECHO" Echo)
                  (mapcar '(lambda (x) (ssdel x All)) (car All_List))
                  (setq Area_List (if (null (cdr Area_List)) (list 0) (cdr Area_List))
                        Hatch_List (if (null (cdr Hatch_List)) (list (ssadd)) (cdr Hatch_List))
                        All_List (cdr All_List)
                        Undo_Obj (if (null (cdr Undo_Obj)) (list 0) (cdr Undo_Obj))
                        Area_Total (car Area_List)
                        Nb_Obj (car Undo_Obj)
                        s (1- s)
                  )
                )
                (prompt "\nImpossible d'annuler une nouvelle fois car vous êtes revenu au point de départ.\nVeuillez de nouveau sélectionner des objets ou choisir un point interne, merci.\n")
              )          
            )
          )
          (if (or (= Method "Objets") (listp Method))
            (setq Area_List (append (list Area_Total) Area_List)
                  Undo_Obj (append (list Nb_Obj) Undo_Obj)
                  Hatch_List (append (list OpHatch) Hatch_List)
                  All_List (cons Op_List All_List)
                  Op_List nil
            )
          )
          (prompt (strcat "\nRésultat de la méthode \"Soustraction\", sélection n°"
              (itoa s)
              " : "
              "\nAire précédente = "
              (rtos (if (> (length Area_List) 1) (nth 1 Area_List) 0) 2 2)
              " m² pour un total de "
              (itoa (if (> (length Undo_Obj) 1) (nth 1 Undo_Obj) 0))
              " objet(s)."
              "\nAire actuelle = "
              (rtos (car Area_List) 2 2)
              " m² pour un total de "
              (itoa (car Undo_Obj))
              " objet(s)."
              "\n"
            )
          )
        )
      )
      ((= Option "annUler")
        (while (> s (car m))
          (if (>= s 1)
            (progn
              (setvar "CMDECHO" 0)
              (command "_ERASE" (car Hatch_List) "")
              (setvar "CMDECHO" Echo)
              (mapcar '(lambda (x) (ssdel x All)) (car All_List))
              (setq Area_List (if (null (cdr Area_List)) (list 0) (cdr Area_List))
                    Hatch_List (if (null (cdr Hatch_List)) (list (ssadd)) (cdr Hatch_List))
                    All_List (cdr All_List)
                    Undo_Obj (if (null (cdr Undo_Obj)) (list 0) (cdr Undo_Obj))
                    Area_Total (car Area_List)
                    Nb_Obj (car Undo_Obj)
                    s (1- s)
              )
            )
            (prompt "\nImpossible d'annuler une nouvelle fois car vous êtes revenu au point de départ.\nVeuillez de nouveau choisir une méthode de calcul, merci.\n")
          )
        )
        (setq m (if (null (cdr m)) (list 0) (cdr m)))
        (prompt (strcat "\nRésultat de la méthode \"Annulation méthode\", sélection n°"
            (itoa s)
            " : "
            "\nAire précédente = "
            (rtos (if (> (length Area_List) 1) (nth 1 Area_List) 0) 2 2)
            " m² pour un total de "
            (itoa (if (> (length Undo_Obj) 1) (nth 1 Undo_Obj) 0))
            " objet(s)."
            "\nAire actuelle = "
            (rtos (car Area_List) 2 2)
            " m² pour un total de "
            (itoa (car Undo_Obj))
            " objet(s)."
            "\n"
          )
        )
      )
    )
  )
  (setvar "CMDECHO" 0)
  (if (and Hatch (> (sslength Hatch) 0))
    (command "_ERASE" Hatch "")
  )
  (if (= layer-lock :vlax-true)
    (vla-put-lock (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) Clayer) :vlax-true)
  )
  (setvar "CMDECHO" Echo)
  (setvar "HPCOLOR" HPColor)
  (setvar "CLAYER" Clayer)
  (setvar "CECOLOR" Color)
  (setvar "OSMODE" AccObj)
  (prompt (strcat "\n"
      "\nAire totale = "
      (rtos Area_Total 2 2)
      " m² pour un ensemble de "
      (itoa Nb_Obj)
      " objet(s) après "
      (itoa s)
      " manipulation(s)."
      "\n"
    )
  )                

)


  ; Permet d'ajouter des sommets à des polylignes appartenant à un jeu de sélection :
(defun C:POLYADDPOINT (/ jsel i name n pt Add-pt)

  (if (not (setq jsel (last (ssgetfirst))))
    (setq jsel (ssget '((0 . "LWPOLYLINE"))))
    (setq jsel (ssget "_I" '((0 . "LWPOLYLINE"))))
  )
  (sssetfirst nil jsel)
  (if jsel
    (while (setq pt (getpoint "\nSélectionner le point précédant le nouveau point à ajouter (ENTER pour terminer) : "))
      (setvar "LASTPOINT" pt)
      (setq i 0
            n 0
            Add-pt (getpoint "\nSpécifier un nouveau point : ")
      )
      (while (< i (sslength jsel))
        (setq name (ssname jsel i)
              i (1+ i)
        )
        (if (Add-Poly2D-Point name (trans pt 1 0) (trans Add-pt 1 0))
          (setq n (1+ n))
        )
      )
      (prompt (strcat "\nLe sommet spécifié appartient désormais à "
          (itoa n)
          " / "
          (itoa (sslength jsel))
          " polyligne(s)."
          "\n"
        )
      )
    )
    (prompt "\nAucun jeu de sélection n'a été trouvé après application du filtre.")
  )
  (sssetfirst nil nil)
  (princ)

)



(defun c:SETCOLOR (/ Color doc LayerTable ClqName Layer)

  (prompt "\nCette commande permet de changer la couleur d'un calque en sélectionnant des objets")
  (setq Color (acad_colordlg 256)
        doc (vla-get-activedocument (vlax-get-acad-object))
        LayerTable (vla-get-layers doc)
  )
  (while (setq ClqName (car (nentsel "\nVeuillez sélectionner un objet pour modifier les propriétés du calque : ")))
    (setq ClqName (cdr (assoc 8 (entget ClqName)))
          Layer (vla-item LayerTable ClqName)
    )
    (if (not (= ClqName "0"))
      (progn
        (vla-put-color Layer Color)
        (command "_REGEN")
        (prompt (strcat "\nLe calque \""
            ClqName
            "\" a désormais la couleur "
            (itoa Color)
            "."
          )
        )
      )
      (prompt "\nLe calque \"0\" ne peut pas changer de couleur.")
    )
  )
  (princ)

)

  ; Permet de compter les objets présents dans les réseaux sélectionnés sans avoir à les exploser :

(defun c:ARRAYCOUNT (/ jsel i n name lst Array ent r c l s ent-list o-list)

  (if (or (setq jsel (ssget "_I" '((0 . "INSERT") (2 . "`*U*"))))
    (setq jsel (ssget '((0 . "INSERT") (2 . "`*U*"))))
      )
    (progn
      (repeat (setq n 0 i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if (wcmatch (getpropertyvalue name "Classname") "AcDbAssociative*Array")
          (progn
            (setq lst (cons (cons (setq n (1+ n)) (setq Array (Array-Def name))) lst)
                  r (cdr (assoc "Rows" Array))
                  c (cdr (assoc "Columns" Array))
                  l (cdr (assoc "Levels" Array))
                  s (cdr (assoc 90 Array))
                  o-list (cons (list (cdr (assoc "TotalObject" Array)) s) o-list)
                  ent-list (mapcar '(lambda (ent)
                    (cond
                      ((and (= (cdr (assoc 0 (entget ent))) "INSERT")
                            (not (wcmatch (getpropertyvalue ent "Classname") "AcDbAssociative*Array"))
                       )
                        (cons (getpropertyvalue ent "BlockTableRecord/Name") (cdr (assoc 8 (entget ent))))
                      )
                      (t
                        (cons (cdr (assoc 0 (entget ent))) (cdr (assoc 8 (entget ent))))
                      )
                    )
                  )
                  (cdr (assoc 330 Array))
                )
            )
            (prompt (strcat "\n||==================================================||"
                "\nRéseau n°"
                (itoa n)
                (cond ((wcmatch (getpropertyvalue name "Classname") "AcDbAssociativeRectangularArray") " (Rectangulaire)")
                      ((wcmatch (getpropertyvalue name "Classname") "AcDbAssociativePolarArray") " (Polaire)")
                      ((wcmatch (getpropertyvalue name "Classname") "AcDbAssociativePathArray") " (Trajectoire)")
                      (t "")
                )
                " :"
                "\n     Nombre d'objets total = "
                (itoa (cdr (assoc "TotalObject" Array))) "u / " (itoa (* r c l)) "u"
                "\n     Nombre de rangées = "
                (itoa r) " u"
                " (Espacement = "
                (rtos (cdr (assoc "RowSpacing" Array)) 2 2)
                " m)"
                "\n     Nombre de colonnes = "
                (itoa c) " u"
                " (Espacement = "
                (rtos (cdr (assoc "ColumnSpacing" Array)) 2 2)
                " m)"
                "\n     Nombre de niveaux = "
                (itoa l) " u"
                " (Espacement = "
                (rtos (cdr (assoc "LevelSpacing" Array)) 2 2)
                " m)"
                "\n     Nombre d'objets source = "
                (itoa s) " u"
                "\n  Composition de la source du réseau :"
                (apply 'strcat (mapcar '(lambda (x) (strcat "\n     "
                              (if (not (= 0 (setq ent (- (length ent-list) (length (setq ent-list (vl-remove (car ent-list) ent-list)))))))
                            (strcat (itoa ent)
                              " \""
                              (car x)
                              "\" sur le calque \""
                              (cdr x)
                              "\"."
                            )
                                ""
                             )
                            )
                      )
                            ent-list
                    )
                )
              )
            )
          )
          (ssdel name jsel)
        )
      )
      (prompt (strcat "\n||==================================================||"
          "\nSynthèse des "
          (itoa n)
          " réseaux sélectionnés :"
          "\n     Nombre d'objets total = "
          (itoa (apply '+ (mapcar 'car o-list))) " u"
          "\n     Nombre d'objets global (avec sources) = "
          (itoa (apply '+ (mapcar '(lambda (x) (apply '* x)) o-list))) " u"
          "\nSe référer au détail de chaque réseau pour la répartition des objets et des objets sources."
        )
      )
    )
  )
  (princ)     

)

(defun c:ARRAYLAYER (/ jsel i name lst ent layer n tt e)

  (if (and
        (or
      (setq jsel (ssget "_I" '((0 . "INSERT") (2 . "`*U*"))))
      (progn
        (prompt "\nVeuillez sélectionner les réseaux à modifier : ")
        (setq jsel (ssget '((0 . "INSERT") (2 . "`*U*"))))
      )
    )
    (setq layer (ListBox "Sélection du calque"
             "Veuillez spécifier le nom du calque de remplacement :"
             (DXF_List (vl-remove-if '(lambda (x) (wcmatch x "*|*")) (flt_tbl "LAYER" "*")) nil nil t nil)
             (getvar "CLAYER")
             1
          )
    )
      )
    (progn
      (setq n 0
            tt 0
            e (sslength jsel)
      )
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if (wcmatch (getpropertyvalue name "Classname") "AcDbAssociative*Array")
          (progn
            (setq lst (cdr (assoc 330 (Array-Def name)))
                  tt (+ (length lst) tt)
            )
            (foreach ent lst
              (if (and
                    (/= layer (cdr (assoc 8 (entget ent))))
                    (entmod (subst (cons 8 layer) (assoc 8 (entget ent)) (entget ent)))
                  )
                (setq n (1+ n))
              )
            )
            (entupd name)
          )
          (ssdel name jsel)
        )
      )
      (prompt (strcat "\nUn total de "
          (itoa (sslength jsel))
          " / "
          (itoa e)
          " objets sont des réseaux."
          "\nLe calque de remplacement choisi est : \""
          layer
          "\". Un total de "
          (itoa n)
          " / "
          (itoa tt)
          " objets ont changé de calque."
        )
      )
    )
    (prompt (strcat (if (null jsel) "\nAucun bloc dynamique n'a été sélectionné...")
        (if (null layer) "\nAucun calque de remplacement n'a été spécifié...")
      )
    )
  )
  (princ)            

)

(defun c:BUBBLESET (/ jsel)

  (if (setq jsel (ssget))
    (SetBubble jsel)
  )
  (princ)

)


;--- Auteur :    Luna
;--- Date de création :  10/12/2021
;--- Version :    v1.0.0
;--- Date :    10/12/2021
(defun c:LCtoFC (/ color# ent-s ent-t layer color62 color420 color430 jsel i)
  (defun color# (color key name)
    (cond
      ( (and
          color
          (assoc key (entget name))
        )
        (entmod (subst color (assoc key (entget name)) (entget name)))
      )
      ( (and
          color
          (not (assoc key (entget name)))
        )
        (entmod (append (entget name) (list color)))
      )
      ( (and
          (not color)
          (assoc key (entget name))
        )
        (entmod (vl-remove (assoc key (entget name)) (entget name)))
      )
    )
  )
  (while
    (and
      (setq ent-s (car (entsel "\nSélectionner l'objet source : ")))
      (if (not (assoc 62 (entget ent-s)))
        (setq
          layer (entget (tblobjname "LAYER" (cdr (assoc 8 (entget ent-s)))))
          color420 (assoc 420 layer)
          color430 (assoc 430 layer)
          color62 (assoc 62 layer)
        )
        (setq
          color420 (assoc 420 (entget ent-s))
          color430 (assoc 430 (entget ent-s))
          color62 (assoc 62 (entget ent-s))
        )
      )
    )
    (if
      (progn
        (princ "\nSélectionner le(s) objet(s) cible(s) : ")
        (setq jsel (ssget))
      )
      (repeat (setq i (sslength jsel))
        (setq ent-t (ssname jsel (setq i (1- i))))
        (color# color62 62 ent-t)
        (color# color420 420 ent-t)
        (color# color430 430 ent-t)
      )
    )
  )
  (princ)
)

; 1.  PSLTSCALECUSTOM
; 2.  FNTAERIENNE
; 3.  LONGCUMUL
; 4.  MODCART
;---- LIVRAISON DE LA NOUVELLE VERSION/UTILISATION DES FICHIERS LISP ----;
5.  POLYDELPOINT
6.  POLYADDPOINT
; 7.  PLINE3Dto2D
8.  ARRAYCOUNT
9.  LCtoFC
10. SIMPLAREA














;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;
;;;◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;;;


(defun c:sb (/ str2lst bn co fl ss i nm en ce)
  (defun str2lst (str sep / pos)
    (if (setq pos (vl-string-search sep str))
      (cons
        (substr str 1 pos)
        (str2lst (substr str (+ (strlen sep) pos 1)) sep)
      )
      (list str)
    )
  )
  
;; DEFINITION OF THE COMMAND PARAMETERS
  (setq
    bn "00_NUM_TUBE" ;; EQUALS TO THE EFFECTIVENAME OF THE BLOCK (YOU CAN USE THE WILDCARD CHARACTERS FOR MORE USABILITY)
    co 4 ;; EQUALS TO THE NEW COLOR TO BE SET. USE AN INTEGER BETWEEN 0 TO 256 FOR ACI COLORS, A LIST OF 3 INTEGERS BETWEEN 0 AND 255 EACH FOR TRUE COLORS, A STRING WITH THE NAME OF COLOR BOOK
    fl nil ;; DETERMINES IF THE SEARCH FOR EFFECTIVENAME IS CASE SENSITIVE OR NOT (nil = CASE SENSITIVE, T = NOT CASE SENSITIVE)
  )
;; END OF PARAMETERS'S DEFINITION
  (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 2 (strcat "`*U*," bn)))))
  (repeat (setq i (sslength ss))
    (setq nm (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
    (if
      (not
        (and
          (vlax-property-available-p nm 'EffectiveName)
          (setq en (vlax-get nm 'EffectiveName))
          (if fl
            (mapcar '(lambda (x) (set (read (vl-princ-to-string x)) (strcase (vl-symbol-value x)))) '(bn en))
            '(bn en)
          )
          (wcmatch en bn)
        )
      )
      (ssdel (vlax-vla-object->ename nm) ss)
    )
  )
  (if (not (zerop (sslength ss)))
    (progn
      (setq ce (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (cond
        ( (listp co) (command "_.CHANGE" ss "" "_Properties" "_Color" "_TrueColor" (setq co (str2lst co ",")) ""))
        ( (= 'STR (type co)) (command "_.CHANGE" ss "" "_Properties" "_Color" "_ColorBook" co ""))
        ( (= 'INT (type co)) (command "_.CHANGE" ss "" "_Properties" "_Color" (setq co (itoa co)) ""))
      )
      (setvar "CMDECHO" ce)
      (princ
        (strcat
          "\n"
          (itoa (sslength ss))
          " blocks has been changed with the name \""
          bn
          "\" to the color "
          co
          "..."
        )
      )
    )
    (princ "\n0 blocks has been changed...")
  )
  (princ)
)






(defun get-lpoly-point&rotation (name pt0 / pt pa an)
  (if
    (and
      name
      pt0
      (= "LWPOLYLINE" (cdr (assoc 0 (entget name))))
      (setq pt (vlax-curve-getClosestPointTo name pt0))
      (setq pa (vlax-curve-getParamAtPoint name pt))
      (setq an (angle '(0.0 0.0 0.0) (vlax-curve-getFirstDeriv name pa)))
    )
    (cons name (list pt an))
  )
)



;;========================= ROUTINES =========================;;

;; gc:FieldCode par GC
;; Retourne la chaîne de caractère d'un attribut, texte ou mtexte
;; avec le(s) code(s) de champ(s)
;;
;; Argument : nom d'entité de l'objet (ENAME)

(defun gc:FieldCode (ent / gc:EnameToObjectID hex2dec foo elst xdict dict field str)
  (defun gc:EnameToObjectId (ename)
    ((lambda (str) (hex2dec (substr (vl-string-right-trim ">" str) (+ 3 (vl-string-search ":" str)))))
    (vl-princ-to-string ename)
    )
  )
  (defun hex2dec (s / r l n)
    (setq  r 0 l (vl-string->list (strcase s)))
    (while (setq n (car l))
      (setq
        l (cdr l)
        r (+ (* r 16) (- n (if (<= n 57) 48 55)))
      )
    )
  )
  (defun foo (field str / pos fldID objID)
    (setq pos 0)
    (if (setq pos (vl-string-search "\\_FldIdx " str pos))
      (while (setq pos (vl-string-search "\\_FldIdx " str pos))
        (setq
          fldId (entget (cdr (assoc 360 field)))
          field (vl-remove (assoc 360 field) field)
          str 
            (strcat
              (substr str 1 pos)
              (if (setq objID (cdr (assoc 331 fldId)))
                (vl-string-subst
                  (strcat "ObjId " (itoa (gc:EnameToObjectId objID)))
                  "ObjIdx"
                  (cdr (assoc 2 fldId))
                )
                (foo fldId (cdr (assoc 2 fldId)))
              )
              (substr str (1+ (vl-string-search ">%" str pos)))
            )
        )
      )
      str
    )
  )
  (setq elst (entget ent))
  (if
    (and
      (member (cdr (assoc 0 elst)) '("ATTRIB" "MTEXT" "TEXT"))
      (setq xdict (cdr (assoc 360 elst)))
      (setq dict (dictsearch xdict "ACAD_FIELD"))
      (setq field (dictsearch (cdr (assoc -1 dict)) "TEXT"))
    )
    (setq str (foo field (cdr (assoc 2 field))))
  )
)

;;============================================================;;

;; gc:EnameToObjectId par GC
;; Retourne l'ObjectID correspondant à un ename
;;
;; Argument : un ename

(defun gc:EnameToObjectId (ename)
  ((lambda (str) (hex2dec (substr (vl-string-right-trim ">" str) (+ 3 (vl-string-search ":" str)))))
   (vl-princ-to-string ename)
  )
)

;;============================================================;;

;; hex2dec par GC
;; conversion hexadécimal -> décimal
;;
;; Argument : un hexadédimal (chaîne)

(defun hex2dec (s / r l n)
  (setq  r 0 l (vl-string->list (strcase s)))
  (while (setq n (car l))
    (setq
      l (cdr l)
      r (+ (* r 16) (- n (if (<= n 57) 48 55)))
    )
  )
)

(defun c:F2SR (/ name )
  (defun get-FieldNames (name / f entlist xdict dict field)
    (defun f (field str pos / fldID)
      (if
        (and
          pos
          (setq pos (vl-string-search "\\_FldIdx " str pos))
          (setq fldID (entget (cdr (assoc 360 field))))
          (setq field (vl-remove (assoc 360 field) field))
          (= "Area" (cdr (assoc 1 (member '(6 . "ObjectPropertyName") fldID))))
        )
        (cons
          (cdr (assoc 331 fldID))
          (f field str (vl-string-search ">%" str pos))
        )
      )
    )
    (if
      (and
        (setq entlist (entget name))
        (member (cdr (assoc 0 entlist)) '("ATTRIB" "MTEXT" "TEXT"))
        (setq xdict (cdr (assoc 360 entlist)))
        (setq dict (dictsearch xdict "ACAD_FIELD"))
        (setq field (dictsearch (cdr (assoc -1 dict)) "TEXT"))
      )
      (f field (cdr (assoc 2 field)) 0)
    )
  )
  (defun IsRectangle (name / entlist pt-list)
    (and
      (setq entlist (entget name))
      (= "LWPOLYLINE" (cdr (assoc 0 entlist)))
      (= 4 (cdr (assoc 90 entlist)))
      (= 1 (logand 1 (cdr (assoc 70 entlist))))
      (setq pt-list (vl-remove-if-not '(lambda (x) (= 10 (car x))) entlist))
      (not
        (member
          nil
          (mapcar
            '(lambda (pt1 pt2)
              (equal (abs (angle pt1 pt2)) (/ pi 2.0) 1E-8)
            )
            pt-list
            (append (cdr pt-list) (list (car pt-list)))
          )
        )
      )
    )
  )
  (defun set-Rec2Area (name)
    (if (IsRectangle name)
      (progn
        
      )
    )
  )
  
  (and
    (setq name (car (entsel "\nSelect an MTEXT with area field :")))
    (setq entl (get-FieldNames name))
    (setq i 0)
    (mapcar 'set-Rec2Area entl)
  )
)

(defun c:PadArchi (/ *error* osm cme atd)
  (setq osm (getvar "OSMODE"))
  (setvar "OSMODE" 512)
  (and
    (setq PtIns (getpoint "\nPoint d'insertion de la porte : "))
    (setq PtDir (getpoint PtIns "\nOrientation et dimension de la porte : "))
  )
)

;; Object Snap for grread: Snap Function  -  Lee Mac
;; Returns: [fun] A function requiring two arguments:
;; p - [lst] UCS Point to be snapped
;; o - [int] Object Snap bit code
;; The returned function returns either the snapped point (displaying an appropriate snap symbol)
;; or the supplied point if the snap failed for the given Object Snap bit code.

(defun LM:grsnap:snapfunction ( )
  (eval
    (list
      'lambda
      '( p o / q )
      (list
        'if
        '(zerop (logand 16384 o))
        (list
          'if
          '(setq q
            (cdar
              (vl-sort
                (vl-remove-if
                  'null
                  (mapcar
                    (function
                      (lambda ( a / b )
                        (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                          (list (distance p b) b (car a))
                        )
                      )
                    )
                    '(
                      (0001 . "_end")
                      (0002 . "_mid")
                      (0004 . "_cen")
                      (0008 . "_nod")
                      (0016 . "_qua")
                      (0032 . "_int")
                      (0064 . "_ins")
                      (0128 . "_per")
                      (0256 . "_tan")
                      (0512 . "_nea")
                      (2048 . "_app")
                      (8192 . "_par")
                    )
                  )
                )
                '(lambda ( a b ) (< (car a) (car b)))
              )
            )
          )
          (list
            'LM:grsnap:displaysnap
            '(car q)
            (list
              'cdr
              (list
                'assoc
                '(cadr q)
                (list
                  'quote
                  (LM:grsnap:snapsymbols (atoi (cond ((getenv "AutoSnapSize")) ("5"))))
                )
              )
            )
            (LM:OLE->ACI
              (if (= 1 (getvar 'cvport))
                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
              )
            )
          )
        )
      )
      '(cond ((car q)) (p))
    )
  )
)

;; Object Snap for grread: Display Snap  -  Lee Mac
;; pnt - [lst] UCS point at which to display the symbol
;; lst - [lst] grvecs vector list
;; col - [int] ACI colour for displayed symbol
;; Returns nil

(defun LM:grsnap:displaysnap ( pnt lst col / scl )
  (setq
    scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
    pnt (trans pnt 1 2)
  )
  (grvecs
    (cons col lst)
    (list
      (list scl 0.0 0.0 (car  pnt))
      (list 0.0 scl 0.0 (cadr pnt))
      (list 0.0 0.0 scl 0.0)
      '(0.0 0.0 0.0 1.0)
    )
  )
)

;; Object Snap for grread: Snap Symbols  -  Lee Mac
;; p - [int] Size of snap symbol in pixels
;; Returns: [lst] List of vector lists describing each Object Snap symbol

(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
  (setq
    -p (- p)
    q (1+  p)
    -q (- q)
    r (+ 2 p)
    -r (- r)
    i (/ pi 6.0)
    a 0.0
  )
  (repeat 12
    (setq
      l (cons (list (* r (cos a)) (* r (sin a))) l)
      a (- a i)
    )
  )
  (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
  (list
    (list 1
      (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
    )
    (list 2
      (list -r -q) (list 0  r) (list 0  r) (list r -q)
      (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
    )
    (cons 4 c)
    (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
    (list 16
      (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
      (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
      (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
    )
    (list 32
      (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
      (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
    )
    (list 64
      '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
      '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
      '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
      '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
    )
    (list 128
      (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
      (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
      (list -p q) (list -p -p) (list -p -p) (list q -p)
      (list -q q) (list -q -q) (list -q -q) (list q -q)
    )
    (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
    (list 512
      (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
      (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
    )
    (list 2048
      (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
      (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
      (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
      (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
      (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
    )
    (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
  )
)

;; Object Snap for grread: Parse Point  -  Lee Mac
;; bpt - [lst] Basepoint for relative point input, e.g. @5,5
;; str - [str] String representing point input
;; Returns: [lst] Point represented by the given string, else nil

(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
  (defun str->lst ( str / pos )
    (if (setq pos (vl-string-position 44 str))
      (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
      (list str)
    )
  )

  (if (wcmatch str "`@*")
    (setq str (substr str 2))
    (setq bpt '(0.0 0.0 0.0))
  )           
  (if
    (and
      (setq lst (mapcar 'distof (str->lst str)))
      (vl-every 'numberp lst)
      (< 1 (length lst) 4)
    )
    (mapcar '+ bpt lst)
  )
)

;; Object Snap for grread: Snap Mode  -  Lee Mac
;; str - [str] Object Snap modifier
;; Returns: [int] Object Snap bit code for the given modifier, else nil

(defun LM:grsnap:snapmode ( str )
  (vl-some
    (function
      (lambda ( x )
        (if (wcmatch (car x) (strcat (strcase str t) "*"))
          (progn
            (princ (cadr x)) (caddr x)
          )
        )
      )
    )
    '(
      ("endpoint"      " of " 00001)
      ("midpoint"      " of " 00002)
      ("center"        " of " 00004)
      ("node"          " of " 00008)
      ("quadrant"      " of " 00016)
      ("intersection"  " of " 00032)
      ("insert"        " of " 00064)
      ("perpendicular" " to " 00128)
      ("tangent"       " to " 00256)
      ("nearest"       " to " 00512)
      ("appint"        " of " 02048)
      ("parallel"      " to " 08192)
      ("none"          ""     16384)
    )
  )
)

;; OLE -> ACI  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->ACI ( c )
  (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)

;; OLE -> RGB  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->RGB ( c )
  (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)

;; RGB -> ACI  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values

(defun LM:RGB->ACI ( r g b / c o )
  (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
    (progn
      (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
      (vlax-release-object o)
      (if (vl-catch-all-error-p c)
        (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
        c
      )
    )
  )
)

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
  (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
  (LM:acapp)
)
(vl-load-com) (princ)








(defun c:IB_YMIN (/ LM:ssBoundingBox bins jsel bn ptn ptx)
  (defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
      (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
      (if
        (and
          (vlax-method-applicable-p obj 'getboundingbox)
          (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
        )
        (setq
          ls1 (mapcar 'min (vlax-safearray->list llp) (cond (ls1) ((vlax-safearray->list llp))))
          ls2 (mapcar 'max (vlax-safearray->list urp) (cond (ls2) ((vlax-safearray->list urp))))
        )
      )
    )
    (if (and ls1 ls2) (list ls1 ls2))
  )
  (defun bins (bn pt)
    (command "_-INSERT" bn pt 1 1 0)
    (while (= 1 (getvar "CMDACTIVE"))
      (command "")
    )
    (princ)
  )
  
  (if (setq jsel (ssget))
    (progn
      (setq
;        bn "NOM_DU_BLOC" ; <-- remplacer par le nom du bloc désiré !!
        bn (getstring T "\nEntrer le nom du bloc : ") ; <-- Entrer le nom du bloc manuellement
        bn (if (tblsearch "BLOCK" bn) bn)
        pts (LM:ssBoundingBox jsel)
        ptn (car pts)
        ptx (cadr pts)
        ptx (subst (cadr ptn) (cadr ptx) ptx)
      )
      (bins (cond (bn) (pause)) ptn)
      (bins (cond (bn) (pause)) ptx 1 1 0)
    )
  )
  (princ)
)