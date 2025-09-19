;;;=================================================================
;;;
;;; GEF.LSP V3.21
;;;
;;; Gerer les calques des fenêtres dans les présentations
;;;
;;; Utilisation de la dll VlLayerlisp afin d'accélérer le lisp dans
;;; la gestion des calques dans les fenêtres. Merci à Gile.
;;; Dll donnée sur le message suivant
;;; http://www.acadlabs.com/viewtopic.php?f=19&t=344&p=592&sid=918514f49151c3a980a5d44a9c256d4e
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:gef(/  act0 act1 aff c0 cal cmd cod dcl doc fen filtre_fen filtre_gef filtre_nom
    filtre_ong grp i js lay lst lst_cal lst_ong modif1 modif2 n obj old old_sel
    pre par prt reg res rgl s sel sel_cal sel_fen tbl tot1 tot2 typ val vie
    affichage_bd affichage_calques convertir_sel flag fra gerer_fenetres in
    liste_fenetres msgbox restaurer_calques sauver_calques selection *errgef*)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion des erreurs
  ;;;
  ;;;---------------------------------------------------------------

  (defun *errgef* (msg)
    (or (member (strcase msg) '("FUNCTION CANCELLED" ""QUIT / EXIT ABORT"" "FONCTION ANNULEE" "QUITTER / SORTIR ABANDON"))
      (princ (strcat "\nErreur : " msg))
    )
    (vla-endundomark doc)
    (setq *error* s)
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Message
  ;;;
  ;;;---------------------------------------------------------------

  (defun msgbox (Titre Bouttons Message / Reponse WshShell)
    (vl-load-com)  
    (setq WshShell (vlax-create-object "WScript.Shell"))
    (setq Reponse  (vlax-invoke WshShell 'Popup Message 0 Titre (itoa Bouttons)))
    (vlax-release-object WshShell)
    Reponse
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Autocad en Français ?
  ;;;
  ;;;---------------------------------------------------------------

  (defun fra()
;    (eq (getvar "locale") "FRA")
    (member (strcase (getvar "dctmain")) '("FR" "FRA" "FRC"))
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Griser ou pas les boutons geler/afficher
  ;;;
  ;;;---------------------------------------------------------------

  (defun selection()
    (if (and sel_fen
       sel_cal
       lst_cal
       (not (zerop (logand filtre_gef (+ 1 2 4))))
       (not (zerop (logand filtre_gef (+ 8 16 32))))
  )
      (progn
  (mode_tile "gel" 0)
  (mode_tile "aff" 0)
      )
      (progn
  (mode_tile "gel" 1)
  (mode_tile "aff" 1)
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Convertir la selection filtré en non filtré
  ;;;
  ;;;---------------------------------------------------------------

  (defun convertir_sel(sel / lay obj tmp val)
    (and aff lst_ong sel
   (/= (length aff) (length lst_ong))
   (setq  tot 0
    val (read sel)
   )
      (progn
  (foreach lay tbl
    (foreach obj (cadr lay)
      (if (and val (equal (nth val lst_ong) obj))
        (progn
    (setq sel (substr sel (+ 2 (strlen (itoa val))) (strlen sel))
          val (read sel)
    )
    (if tmp
      (setq tmp (strcat tmp " " (itoa tot)))
      (setq tmp (itoa tot))
    )
        )
      )
      (setq tot (1+ tot))
    )
  )
        (setq sel tmp)
      )
    )
    sel
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Afficher l'état des calques
  ;;;
  ;;;---------------------------------------------------------------

  (defun affichage_calques(sel / js lst n mar old tab tmp tot val)
    (setq sel_fen (convertir_sel sel)
    sel sel_fen
    lst (list)
    tot 0
    lst_cal nil
    )
    (if sel_fen
      (progn
  (mode_tile "res" 0)
  (while (not (eq sel ""))
    (setq val (read sel)
    lst (append lst (nth val aff))
    sel (substr sel (+ 2 (strlen (itoa val))) (strlen sel))
    tot (1+ tot)
    )
  )
  (while lst  
    (setq n   (length lst)
    js  (car lst)
    lst (vl-remove js lst)
    tab (cons (cons js (- n (length lst))) tab)
    )
  )
      )
    )
    (setq n 0)
    (start_list "cal")
    (while (setq val (nth n cal))
      (setq mar nil)
      (if sel_fen
  (if (setq js (assoc val tab))
    (if (eq (cdr js) tot)
      (and (eq (logand filtre_gef 32) 32)
        (if (fra)
    (setq mar "G")
    (setq mar "F")
        )
      )
      (and (eq (logand filtre_gef 16) 16)
        (if (fra)
    (setq mar "M")
    (setq mar "J")
        )
      )
    )
    (and (eq (logand filtre_gef 8) 8)
      (if (fra)
        (setq mar "A")
        (setq mar "T")
      )
    )
  )
  (setq mar "X")
      )
      (and mar
     (wcmatch (strcase val) (strcase filtre_nom))
     (if (eq (logand filtre_gef 64) 64)
       (progn
         (setq lst_cal (cons val lst_cal))
         (add_list (strcat mar "   " val))
       )
       (if (not (wcmatch val "*|*"))
         (progn
     (setq lst_cal (cons val lst_cal))
     (add_list (strcat mar "   " val))
         )
       )
     )
      )
      (setq n (1+ n))
    )
    (end_list)
    (and sel_cal
      (set_tile "cal" sel_cal)
    )
    (setq lst_cal (reverse lst_cal))
    (selection)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Détermine si un nombre est dans une plage
  ;;; ex : (in "45" "44:50,55sfs:66,100,800") --> T
  ;;; ex : (in "52" "44:50,55sfs:66,100,800") --> nil
  ;;;
  ;;;---------------------------------------------------------------

  (defun in(nb str / lst pos txt val val2 valid)
    (and (eq (type nb) 'STR)
      (setq nb (read nb))
    )
    (setq pos 1)
    (while (/= (setq val (substr str pos 1)) "")
      (cond
  ((eq val ":")
    (if txt
      (setq lst (cons "to" (cons (read txt) lst)))
      (setq lst (cons "to" lst))
    )
    (setq txt nil)
  )
  ((member val '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "." "-"))
    (if txt
      (setq txt (strcat txt val))
      (setq txt val)
    )
  )
  (T
    (and txt (setq lst (cons (read txt) lst)))
    (setq txt nil)
  )
      )
      (setq pos (1+ pos))
    )
    (setq lst (reverse (vl-remove '- (if txt (cons (read txt) lst) lst)))
    pos 0
    )
    (if (member nb lst)
      (setq valid T)
      (while (setq val (nth pos lst))
  (and (eq (nth (1+ pos) lst) "to")
       (setq val2 (nth (+ 2 pos) lst))
       (< val nb)
       (> val2 nb)
    (setq valid T
    pos (length lst)
    )
  )
  (setq pos (1+ pos))
      )
    )
    valid
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Afficher la liste des fenêtres
  ;;;
  ;;;---------------------------------------------------------------

  (defun liste_fenetres(/ n obj sau sui str str1 str2 str3 tot)
    (setq lst_ong nil)
    (start_list "fen")
    (foreach obj tbl
      (setq sau nil
      sui nil
      tot 0
      )
      (foreach lay (cadr obj)
  (setq n (vlax-get lay 'center)
        str ""
        str1 (car obj)
        str2 (if sau
         (strcat (if (fra)
             "Fenêtre "
             "View "
           )
           (itoa tot)
           " : "
           (rtos (vla-get-width lay))
           " x "
           (rtos (vla-get-height lay))
         )
         (if (fra)
           "Présentation"
           "Layout"
         )
       )
        str3 (strcat "("
         (rtos (car n))
         " x "
         (rtos (cadr n))
         ")"
       )
        tot (1+ tot)
  )
  (and (eq (logand filtre_gef 1) 1)
    (setq str str1)
  )
  (and (eq (logand filtre_gef 2) 2)
    (if (eq str "")
      (setq str str2)
      (setq str (strcat str " ---> " str2))
    )
  )
  (and sau (eq (logand filtre_gef 4) 4)
    (if (eq str "")
      (setq str str3)
      (setq str (strcat str "   " str3))
    )
  )
  (and (not (eq str ""))
       (wcmatch (strcase str1) (strcase filtre_ong))
       (or (eq filtre_fen "*")
     (in (itoa (1- tot)) filtre_fen)
       )
    (setq lst_ong (cons lay lst_ong))
    (add_list str)
  )
  (setq sau T)
      )
    )
    (end_list)
    (setq lst_ong (reverse lst_ong))
    (selection)
    (and old_sel (set_tile "fen" old_sel))
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Afficher la boite de dialogue
  ;;;
  ;;;---------------------------------------------------------------

  (defun affichage_bd(/ ele)
    (new_dialog "gef" dcl "")
    (set_tile "titre" "GEF V3.21")
    (mode_tile "res" 1)
    (liste_fenetres)
    (if old_sel
      (affichage_calques old_sel)
      (affichage_calques sel_fen)
    )
    (foreach ele '((1 "nom")(2 "tai")(4 "cen")(8 "gaff")(16 "gmel")(32 "ggel")(64 "xref"))
      (and (eq (logand filtre_gef (car ele)) (car ele))
  (set_tile (cadr ele) "1")
      )
    )
    (cond
      (old_sel
  (set_tile "fen" old_sel)
      )
      (sel_fen
  (set_tile "fen" sel_fen)
      )
    )
    (and sel_cal (set_tile "cal" sel_cal))
    (set_tile "calq" filtre_nom)
    (set_tile "ong"  filtre_ong)
    (set_tile "ffen" filtre_fen)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Mise à jour de l'affichage de l'état des calques suivant
  ;;; l'action de l'utilisateur sur les boutons afficher/geler
  ;;;
  ;;;---------------------------------------------------------------

  (defun gerer_fenetres(typ / lst n par sel val)
    (setq sel sel_cal)
    (while (not (eq sel ""))
      (setq val (read sel)
      lst (cons (nth val lst_cal) lst)
      sel (substr sel (+ 2 (strlen (itoa val))) (strlen sel))
      )
      (start_list "cal" 1 val)
      (if typ
  (add_list (strcat (if (fra) "G   " "F   ") (nth val lst_cal)))
  (add_list (strcat (if (fra) "A   " "T   ") (nth val lst_cal)))
      )
      (end_list)
    )
    (setq sel sel_fen)
    (while (not (eq sel ""))
      (setq val (read sel)
      n (1+ val)
      par (nth val aff)
      )
      (foreach ele lst
  (if typ
    (or (member ele par)
      (setq par (cons ele par))
    )
    (and (member ele par)
      (setq par (vl-remove ele par))
    )
  )
      )
      (setq aff (mapcar '(lambda(x) (if (zerop (setq n (1- n))) par x)) aff)
      sel (substr sel (+ 2 (strlen (itoa val))) (strlen sel))
      )
    )
  )

  ;-----------------------------------------------------------------
  ; Ajouter/Enlever un bit
  ;-----------------------------------------------------------------

  (defun flag(nb)
    (if (zerop (logand filtre_gef nb))
      (setq filtre_gef (+ filtre_gef nb))
      (setq filtre_gef (- filtre_gef nb))
    )
  )

  ;-----------------------------------------------------------------
  ; Sauver la liste de calques
  ;-----------------------------------------------------------------

  (defun sauver_calques(/ cal fic fil sel)
    (princ "\nSélectionnez une fenêtre : ")
    (and (setq sel (ssget "_+.:E:S" (list (cons 0 "VIEWPORT"))))
   (setq fic (getfiled "Nom de la liste de calques à sauvegarder" (getvar "dwgprefix") "GEF" 1))
      (progn
  (vla-getxdata (vlax-ename->vla-object (ssname sel 0)) "" 'cod 'typ)
  (setq cod (vlax-safearray->list cod)
        typ (vlax-safearray->list typ)
        grp nil
        n 0
  )
  (while (setq val (nth n cod))
    (and (eq val 1003)
      (setq grp (cons (vlax-variant-value (nth n typ)) grp))
    )
    (setq n (1+ n))
  )
  (setq fil (open fic "w"))
  (mapcar '(lambda(x)(write-line x fil)) grp)
  (write-line "***AFFICHER***" fil)
  (vlax-map-collection (vla-get-layers doc)
           '(lambda(x)
             (or (vl-position (vla-get-name x) grp)
         (write-line (vla-get-name x) fil)
             )
           )
  )
  (close fil)
  (alert (strcat "L'état des calques de la fenêtre sélectionnée ont été sauvegardé dans le fichier :\n" fic))
      )
    )
  )

  ;-----------------------------------------------------------------
  ; Restaurer la liste de calques
  ;-----------------------------------------------------------------

  (defun restaurer_calques(/ n cal fic fil ele gel grp lst sel val vie)
    (and (setq fic (getfiled "Nom de la liste de calques à restaurer" (getvar "dwgprefix") "GEF" 8))
      (progn
  (setq fil (open (findfile fic) "r")
        sel sel_fen
        val (read sel)
        n 0
  )
  (while (setq ele (read-line fil))
    (cond
      ((= ele "***AFFICHER***")
        (setq gel T)
      )
      ((tblsearch "layer" ele)
        (if gel
    (setq grp (cons ele grp))
    (setq lst (cons ele lst))
        )
      )
    )
  )
  (close fil)
  (foreach pre tbl
    (foreach vie (cadr pre)
      (if (= val n)
        (progn
    (if (eval 'gc-vpthaw)
      (progn
        (and grp (gc-vpthaw   (vlax-vla-object->ename vie) grp))
        (and lst (gc-vpfreeze (vlax-vla-object->ename vie) lst))
      )
      (progn
        (or (eq (vla-get-objectid (vla-get-activelayout doc)) (vla-get-objectid (setq lay (vla-item (vla-get-layouts doc) (car pre)))))
          (vla-put-activelayout doc lay)
        )
        (setq cmd (getvar "cmdecho")
        reg (getvar "regenmode")
        )
        (setvar "cmdecho" 0)
        (setvar "regenmode" 0)
        (foreach cal grp
          (vl-cmdf "_.vplayer" "_thaw" cal (vlax-vla-object->ename vie) "" "")
        )
        (foreach cal lst
          (vl-cmdf "_.vplayer" "_freeze" cal (vlax-vla-object->ename vie) "" "")
        )
        (setvar "regenmode" reg)
        (setvar "cmdecho" cmd)
      )
    )
    (setq sel (substr sel (+ 2 (strlen (itoa val))) (strlen sel))
          val (read sel)
    )
        )
      )
      (setq n (1+ n))
    )
  )
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine principale
  ;;;
  ;;;---------------------------------------------------------------

  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
  n 1
  s *error*
  *error* *errgef*
  )
  (and  (not (eval 'gc-vpthaw))
  (setq dcl (findfile "VpLayerLisp.dll"))
    (if (< 18 (atoi (getvar 'acadver)))
      (vl-cmdf "_netload" "vplayerlisp_19.dll")
      (vl-cmdf "_netload" "vplayerlisp.dll")
    )
  )
  (if (fra)
    (setq dcl "gef_fr.dcl")
    (setq dcl "gef_en.dcl")
  )
  (vla-startundomark doc)
  (if (setq dcl (findfile dcl))
    (progn
      (vlax-for lay (vla-get-layouts doc)
  (if (/= (vla-get-name lay) "Model")
    (progn
      (setq lst (cons (list (vla-get-taborder lay) (vla-get-name lay)) lst)
      vie nil
      )
      (vlax-for ent (vla-get-block lay)
        (and
    (eq (vla-get-objectname ent) "AcDbViewport")
    (zerop (logand (cdr (assoc 90 (entget (vlax-vla-object->ename ent)))) 131072))
    (setq vie (cons ent vie))
        )
      )
      (and vie
        (setq fen (cons (list (vla-get-name lay) (reverse vie)) fen))
      )
    )
  )
      )
      (repeat (length lst)
  (and (setq val (cadr (assoc (cadr (assoc n lst)) fen)))
    (setq tbl (cons (list (cadr (assoc n lst))
        val
        )
        tbl
        )
    )
  )
  (setq n (1+ n))
      )
      (vlax-for lay (vla-get-layers doc)
  (setq cal (cons (vla-get-name lay) cal))
      )
      (setq cal (acad_strlsort cal)
      tbl (reverse tbl)
      dcl (load_dialog dcl)
      )
      (if (setq filtre_gef (getenv "Patrick_35_Gef"))
  (setq filtre_gef (atoi filtre_gef))
  (setq filtre_gef 63)
      )
      (setq filtre_nom "*"
      filtre_fen "*"
      filtre_ong "*"
      )
      (affichage_bd)
      (foreach obj tbl
  (setq lst nil)
  (foreach lay (cadr obj)
    (vla-getxdata lay "" 'cod 'typ)
    (setq cod (vlax-safearray->list cod)
    typ (vlax-safearray->list typ)
    grp nil
    n 0
    )
    (while (setq val (nth n cod))
      (and (eq val 1003)
        (setq grp (cons (vlax-variant-value (nth n typ)) grp))
      )
      (setq n (1+ n))
    )
    (if grp
      (setq lst (cons grp lst))
      (setq lst (cons '(nil) lst))
    )
  )
  (if lst
    (setq aff (append aff (reverse lst)))
    (setq aff (append aff '(nil)))
  )
      )(setq dd aff)
      (while (not (member res '(0 1)))
  (action_tile "fen"    "(setq sel_cal nil)(affichage_calques (setq old_sel (get_tile \"fen\")))")
  (action_tile "cal"    "(setq sel_cal (get_tile \"cal\"))(selection)")
  (action_tile "gel"    "(gerer_fenetres T)")
  (action_tile "aff"    "(gerer_fenetres nil)")
  (action_tile "nom"    "(flag 1) (liste_fenetres)")
  (action_tile "tai"    "(flag 2) (liste_fenetres)")
  (action_tile "cen"    "(flag 4) (liste_fenetres)")
  (action_tile "gaff"   "(flag 8) (affichage_calques old_sel)")
  (action_tile "gmel"   "(flag 16)(affichage_calques old_sel)")
  (action_tile "ggel"   "(flag 32)(affichage_calques old_sel)")
  (action_tile "xref"   "(flag 64)(affichage_calques old_sel)")
  (action_tile "calq"   "(setq filtre_nom $value)(setq sel_cal nil)(affichage_calques old_sel)")
  (action_tile "ong"    "(setq filtre_ong $value)(setq sel_fen nil old_sel nil sel_cal nil)(liste_fenetres)(affichage_calques old_sel)")
  (action_tile "ffen"   "(setq filtre_fen $value)(setq sel_fen nil old_sel nil sel_cal nil)(liste_fenetres)(affichage_calques old_sel)")
  (action_tile "sav"    "(done_dialog 3)")
  (action_tile "res"    "(done_dialog 4)")
  (action_tile "sel"    "(done_dialog 2)")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq res (start_dialog))
  (cond
    ((eq res 1)
      (setq old (vla-get-activelayout doc)
      n 0
      tot1 0
      tot2 0
      rgl (getvar "layoutregenctl")
      )
      (setvar "layoutregenctl" 0)
      (foreach pre tbl
        (foreach vie (cadr pre)
    (vla-getxdata vie "" 'cod 'typ)
    (setq cod (vlax-safearray->list cod)
          typ (vlax-safearray->list typ)
          par (nth n aff)
          lst nil
          cal nil
          i 0
          modif2 nil
          act0 nil
          act1 nil
    )
    (while (setq val (nth i cod))
      (and (eq val 1003)
        (setq cal (cons (vlax-variant-value (nth i typ)) cal))
      )
      (setq i (1+ i))
    )
    (foreach ele par
      (and ele
           (not (member ele cal))
        (setq lst (cons (cons ele 0) lst)
        act0 (cons ele act0)
        )
      )
    )
    (foreach ele cal
      (or (member ele par)
        (setq lst (cons (cons ele 1) lst)
        act1 (cons ele act1)
        )
      )
    )
    (and lst
         (setq modif1 T
         modif2 T
         tot2 (1+ tot2)
         c0 (vla-item (vla-get-layers doc) (vla-get-layer vie))
         )
      (if (eq (vla-get-lock c0) :vlax-true)
        (progn
          (if (fra)
      (princ (strcat "\nLe calque " (vla-get-name c0) " est verrouillé."))
      (princ (strcat "\nThe layer " (vla-get-name c0) " as locked."))
          )
          (setq tot2 (1- tot2))
        )
        (progn
          (if (eval 'gc-vpthaw)
      (progn
        (and act0 (gc-vpfreeze (vlax-vla-object->ename vie) act0))
        (and act1 (gc-vpthaw   (vlax-vla-object->ename vie) act1))
      )
      (progn
        (or (eq (vla-get-objectid (vla-get-activelayout doc)) (vla-get-objectid (setq lay (vla-item (vla-get-layouts doc) (car pre)))))
          (vla-put-activelayout doc lay)
        )
        (setq cmd (getvar "cmdecho")
        reg (getvar "regenmode")
        )
        (setvar "cmdecho" 0)
        (setvar "regenmode" 0)
        (foreach cal lst
          (if (eq (cdr cal) 0)
            (setq val "_freeze")
            (setq val "_thaw")
          )
          (vl-cmdf "_.vplayer" val (car cal) "_select" (vlax-vla-object->ename vie) "" "")
        )
        (setvar "regenmode" reg)
        (setvar "cmdecho" cmd)
      )
          )
        )
      )
    )
    (setq n (1+ n))
        )
        (and modif1
    (setq tot1 (1+ tot1)
          modif1 nil)
        )
      )
      (or (eq (vla-get-objectid (vla-get-activelayout doc)) (vla-get-objectid old))
        (vla-put-activelayout doc old)
      )
      (if (fra)
        (princ (strcat "\nTraitement de " (itoa tot1) " présentation(s) et de " (itoa tot2) " fenêtre(s)"))
        (princ (strcat "\nWorks to " (itoa tot1) " layout(s) and " (itoa tot2) " viewport(s)"))
      )
      (setvar "layoutregenctl" rgl)
    )
    ((eq res 2)
      (setq lst nil js nil i 0)
      (if (ssget (list (cons 0 "VIEWPORT")))
        (progn
    (vlax-for lay (setq sel (vla-get-activeselectionset doc))
      (setq lst (cons (vla-get-objectid lay) lst))
    )
    (vla-delete sel)
    (foreach obj lst_ong
      (and (member (vla-get-objectid obj) lst)
        (if js
          (setq js (strcat js " " (itoa i)))
          (setq js (itoa i))
        )
      )
      (setq i (1+ i))
    )
    (and js (setq sel_fen (convertir_sel js) old_sel js sel_cal nil))
        )
      )
      (affichage_bd)
    )
    ((eq res 3)
      (sauver_calques)
    )
    ((eq res 4)
      (restaurer_calques)
    )
  )
      )
      (unload_dialog dcl)
    )
    (if (fra)
      (msgbox "GEF" 16 "Fichier GEF_FR.DCL introuvable")
      (msgbox "GEF" 16 "Don't find file GEF_EN.DCL")
    )
  )
  (setenv "Patrick_35_Gef" (itoa filtre_gef))
  (vla-endundomark doc)
  (setq *error* s)
  (princ)
)

(setq nom_lisp "GEF")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)