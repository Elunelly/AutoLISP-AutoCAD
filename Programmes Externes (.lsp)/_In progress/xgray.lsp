;; ACI -> RGB - Lee Mac 2011
;; Args: c - ACI (AutoCAD Colour Index) Colour
(defun LM:ACI->RGB (c / cObj rgb)
  (vl-load-com)
  (if
    (and (<= 1 c 255)
         (setq cObj
                (vla-getInterfaceObject
                  (vlax-get-acad-object)
                  (strcat "AutoCAD.AcCmColor." (substr (getvar 'ACADVER) 1 2))
                )
         )
         (not
           (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-put-ColorIndex (list cObj c))
           )
         )
    )
     (setq rgb (list (vla-get-Red cObj) (vla-get-Green cObj) (vla-get-Blue cObj)))
  )
  (if cObj
    (vlax-release-object cObj)
  )
  rgb
)
;; RGB -> ACI - Lee Mac 2011
;; Args: r,g,b - Red,Green,Blue values
(defun LM:RGB->ACI (r g b / cObj aci)
  (vl-load-com)
  (if
    (and
      (setq cObj
             (vla-getInterfaceObject
               (vlax-get-acad-object)
               (strcat "AutoCAD.AcCmColor." (substr (getvar 'ACADVER) 1 2))
             )
      )
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-SetRGB (list cObj r g B))
        )
      )
    )
     (setq aci (vla-get-ColorIndex cObj))
  )
  (if cObj
    (vlax-release-object cObj)
  )
  aci
)
;; str2lst
;; Transforme un chaine avec séparateur en liste de chaines
;;
;; Arguments
;; str : la chaine à transformer en liste
;; sep : le séparateur
;;
;; Exemples
;; (str2lst "a b c" " ") -> ("a" "b" "c")
;; (str2lst "1,2,3" ",") -> ("1" "2" "3")
;; (mapcar 'read (str2lst "1,2,3" ",")) -> (1 2 3)
(defun str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons (substr str 1 pos)
          (str2lst (substr str (+ (strlen sep) pos 1)) sep)
    )
    (list str)
  )
)
;; ListBox (gile)
;; Boite de dialogue permettant un ou plusieurs choix dans une liste
;;
;; Arguments
;; title : le titre de la boite de dialogue (chaîne)
;; msg ; message (chaîne), "" ou nil pour aucun
;; keylab : une liste d'association du type ((key1 . label1) (key2 . label2) ...)
;; flag : 0 = liste déroulante
;;        1 = liste choix unique
;;        2 = liste choix multipes
;;
;; Retour : la clé de l'option (flag = 0 ou 1) ou la liste des clés des options (flag = 2)
;;
;; Exemple d'utilisation
;; (listbox "Présentation" "Choisir une présentation" (mapcar 'cons (layoutlist) (layoutlist)) 1)
(defun ListBox (title msg keylab flag / tmp file dcl_id choice)
  (setq tmp  (vl-filename-mktemp "tmp.dcl")
        file (open tmp "w")
  )
  (write-line
    (strcat "ListBox:dialog{label=\"" title "\";")
    file
  )
  (if (and msg (/= msg ""))
    (write-line (strcat ":text{label=\"" msg "\";}") file)
  )
  (write-line
    (cond
      ((= 0 flag) "spacer;:popup_list{key=\"lst\";")
      ((= 1 flag) "spacer;:list_box{key=\"lst\";")
      (T "spacer;:list_box{key=\"lst\";multiple_select=true;")
    )
    file
  )
  (write-line "}spacer;ok_cancel;}" file)
  (close file)
  (setq dcl_id (load_dialog tmp))
  (if (not (new_dialog "ListBox" dcl_id))
    (exit)
  )
  (start_list "lst")
  (mapcar 'add_list (mapcar 'cdr keylab))
  (end_list)
  (action_tile
    "accept"
    "(or (= (get_tile \"lst\") \"\")
    (if (= 2 flag) (progn
    (foreach n (str2lst (get_tile \"lst\") \" \")
    (setq choice (cons (nth (atoi n) (mapcar 'car keylab)) choice)))
    (setq choice (reverse choice)))
    (setq choice (nth (atoi (get_tile \"lst\")) (mapcar 'car keylab)))))
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete tmp)
  choice
)
;;; XGRAY - Bryce, basé sur XCOL (gile)
;;; Colorise les XRefs sélectionnées
;;; Choix d'une couleur dans la boîte de dialogue standard (option "Monochrome")
;;; ou conversion en niveaux de gris automatique (option "niveaux de Gris")
;;; option "Restaurer" pour restituer les couleurs d'origine

(defun c:XGray (/ acdoc layers blocks ss name lst action color shade layername vr)
  (vl-load-com)
  (setq acdoc  (vla-get-ActiveDocument (vlax-get-acad-object))
        layers (vla-get-Layers acdoc)
        blocks (vla-get-Blocks acdoc)
  )

  (vla-StartUndoMark acdoc)

  (initget "Monochrome Gris Restaurer")
  (setq action (getkword "\nChoix de l'option [Monochrome/niveaux de Gris/Restaurer] <Gris>:"))
  (if (not action)
    (setq action "Gris")
  )

  (or (getenv "XrefColor") (setenv "XrefColor" "8"))

  (if (eq action "Monochrome")
    (progn
      (setq color (acad_colordlg (atoi (getenv "XrefColor"))))
      (setenv "XrefColor" (itoa color))
    )
  )
  (vlax-for x blocks
    (if (= (vla-get-IsXref x) :vlax-true)
      (setq lst (cons (vla-get-Name x) lst))
    )
  )
  (if (setq lst (ListBox "Xref color" "Choisir les xref à coloriser" (mapcar 'cons lst lst) 2))
    (progn
      (vlax-for l layers
        (if
          (and
            (setq layername (vla-get-Name l))
            (setq pos (vl-string-position (ascii "|") layername))
            (member (substr layername 1 pos) lst)
          )
           (if (eq action "Restaurer")
             (progn
               (setq vr (getvar "VISRETAIN"))
               (setvar "VISRETAIN" 0)
               (vlax-for b (vla-get-Blocks acdoc)
                 (if (= (vla-get-IsXref B) :vlax-true)
                   (progn
                     (vla-unload B)
                     (vla-reload B)
                   )
                 )
               )
               (setvar "VISRETAIN" vr)
             )

             (if (eq action "Monochrome")

               (vla-put-color l color)

               (if (eq action "Gris")
                 (progn
                   (setq color (LM:ACI->RGB (vla-get-color l)))
                   (setq shade (fix (/ (+ (car color) (cadr color) (caddr color)) 3)))
                   (setq shade (+ 34 shade))
                   ;; augmenter pour éclaircir
                   (if (>= shade 255)
                     (setq shade 254)
                   )
                   (setq shade (LM:RGB->ACI shade shade shade))
                   (if (or (= shade 7) (= shade 250))
                     (setq shade 251)
                   )
                   (vla-put-color l shade)
                 )
               )
             )
           )
        )
      )
      (vla-regen acdoc acActiveViewport)
    )
  )
  (vla-EndUndoMark acdoc)

  (princ)
)