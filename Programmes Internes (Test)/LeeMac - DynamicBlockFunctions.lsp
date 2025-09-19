;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)

(defun LM:getdynpropvalue ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    ;(vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (vla-put-value x val)
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
;; Get Dynamic Block Properties  -  Lee Mac
;; Returns an association list of Dynamic Block properties & values.
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [lst] Association list of ((<prop> . <value>) ... )

(defun LM:getdynprops ( blk )
    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
;; Set Dynamic Block Properties  -  Lee Mac
;; Modifies values of Dynamic Block properties using a supplied association list.
;; blk - [vla] VLA Dynamic Block Reference object
;; lst - [lst] Association list of ((<Property> . <Value>) ... )
;; Returns: nil

(defun LM:setdynprops ( blk lst / itm )
    (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
    (foreach x (vlax-invoke blk 'getdynamicblockproperties)
        (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
            (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
        )
    )
)
;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
;; Toggle Dynamic Block Flip State  -  Lee Mac
;; Toggles the Flip parameter if present in a supplied Dynamic Block.
;; blk - [vla] VLA Dynamic Block Reference object
;; Return: [int] New Flip Parameter value

(defun LM:toggleflipstate ( blk )
    (vl-some
       '(lambda ( prp / rtn )
            (if (equal '(0 1) (vlax-get prp 'allowedvalues))
                (progn
                    (vla-put-value prp (vlax-make-variant (setq rtn (- 1 (vlax-get prp 'value))) vlax-vbinteger))
                    rtn
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)
;; Get Dynamic Block Visibility State  -  Lee Mac
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil

(defun LM:getvisibilitystate ( blk / vis )
    (if (setq vis (LM:getvisibilityparametername blk))
        (LM:getdynpropvalue blk vis)
    )
)
;; Set Dynamic Block Visibility State  -  Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil

(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)










;; ==================================================================================================================== ;;
;;            SEPARATION CADASTRALE (JESSY)            ;;
;; ==================================================================================================================== ;;

;; Permet de récupérer la liste des coordonnées de points des segments de longueurs définie pour une polyligne :
;;--- La fonction (get-segment-list) possède 2 arguments
;;--- name  correspond au nom d'entité de la polyligne (Pas de garde-fou mis en place pour une utilisation externe)
;;--- d    correspond à la longueur désirée des segments (un lissage des longueurs n'est pas prévu)

;;--- Renvoie la liste des données concernant les segments. Chaque segment est sous la forme :
;;  (i pt1 pt2 ... ptn) avec i le numéro du segment et pt1, pt2, ... les coordonnées des sommets
;;  Un segment n'est en réalité une polyligne de longueur fixe (d) pouvant ainsi posséder plus de 2 sommets





;; ==================================================================================================================== ;;


;; Permet de retracer une polyligne existante en la scindant en plusieurs polylignes de longueur définie :
;;--- La commande (PLINE-JESSY) possède 2 interventions utilisateurs
;;--- 1°) Sélection de la polyligne à retracer (filtre actif sur "LWPOLYLINE" uniquement, inclu dans une boucle) une seule polyligne est
;;---  prise en compte (possibilité d'ajout d'une boucle pour traitement d'un jeu de sélection)
;;--- 2°) Définition de la longueur des segments souhaitée. La valeur par défaut est définie sur 1 (Appuyer sur ENTER pour prise en compte)

;;--- Le résultat obtenu est similaire au type de ligne "CACHE" avec une définition de l'échelle du type de ligne

(defun c:PLINE-CUT (/ get-pt-list seg-list name d ent-list n segment str)
  (defun get-pt-list (name / pt-list)
    (repeat (setq i (1+ (fix (vlax-curve-getendparam name))))
      (setq pt-list (cons (vlax-curve-getPointAtParam name (setq i (1- i))) pt-list))
    )
  )
  
  (defun get-segment-list (name dv dn / get-dist i dist dist-tmp dist-max pt-list last-pt pt fl d seg-list segment)
    (defun get-dist (pt-list / n dt)
      (setq
        n 0
        dt 0
      )
      (repeat (1- (length pt-list))
        (setq dt (+ (distance (nth n pt-list) (nth (setq n (1+ n)) pt-list)) dt))
      )
      dt
    )
    
    (setq
      dist 0
      i 0
      dist-max (vlax-curve-getdistatparam name (vlax-curve-getendparam name))
      pt-list (get-pt-list name)
      last-pt (car pt-list)
      pt-list (cdr pt-list)
      fl nil
    )
    (while (< (setq dist (+ dist (setq d (if (setq fl (not fl)) dv dn)))) dist-max)
      (cond
        ((> (setq dist-tmp (distance last-pt (car pt-list))) d)
          (setq pt (polar last-pt (angle last-pt (car pt-list)) d)
                segment (cons (setq i (1+ i)) (list last-pt pt))
                seg-list (cons segment seg-list)
                last-pt pt
          )
        )
        ((< dist-tmp d)
          (setq segment (list last-pt)
                dist-tmp 0
          )
          (while (< (setq dist-tmp (+ (distance last-pt (car pt-list)) dist-tmp)) d)
            (setq segment (cons (car pt-list) segment)
                  last-pt (car pt-list)
                  pt-list (cdr pt-list)
            )
          )
          (setq dist-tmp (- d (get-dist segment))
                pt (polar last-pt (angle last-pt (car pt-list)) dist-tmp)
                segment (cons (setq i (1+ i)) (reverse (cons pt segment)))
                seg-list (cons segment seg-list)
                last-pt pt
          )
        )
        ((= dist-tmp d)
          (setq pt (car pt-list)
                segment (cons (setq i (1+ i)) (list last-pt pt))
                seg-list (cons segment seg-list)
                last-pt pt
                pt-list (cdr pt-list)
          )
        )
      )
    )
    (if (= dist dist-max)
      (setq seg-list (reverse seg-list))
      (setq seg-list (reverse (cons (cons (setq i (1+ i)) (list last-pt (last pt-list))) seg-list)))
    )
  )
  
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (while
    (or
      (not (setq name (ssget "_:S+." '((0 . "LWPOLYLINE")))))
      (not (setq name (ssname name 0)))
    )
    (prompt "\nAucune polyligne n'a été sélectionnée, veuillez recommencer svp...")
  )
  (initget 6) ;; Empêche l'utilisateur de saisir une valeur nulle ou négative
  (if (null (setq dv (getreal "\nVeuillez spécifier la longueur des segments visibles <1.0> : ")))
    (setq dv 1.0)
  )
  (initget 6) ;; Empêche l'utilisateur de saisir une valeur nulle ou négative
  (if (null (setq dn (getreal (strcat "\nVeuillez spécifier la longueur des segments non-visibles <" (rtos dv) "> : "))))
    (setq dn dv)
  )
  (setq
    seg-list (get-segment-list name dv dn)
    ent-list (entget name)
    n 0
  )
  (foreach segment seg-list
    (if (= "5" (substr (setq str (rtos (/ (car segment) 2.0) 2 1)) (strlen str) 1))
      (if
        (entmake
          (append
            (vl-remove
              nil
              (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                (assoc 8 ent-list) ;; Calque
                (cons 90 (1- (length segment))) ;; Nb de sommets
                '(70 . 0) ;; Polyligne non fermée
                (if (assoc 62 ent-list) (assoc 62 ent-list)) ;; Si couleur forcée
                (if (assoc 43 ent-list) (assoc 43 ent-list)) ;; Si épaisseur globale
              )
            )
            (mapcar '(lambda (pt) (cons 10 pt)) (cdr segment))
          )
        )
        (setq n (1+ n))
      )
    )
  )
  (if (= n (fix (/ (1+ (length seg-list)) 2.0)))
    (progn
      (entdel name)
      (princ "\nAucune erreur n'a été détectée lors de l'exécution.")
    )
    (progn
      (entmod (if (assoc 62 ent-list)
          (subst (cons 62 10) (assoc 62 ent-list) ent-list)
          (append ent-list (list (cons 62 10)))
        )
      )
      (princ "\nUne erreur a été détectée lors de l'exécution...")
    )
  )
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)

)