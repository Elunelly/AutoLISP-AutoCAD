

;                          +---------------------------------------------------------------------------------------------+                          ;
;                          |                                                                                             |                          ;
;                          |          	    DEFINITION DES FONCTIONS ET COMMANDES EN SUPPORT INTERNE DU BE               |                          ;
;                          |                                                                                             |                          ;
;                          +---------------------------------------------------------------------------------------------+                          ;
;								VERSION INTERNE 02.06.2020							    ;



((lambda ()
	(if (not (tblsearch "STYLE" "MonoSpace"))
		(entmake
			'(
				(0 . "STYLE")
				(100 . "AcDbSymbolTableRecord")
				(100 . "AcDbTextStyleTableRecord")
				(2 . "MonoSpace")
				(70 . 0)
				(40 . 0.0)
				(41 . 1.0)
				(50 . 0.0)
				(71 . 0)
				(42 . 2.5)
				(3 . "monos.ttf")
				(4 . "")
			)
		)
    )
	(princ)
))

;; ============================================================= DEFINITION DES FONCTIONS ============================================================= ;;



	; Cr�ation d'une liste issue d'une "Symbol Table" selon un crit�re de recherche + possibilit�e de faire une liste simple de forme DXF (MAJ n�cessaire depuis la fonction (DXF_List) :

;--- La fonction (flt_tbl) poss�de 3 arguments
;--- tbl peut actuellement prendre 3 valeurs possibles ("LAYER", "BLOCK" ou "LTYPE"), permet de d�finir dans quelle "Symbol Table" la recherche est effectu�e.
;--- search est une cha�ne de caract�re correspondant au filtre que l'on souhaite appliquer � notre "Symbol Table" (ex : "UBS*" /= "*UBS" /= "UBS" ...).
;--- get d�fini si l'on souhaite obtenir une liste de n atomes 'STR ou une liste d'un unique atome compos� de chaque �l�ment s�par� par une virgule. (Amen� � dispara�tre)

;--- Renvoie une liste de la forme ("A" "B" "C" "D").
(defun flt_tbl (tbl search / lst_tbl name)

	(setq name (cdr (assoc 2 (tblnext tbl t))))
	(while (/= name nil)
		(if (wcmatch (strcase name) (strcase search))
			(setq lst_tbl (cons name lst_tbl))
		)
		(setq name (cdr (assoc 2 (tblnext tbl))))
	)
	lst_tbl
	
)

	; V�rification d'�galit� entre deux liste de m�me dimension :
;--- La fonction (lst_equal) poss�de 2 arguments
;--- Lst_1 correspond � la premi�re liste que l'on souhaite comparer � la seconde
;--- Lst_2 correspond � la seconde liste que l'on souhaite comparer � la premi�re

;--- Renvoie t si les deux listes sont �quivalentes, renvoie nil si les deux listes sont diff�rentes ou si l'on cherche � comparer deux listes de longueurs diff�rentes.
(defun lst_equal (lst_1 lst_2 / rslt)

	(if (= (vl-position (last lst_1) lst_1) (vl-position (last lst_2) lst_2))
		(progn
			(while lst_1
				(if (and (= (type (car lst_1)) (type (car lst_2)))
					 (= (vl-princ-to-string (car lst_1)) (vl-princ-to-string (car lst_2)))
				    )
					(setq lst_1 (cdr lst_1)
					      lst_2 (cdr lst_2)
					      rslt t
					)
					(setq lst_1 nil
					      lst_2 nil
					      rslt nil
					)
				)
			)
		)
		(setq rslt nil)
	)
	rslt

)


	; Permet de r�cup�rer la liste des sommets de l'entit� s�lectionn�e sous forme de liste de coordonn�es (2D ou 3D selon le type d'objet) :
;--- La fonction (get-pt-list) poss�de 1 argument
;--- name correspond au nom de l'entit� �tudi�

;--- Renvoie la liste de coordonn�es de chaque points de l'entit� dans le rep�re WCS (� confirmer !)
;--- A �tudier en d�tails sur les transpositions de coordonn�es (erreurs de calcul ?)
(defun Get-pt-list (name / ent-list pt-list)

	(if (= (type name) 'ENAME)
		(progn
			(setq ent-list (entget name))
			(while (setq ent-list (member (assoc 10 ent-list) ent-list))
				(setq pt-list (cons (cdr (assoc 10 ent-list)) pt-list)
				      ent-list (cdr ent-list)
				)
			)
		)
		(prompt "\nL'argument sp�cifi� n'est pas un nom d'entit�.\n")
	)
	(if pt-list
		(cond
			((= (cdr (assoc 0 (entget name))) "HATCH")
				(setq pt-list (cdr (reverse (cdr pt-list))))
			)
			((= (cdr (assoc 0 (entget name))) "ARC")
				(setq pt-list (append pt-list
						      (list
							(polar (car pt-list) (cdr (assoc 50 (entget name))) (cdr (assoc 40 (entget name))))
							(polar (car pt-list) (cdr (assoc 51 (entget name))) (cdr (assoc 40 (entget name))))
						      )
					      )
				)
			)
			((= (cdr (assoc 0 (entget name))) "LINE")
				(setq pt-list (reverse (cons (cdr (assoc 11 (entget name))) pt-list)))
			)
			((= (cdr (assoc 0 (entget name))) "POLYLINE")
				(setq ent-list (entget name))
				(while (/= (cdr (assoc 0 ent-list)) "SEQEND")
					(setq ent-list (entget (setq name (entnext name))))
					(if (assoc 10 ent-list)
						(setq pt-list (cons (cdr (assoc 10 ent-list)) pt-list))
					)
				)
				(setq pt-list (reverse (cdr (reverse pt-list))))
			)
			(t
				(setq pt-list (reverse pt-list))
			)
		)
		nil
	)

)

(defun pt-member (pt pt-list fuzz / d)

	(setq d (strlen (substr (vl-princ-to-string fuzz) (1+ (cond ((= (type fuzz) 'REAL) (vl-string-position (ascii ".") (vl-princ-to-string fuzz))) (t 0))))))
	(if (> (length pt) (length (car pt-list)))
		 (setq pt (reverse (cdr (reverse pt))))
	)
	(while (and pt-list
		    (not (equal (mapcar '(lambda (x) (rtos x 2 d)) pt) (mapcar '(lambda (x) (rtos x 2 d)) (car pt-list)) fuzz))
	       )
		(setq pt-list (cdr pt-list))
	)
	pt-list

)

	; Permet de r�cup�rer les coordonn�es du sommet d'une polyligne le plus proche d'un point sp�cifi� en argument :
;--- La fonction (osnap-poly) poss�de 2 argument
;--- name correspond au nom de l'entit� �tudi�
;--- Point correspond au point de d�part qui sera �tudi�

;--- Renvoie les coordonn�es du sommet le plus proche du point de d�part appartenant � l'entit� name, nil si la fonction (get-pt-list) retourne nil
(defun osnap-poly (name Point / pt-list dist pt)

	(if (setq pt-list (Get-pt-list name))
		(progn
			(foreach pt pt-list
				(setq dist (cons (cons (distance pt Point) pt) dist))
			)
			(setq Point (cdr (assoc (apply 'min (mapcar 'car dist)) dist)))
		)
	)

)

	; Permet d'ajouter un point pour les polylignes 2D uniquement � partir du point pr�c�dent sp�cifi� en argument :
;--- La fonction (Add-Poly2D-Point) poss�de 3 arguments
;--- name correspond au nom de l'entit� polyligne
;--- Start-pt correspond au point de d�part appartenant � la polyligne (ajout de la fonction (osnap-poly) pour utilisation dans un jsel)
;--- Add-pt correspond au point que l'on souhaite ajouter � la polyligne

;--- Renvoie la liste DXF de la nouvelle polyligne si fonctionnnelle, sinon nil
(defun Add-Poly2D-Point (name Start-pt Add-pt / entlist pt-list pos add)

	(if (= (cdr (assoc 0 (entget name))) "LWPOLYLINE")
		(setq entlist (entget name)
		      pt-list (Get-pt-list name)
		      Start-pt (osnap-poly name Start-pt)
		      pos (+ 5 (- (length entlist) (length (member Start-pt (mapcar 'cdr entlist)))))
		      add 	(list 	(assoc 40 (sublist entlist (- pos 4) nil))
					(assoc 41 (sublist entlist (- pos 4) nil))
					(assoc 42 (sublist entlist (- pos 4) nil))
					(assoc 91 (sublist entlist (- pos 4) nil))
				)
		      entlist	(entmod	(append
						(sublist entlist 1 pos)
						(append (list (cons 10 (if (/= 2 (length Add-pt)) (setq Add-pt (list (car Add-pt) (cadr Add-pt))) Add-pt))) add)
						(sublist entlist (1+ pos) nil)
					)
				)
		)
	)

)

	; Permet de r�cup�rer les coordonn�es et donn�es associ�es d'un point situ� � une distance sp�cifi�e d'un �l�ment lin�aire :
;--- La fonction (get-AlignPoint-AtDist) poss�de 3 arguments
;--- curve-obj correspond au nom d'entit� de l'objet servant de r�f�rence (peut �tre le nom d'entit� ou le VLA-Object)
;--- dist correspond � l'emplacement du point situ� sur la courbe pour une distance donn�e depuis le point de d�part
;--- e correspond au d�calage de la courbe de r�f�rence. Une valeur positive placera le point calcul� au-dessus de la courbe de r�f�rence et une valeur n�gative
; placera le point calcul� au-dessous de la courbe de r�f�rence (dans le sens de lecture de la courbe)

;---Renvoie une liste de paire point�e de la forme
;	((-1 . <Nom d'entit�>) (10 . PointOnCurve) (11 . PointOutsideCurve) (50 . Angle-Rad_Tangente) (1041 . DistanceOnCurve))
; si l'objet n'est pas un objet lin�aire, retourne nil
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

	; Permet de r�cup�rer la liste des pr�sentations sous forme de liste :
;--- La fonction (Get-Layout-list) poss�de aucun argument

;--- Renvoie une liste compos�e de paire point�e avec le premier �l�ment de la paire correspondant au nom de la pr�sentation, le second au VLA-Object de cette pr�sentation
(defun Get-Layout-list (/ layout layout-list)

	(vlax-for layout (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
		(setq layout-list (cons (cons (vla-get-name layout) layout) layout-list))
	)
	(reverse layout-list)

)

	; Permet de renommer une pr�sentation sans utiliser la fonction (command) :
;--- La fonction (Set-Layout-Name) poss�de 2 arguments
;--- layout correspond au nom de la pr�sentation � renommer
;--- name correspond au nouveau nom de la pr�sentation

;--- Renvoie le nouveau nom de la pr�sentation ou nil en cas d'�chec
(defun Set-Layout-Name (layout name / layout-list)

	(setq layout-list (get-layout-list))
	(if (assoc layout layout-list)
		(if (not (assoc name layout-list))
			(progn
				(vla-put-name (cdr (assoc layout layout-list)) name)
				(prompt (strcat "\nLa pr�sentation \"" layout "\" a �t� renomm�e en \"" name "\"."))
				name
			)
			(progn
				(while (assoc name layout-list)
					(cond
						((wcmatch name "* (#)")
							(setq name (strcat (substr name 1 (- (strlen name) 2)) (itoa (1+ (atoi (substr name (1- (strlen name)) 1)))) ")"))
						)
						((wcmatch name "* (##)")
							(setq name (strcat (substr name 1 (- (strlen name) 3)) (itoa (1+ (atoi (substr name (- (strlen name) 2) 2)))) ")"))
						)
						(t
							(setq name (strcat name " (2)"))
						)
					)
				)
				(vla-put-name (cdr (assoc layout layout-list)) name)
				(prompt (strcat "\nLa pr�sentation \"" layout "\" a �t� renomm�e en \"" name "\"."))
				name
			)
		)
		(progn
			(prompt (strcat "\nLa pr�sentation \"" layout "\" n'existe pas..."))
			(princ)
		)
	)

)

	; Fonction issue du fichier "NumIncV3-9.lsp" �crit par Lee-Mac. Permet d'incr�menter un nombre ou une cha�ne de caract�res avec un pas d�fini :
; La fonction (Sup-Incr) poss�de 2 arguments
;--- value correspond au nombre que l'on souhaite incr�menter (= (type str) 'STRING)
;--- pas correspond au pas que l'on souhaite appliquer � la cha�ne str (= (type inc) 'NUMBER)

;--- ATTENTION : AUCUN GARDE-FOU MIS EN PLACE
;--- Renvoie la valeur incr�ment�e
(defun Sup-Incr (value pas / i)

	(if (distof value 2)
		(Sup-Incr-Number value (vl-princ-to-string pas))
		(Sup-Incr-Alpha value (fix pas))
	)

)

	; Fonction issue du fichier "NumIncV3-9.lsp" �crit par Lee-Mac. Permet d'incr�menter un nombre avec un pas d�fini :
; La fonction (Sup-Incr-Number) poss�de 2 arguments
;--- str correspond au nombre que l'on souhaite incr�menter (= (type str) 'STRING)
;--- inc correspond au pas que l'on souhaite appliquer � la cha�ne str (= (type inc) 'STRING)

;--- ATTENTION : AUCUN GARDE-FOU MIS EN PLACE
;--- Renvoie la valeur incr�ment�e
(defun Sup-Incr-Number (str inc / _rtos _decimalplaces incd maxd num slen strd)

    (defun _rtos (real prec / dimzin result)
      
        (setq dimzin (getvar "DIMZIN"))
        (setvar "DIMZIN" 0)
        (setq result (rtos real 2 prec))
        (setvar "DIMZIN" dimzin)
        result
      
    )

    (defun _decimalplaces (string / pos)
      
        (if (setq pos (vl-string-position 46 string))
        	(- (strlen string) pos 1)
            	0
    	)
      
    )
    
    (setq num (+ (distof str) (distof inc)))

    (if (minusp (distof str))
    	(setq str (substr str 2))
    )
    (if (minusp (distof inc))
        (setq inc (substr inc 2))
    )
    (setq incd (_decimalplaces inc)
          strd (_decimalplaces str)
          maxd (max incd strd)
          slen (strlen str)
    )
    (cond
        (   (and (< 0 strd) (< 0 incd))
            (setq slen (+ (- slen strd) maxd))
        )
        (   (and (= 0 strd) (< 0 incd))
            (setq slen (+ incd slen 1))
        )
    )
    (setq str (_rtos num maxd))
    (if (minusp num)
        (setq str (substr str 2))
    )
    (while (< (strlen str) slen)
        (setq str (strcat "0" str))
    )
    (if (minusp num)
        (strcat "-" str)
        str
    )
)

	; Fonction issue du fichier "NumIncV3-9.lsp" �crit par Lee-Mac. Permet d'incr�menter une cha�ne de caract�res selon un pas d�fini :
; La fonction (Sup-Incr-Alpha) poss�de 2 arguments
;--- str correspond � la cha�ne de caract�res que l'on souhaite incr�menter (= (type str) 'STRING)
;--- inc correspond au pas que l'on souhaite appliquer � la cha�ne str (= (type inc) 'INTEGER)

;--- ATTENTION : AUCUN GARDE-FOU MIS EN PLACE ET NON COMPATIBILITE AVEC UNE INCREMENTATION NEGATIVE (� recouper avec (SupIncr))
;--- Renvoie la valeur incr�ment�e
(defun Sup-Incr-Alpha (str inc / _incrementalpha a)

    (defun _incrementalpha (a b / c d e)
        (cond
            (   (cond
                    (   (< 47 (setq c (car a)) 58)
                        (setq d 48
                              e 10
                        )
                    )
                    (   (< 64 c 91)
                        (setq d 65
                              e 26
                        )
                    )
                    (   (< 96 c 123)
                        (setq d 97
                              e 26
                        )
                    )
                )
                (setq c (+ (- c d) b)
                      b (/ c e)
                )
                (if (not (minusp c))
			(cons (+ d (rem c e))
	                    (if (zerop b)
	                        (cdr a)
	                        (if (cdr a)
	                            (_incrementalpha (cdr  a) b)
	                            (_incrementalpha (list d) (if (= 10 e) b (1- b)))
	                        )
	                    )
	                )
			(cons (+ d e (rem c e))
	                    (if (and (cdr a) (= (length (cdr a)) 1) (= (cadr a) d))
			    	'(0)
			    	(if (zerop b)
	                        	(_incrementalpha (cdr a) (1- b))
	                        	(_incrementalpha (list d) (if (= 10 e) b (1- b)))
				)
	                    )
	                )
		)
            )
            (   (cons c
                    (if (cdr a)
                        (_incrementalpha (cdr a) b)
                        (_incrementalpha (list 65) (1- b))
                    )
                )
            )
        )
    )

    (vl-list->string
        (reverse
		(vl-remove 0
	        	(if (setq a (reverse (vl-string->list str)))
		                (_incrementalpha a inc)
		                (_incrementalpha '(65) (1- inc))
	        	)
		)
        )
    )
)

	; Transforme la cha�ne de caract�res pass�e en argument sous forme de liste de caract�res isol�s :
;--- La fonction (atol) poss�de 1 argument
;--- string correspond � la cha�ne de caract�res qui va �tre d�compos�e en caract�res isol�s (ex : (atol "texte) = ("t" "e" "x" "t" "e") )

; Renvoie la liste de caract�res isol�s.
(defun atol (string)

	(if (= (type string) 'STR)
		(setq lst_string (mapcar '(lambda (x) (chr x)) (vl-string->list string)))
		(alert "Argument en entr�e diff�rent d'une cha�ne de caract�re")
	)

)

	; D�compose un nombre par rapport � sa partie enti�re et sa partie d�cimale sous forme de liste :
;--- La fonction (rtol) poss�de 2 arguments
;--- r correspond au nombre que l'on souhaite d�composer
;--- p correspond au nombre de d�cimales � afficher. Si nil, renvoie la partie d�cimale connue (cf. LUPREC)

;--- Renvoie une liste de la forme (num fix-part decimal-part) avec
;	num 		le nombre sp�cifi� en argument (conservation du type)
;	fix-part	la partie enti�re du nombre (type STRING)
;	decimal-part	la partie d�cimale du nombre (type STRING) | Dans le cas d'un nombre entier, renvoie nil
(defun rtol (r p / d l s)

	(cond
		((numberp p) (setq p (fix (abs p))))
		(t (setq p nil))
	)
	(cond
		((= (type r) 'REAL)
			(setq d (vl-string-position (ascii ".") (setq s (if p (rtos r 2 p) (vl-princ-to-string r))))
			      l (cons r (list (substr s 1 d) (substr s (+ d 2))))
			)
		)
		((= (type r) 'INT)
			(setq l (cons r (list (itoa r) (if p (repeat (progn (setq s "") p) (setq s (strcat s "0")))))))
		)
		((= (type r) 'STR)
			(setq d (vl-string-position (ascii ".") (setq s (if p (rtos (distof r 2) 2 p) r)))
			      l (cons r (list (substr s 1 (if d d)) (if d (substr s (+ d 2)))))
			)
		)
	)

)

	; R�cup�re la valeur du n-i�me code DXF au sein d'une liste de paires point�es :
;--- La fonction (get-DXF-value) poss�de 3 arguments
;--- entlist correspond � la liste DXF d'une entit� (ou une liste de paires point�es)
;--- code correspond au "group code" DXF de la valeur recherch�e
;--- n correspond � l'indice du code

;--- Renvoie la valeur associ�e au code DXF recherch�, nil sinon
(defun get-DXF-value (entlist code n)

	(while 	(and (setq entlist (member (assoc code entlist) entlist))
		     (> (setq n (1- n)) 0)
		)
		(setq entlist (cdr entlist))
	)
	(if (and entlist
		 (= n 0)
	    )
		(cdr (assoc code entlist))
	)

)

	; R�cup�re la liste de l'ensemble des valeurs associ�es � un code dans une liste de paires point�es :
;--- La fonction (get-DXF-list) poss�de 2 arguments
;--- entlist correspond � la liste DXF d'une entit� (ou une liste de paires point�es)
;--- code correspond au "group code" DXF de la valeur recherch�e

;--- Renvoie la listes des valeurs associ�es (dans l'ordre d'apparition au sein de la liste) au code DXF recherch�, nil sinon
(defun get-DXF-list (entlist code / n value lst)

	(setq n 0)
	(while (setq value (get-DXF-value entlist code (setq n (1+ n))))
		(setq lst (cons value lst))
	)
	(reverse lst)

)

	; R�cup�re une liste de paires point�es de l'ensemble des attributs du bloc sp�cifi� en argument et de leur valeur correspondantes :
;--- La fonction (Get-att-list) poss�de 1 arguments
;--- e_name correspond au nom de l'entit� cibl�e

;--- Renvoie une liste de paires point�es compos�es chacunes du nom de l'attribut et de sa valeur (ex : (("CODEPROJET" "C2244") ("N�_DESSIN" "1030 -") ("TITRE_2" "CALEPINAGE DES MODULES PV")) )
(defun Get-att-list (name / Att Att_List)

	(if  (= (cdr (assoc 0 (entget name))) "INSERT")
		(progn
			(setq Att_List (mapcar '(lambda (att) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke (vlax-ename->vla-object name) 'getattributes)))
			(if Att_List
				Att_List
			)
		)
		(prompt (strcat "\nErreur : Le nom d'entit� sp�cifi� ne fait pas r�f�rence � un bloc."
				"\nIl fait r�f�rence � : "
				(cdr (assoc 0 (entget name)))
				".\n"
			)
		)
	)
	
)

	; Permet de modifier la valeur d'un attribut pour un bloc sp�cifique :
;--- La fonction (Set-att-value) poss�de 3 arguments
;--- name correspond au nom d'entit� d'un bloc
;--- tag correspond au nom (�tiquette) de l'attribut vis� (la casse n'est pas d�terminante...)
;--- value correspond � la nouvelle valeur de l'attribut "tag"

; Renvoie t si l'attribut tag a �t� modifi�, nil en cas d'�chec (avec une aide pour d�tecter l'erreur)
(defun Set-att-value (name tag value)

	(if (and
		(= (cdr (assoc 0 (entget name))) "INSERT")
		(Get-att-list name)
		(member (strcase tag) (mapcar 'strcase (mapcar 'car (Get-att-list name))))
	    )
		(progn
			(setpropertyvalue name tag value)
			(princ)
			t
		)
		(progn
			(prompt (strcat "\nUne erreur est survenue : "
					(cond
						((/= (cdr (assoc 0 (entget name))) "INSERT")
							"\n - L'entit� n'est pas un bloc."
						)
						((not (Get-att-list name))
							"\n - Le bloc ne poss�de pas d'attribut."
						)
						((not (member tag (mapcar 'car (Get-att-list name))))
							(strcat "\n - Le bloc ne poss�de pas d'attribut nomm� \"" (strcase tag) "\" dans sa liste.")
						)
					)
				)
			)
			(princ)
		)
	)

)

	; Permet de modifier la valeur de chaque attribut sp�cifi� dans une liste de paire point�e de la forme ("tag" . "value") pour un bloc sp�cifique :
;--- La fonction (Set-att-list) poss�de 2 arguments
;--- name correspond au nom d'entit� d'un bloc
;--- lst correspond � une liste de paire point�es dont la cl� correspond au nom de l'�tiquette de l'attribut et le second terme � la valeur associ�e (ex : '(("tag1" . "value1") ("tag2" . "value2") ...) )

; Renvoie t si l'attribut tag a �t� modifi�, nil en cas d'�chec (avec une aide pour d�tecter l'erreur)
(defun Set-att-list (name lst / old-lst att-list tag-value)

	(if (and
		(= (cdr (assoc 0 (entget name))) "INSERT")
		(setq att-list (Get-att-list name))
		(setq old-lst lst)
		(setq lst (vl-remove-if-not '(lambda (x) (member (strcase (car x)) (mapcar 'strcase (mapcar 'car att-list)))) lst))
	    )
		(progn
			(foreach tag-value lst
				(prompt (strcat "\nModification de l'attribut \""
						(car tag-value)
						"\" passant de la valeur \""
						(getpropertyvalue name (strcase (car tag-value)))
						"\" � la valeur \""
						(cdr tag-value)
						"\"."
					)
				)
				(setpropertyvalue name (strcase (car tag-value)) (cdr tag-value))
			)
			(princ)
			t
		)
		(progn
			(prompt (strcat "\nUne erreur est survenue : "
					(cond
						((/= (cdr (assoc 0 (entget name))) "INSERT")
							"\n - L'entit� n'est pas un bloc."
						)
						((not att-list)
							"\n - Le bloc ne poss�de pas d'attribut."
						)
						((not lst)
							(strcat "\n - Le bloc ne poss�de aucun des attributs renseign�s : \"" (DXF_List (mapcar 'car old-lst) "\", \"" "right" t t) "\".")
						)
					)
				)
			)
			(princ)
		)
	)

)

	; R�cup�re une liste de paires point�es de l'ensemble des propri�t�s dynamiques du bloc sp�cifi� en argument et de leur valeur correspondantes :
;--- La fonction (Get-DynProp-list) poss�de 1 argument
;--- e_name correspond au nom de l'entit� cibl�e

;--- Renvoie une liste de paires point�es compos�es chacunes du nom de la propri�t� dynamique et de sa valeur (ex : (("Table de blocs1" . 0) ("Visibilit�1" . "Masquer") ("Largeur Cartouche" . 1178.24) ("Origin" 209.874 -0.323759) ("Hauteur Cartouche" . 832.35)) )
(defun Get-DynProp-list (name / obj DynProp_List DynProp)

	(if (= (cdr (assoc 0 (entget name))) "INSERT")
		(progn
			(setq obj (vlax-ename->vla-object name)
			      DynProp_List (mapcar '(lambda (DynProp) (cons (vla-get-propertyname DynProp) (vlax-get DynProp 'value))) (vlax-invoke obj 'getdynamicblockproperties))
			)
			(if (setq DynProp_List (vl-remove-if '(lambda (x) (= (car x) "Origin")) DynProp_List))
				DynProp_List
			)
		)
		(prompt (strcat "\nErreur : Le nom d'entit� sp�cifi� ne fait pas r�f�rence � un bloc."
				"\nIl fait r�f�rence � : "
				(cdr (assoc 0 (entget name)))
				".\n"
			)
		)
	)

)

;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun Set-DynProp-Value (name tag value / dyn-list)

	(if (and (= (cdr (assoc 0 (entget name))) "INSERT")
		 (setq dyn-list (Get-DynProp-list name))
		 (member (strcase tag) (mapcar '(lambda (x) (strcase (car x))) dyn-list))
	    )
		(progn
			(setpropertyvalue name (strcat "AcDbDynBlockProperty" tag) value)
			(princ)
			t
		)
		(progn
			(prompt (strcat "\nUne erreur est survenue : "
					(cond
						((/= (cdr (assoc 0 (entget name))) "INSERT")
							"\n - L'entit� n'est pas un bloc."
						)
						((null dyn-list)
							"\n - Le bloc ne poss�de pas de propri�t�(s) dynamique(s)."
						)
						((not (member (strcase tag) (mapcar '(lambda (x) (strcase (car x))) dyn-list)))
							(strcat "\n - Le bloc ne poss�de pas de propri�t� dynamique nomm�e \"" (strcase tag) "\" dans sa liste.")
						)
					)
				)
			)
			(princ)
		)
	)

)

;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun Get-Visibility-Name (name / vis)

	(if (and
		(vlax-property-available-p (setq name (vlax-ename->vla-object name)) 'effectivename)
		(setq name
		      	(vla-item
				(vla-get-blocks (vla-get-document name))
				(vla-get-effectivename name)
			)
		)
		(= :vlax-true (vla-get-isdynamicblock name))
		(= :vlax-true (vla-get-hasextensiondictionary name))
		(setq vis
			(vl-some
				'(lambda (x)
					(if (and
					    	(= 360 (car x))
						(= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr x)))))
					    )
						(cdr x)
					)
				)
				(dictsearch
					(vlax-vla-object->ename (vla-getextensiondictionary name))
					"ACAD_ENHANCEDBLOCK"
				)
			)
		)
	    )
		(cdr (assoc 301 (entget vis)))
	)

)

  ;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun Get-DynProp-AllowedValues (name tag)

	(if (and
	    	(Get-DynProp-list name)
		(member (strcase tag) (mapcar '(lambda (x) (strcase (car x))) (Get-DynProp-list name)))
	    )
		(vl-some '(lambda (x) (if (= tag (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
		        (vlax-invoke (vlax-ename->vla-object name) 'getdynamicblockproperties)
		)
	)

)

	; Permet de cr�er des demandes de jeu de s�lection en fonction d'un filtre plus �largie que les filtres (ssget) :
; La fonction (Select-filter) poss�de 4 arguments :
;--- mode correspond au type du filtre devant �tre appliqu�. Seul 4 valeurs sont autoris�es :
;	- "DXF" pour appliquer un filtre de s�lection �quivalent � la fonction (ssget) avec une m�thode de s�lection et une liste DXF filtr�e (liste de point non prise en compte)
;	- "BLC" applique un filtre pour les blocs selon une m�thode de s�lection d�finie et une liste DXF filtr�e (optionnelle) puis filtre en fonction du nom EFFECTIF du bloc (prise en compte des blocs dynamiques)
;	- "ATT" revient � une m�thode de s�lection "_X" puis filtre en fonction du nom d'un attribut et de sa valeur associ�e (nom relatif pris en compte)
;	- "DYN" revient � une m�thode de s�lection "_X" puis filtre en fonction du nom d'une propri�t� dynamique et de sa valeur associ�e (nom relatif pris en compte)
;--- blocname est n�cessaire uniquement pour les modes "BLC", "ATT" et "DYN" permettant de filtrer les blocs en fonction de leur nom EFFECTIF (wcmatch)
;		si mode = "DXF" alors blocname correspond � la liste des filtres concernant les quantitatifs retourn�s (codes DXF)
;--- tag correspond � la m�thode de s�lection (ssget) si mode = "DXF" ou "BLC", sinon correspond au nom de l'attribut ou de la propri�t� dynamique (wcmatch). Si tag = nil, appliquera la m�thode par d�faut (pause pour "DXF", et "_X" pour "BLC")
;	Si tag = "User" et mode = "BLC", appliquera la m�thode pause pour (ssget)
;	Se r�f�rer aux m�thodes (ssget) via le site suivant : https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2018/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0F37CC5E-1559-4011-B8CF-A3BA0973B2C3-htm.html
;	Liste non-exhaustive des m�thodes courantes :
;		- "_X" Entire database. Including entities on layers that are off, frozen, and out of the visible screen.
;		- "_C" Crossing selection; all objects crossing and inside of the specified rectangle.
;		- "_F" Fence selection ; all objects crossing the specified line.
;		- "_L" Last visible object added to the database.
;		- "_P" Previous selection set is selected.
;		- "_W" Window selection (all objects within the specified rectangle).
;		- "_:L" Allows for the selection of unlocked layers only.
;		- "_:S" Allows for a single selection only, one or more objects can be selected.
;		- "_:S+." Allows for a single object selection only.
;		- "pause" Valeur personnalis�e d�finissant une m�thode de s�lection libre
;--- value correspond � la liste filter (ssget) si mode = "DXF" ou "BLC", sinon correspond � la valeur de l'attribut ou de la propri�t� dynamique (wcmatch). Si value = nil, aucun filtre sera appliqu� � la s�lection (sauf pour "BLC" qui ne conserve que les blocs)

; Renvoie un jeu de s�lection apr�s avoir �t� filtr� ou nil si le jeu de s�lection est vide.

(defun Select-filter (mode blocname tag value / jsel i n name BlocList AttList DynList att-list dyn-list lst txt)

	(cond
		((= mode "DXF")
			(progn
				(cond
					((and	; m�thode de s�lection non d�finie ou bien simple avec filtre (sans liste de points n�cessaire)
				      		(or (= (type tag) 'STR) (null tag))
				      		(= (type value) 'LIST)
				    	 )
						(if (not (last (ssgetfirst)))
							(if (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget tag value)))))
								(setq jsel (ssget value))
							)
							(setq jsel (ssget "I" value))
						)
					)
					((and	; m�thode de s�lection non d�finie ou bien simple sans filtre (sans liste de points n�cessaire)
					 	(or (= (type tag) 'STR) (null tag))
						(null value)
					 )
						(if (not (last (ssgetfirst)))
							(if (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget tag)))))
								(setq jsel (ssget))
							)
							(setq jsel (ssget "I"))
						)
					)
					((and	; m�thode de s�lection sous forme de liste avec filtre (avec liste de points n�cessaire)
				      		(= (type tag) 'LIST)
				      		(= (type value) 'LIST)
				    	 )
						(if (not (last (ssgetfirst)))
							(if (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag) value)))))
								(setq jsel (ssget "_CP" (cadr tag) value))
							)
							(progn
								(setq jsel (ssget "I" value)
								      i 0
								)
								(if (vl-catch-all-error-p (setq jseltemp (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag) value)))))
									(setq jseltemp (ssget "_CP" (cadr tag) value))
								)
								(while (< i (sslength jsel))
									(setq name (ssname jsel i))
									(if (not (ssmemb name jseltemp))
										(ssdel name jsel)
										(setq i (1+ i))
									)
								)
							)
						)
					)
					((and	; m�thode de s�lection sous forme de liste sans filtre (avec liste de points n�cessaire)
					 	(= (type tag) 'LIST)
						(null value)
					 )
						(if (not (last (ssgetfirst)))
							(if (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget tag)))))
								(setq jsel (ssget))
							)
							(progn
								(setq jsel (ssget "I")
								      i 0
								)
								(if (vl-catch-all-error-p (setq jseltemp (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag))))))
									(setq jseltemp (ssget "_CP" (cadr tag)))
								)
								(while (< i (sslength jsel))
									(setq name (ssname jsel i))
									(if (not (ssmemb name jseltemp))
										(ssdel name jsel)
										(setq i (1+ i))
									)
								)
							)
						)
					)
					(t
						(prompt (strcat "\nUne erreur de saisie a �t� d�tect�e sur les arguments \""
								(vl-princ-to-string tag)
								"\" (suppos� �tre une cha�ne de caract�re) et \""
								(vl-princ-to-string value)
								"\" (suppos� �tre une liste)."
								"\n"
							)
						)
					)
				)
				(if (and jsel (= (getvar "CMDACTIVE") 0) (listp blocname))
					(progn
						(setq i 0)
						(foreach x blocname
							(set (read (strcat "lst_" (itoa x))) nil)
						)
						(while (< i (sslength jsel))
							(setq name (ssname jsel i)
							      i (1+ i)
							)
							(foreach x blocname
								(set (read (strcat "lst_" (itoa x))) (cons (cdr (assoc x (entget name))) (vl-symbol-value (read (strcat "lst_" (itoa x))))))
							)
						)
						(prompt (strcat "\nNombre d'objet(s) s�lectionn�(s) = "
								(itoa (sslength jsel))
								" u"
								(apply 'strcat (mapcar '(lambda (x)
												(strcat "\nListe des entit�s trouv�es pour le code DXF \"" (itoa x) "\" : "
													(apply 'strcat (mapcar '(lambda (y) 
																	(strcat "\n  - \""
																		(vl-princ-to-string y)
																		"\" ("
																		(itoa (setq n (length (vl-remove-if-not '(lambda (e) (= (strcase (vl-princ-to-string y)) (strcase (vl-princ-to-string e)))) (vl-symbol-value (read (strcat "lst_" (itoa x))))))))
																		"u - "
																		(rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
																		"%)"
																	)
																)
															     	(DXF_List (vl-symbol-value (read (strcat "lst_" (itoa x)))) nil nil t nil)
															)
													)
												)
											)
											blocname
										)
								)
								"\n"
							)
						)
						(foreach x blocname
							(set (read (strcat "lst_" (itoa x))) nil)
						)
						(princ)
					)
				)
			)
		)
		((= mode "BLC")
			(if (flt_tbl "BLOCK" blocname)
				(progn
					(setq i 0)
					(cond
						((and	; m�thode de s�lection non d�finie ou bien simple avec filtre (sans liste de points n�cessaire)
					      		(or (= (type tag) 'STR) (null tag))
					      		(= (type value) 'LIST)
					    	 )
							(if (not (last (ssgetfirst)))
								(if (or (and tag (= (strcase tag) (strcase "User"))) (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget tag (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value)))))))
									(setq jsel (if (and tag (= (strcase tag) (strcase "User")))
										   	(progn
												(prompt (strcat "\nSeul(s) le(s) bloc(s) \""
														(DXF_List (flt_tbl "BLOCK" blocname) "\", \"" "right" t t)
														"\" seront pris en compte dans la s�lection."
													)
												)
										   		(ssget (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value))
											)
										   	(ssget "_X" (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value))
										   )
									)
								)
								(setq jsel (ssget "I" (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value)))
							)
						)
						((and	; m�thode de s�lection non d�finie ou bien simple sans filtre (sans liste de points n�cessaire)
						 	(or (= (type tag) 'STR) (null tag))
							(null value)
						 )
							(if (not (last (ssgetfirst)))
								(if (or (and tag (= (strcase tag) (strcase "User"))) (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget tag (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))))))
									(setq jsel (if (and tag (= (strcase tag) (strcase "User")))
										   	(progn
												(prompt (strcat "\nSeul(s) le(s) bloc(s) \""
														(DXF_List (flt_tbl "BLOCK" blocname) "\", \"" "right" t t)
														"\" seront pris en compte dans la s�lection."
													)
												)
										   		(ssget (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))))
											)
										   	(ssget "_X" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))))
										   )
									)
								)
								(setq jsel (ssget "I" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
							)
						)
						((and	; m�thode de s�lection sous forme de liste avec filtre (avec liste de points n�cessaire)
					      		(= (type tag) 'LIST)
					      		(= (type value) 'LIST)
					    	 )
							(if (not (last (ssgetfirst)))
								(if (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag) (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value))))))
									(setq jsel (ssget "_CP" (cadr tag) (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value)))
								)
								(progn
									(setq jsel (ssget "I" (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value))
									      i 0
									)
									(if (vl-catch-all-error-p (setq jseltemp (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag) (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value))))))
										(setq jseltemp (ssget "_CP" (cadr tag) (append (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))) value)))
									)
									(while (< i (sslength jsel))
										(setq name (ssname jsel i))
										(if (not (ssmemb name jseltemp))
											(ssdel name jsel)
											(setq i (1+ i))
										)
									)
								)
							)
						)
						((and	; m�thode de s�lection sous forme de liste sans filtre (avec liste de points n�cessaire)
						 	(= (type tag) 'LIST)
							(null value)
						 )
							(if (not (last (ssgetfirst)))
								(if (vl-catch-all-error-p (setq jsel (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag) (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))))))))
									(setq jsel (ssget "_CP" (cadr tag) (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
								)
								(progn
									(setq jsel (ssget "I" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))))
									      i 0
									)
									(if (vl-catch-all-error-p (setq jseltemp (vl-catch-all-apply '(lambda () (ssget (car tag) (cadr tag) (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t))))))))
										(setq jseltemp (ssget "_CP" (cadr tag) (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
									)
									(while (< i (sslength jsel))
										(setq name (ssname jsel i))
										(if (not (ssmemb name jseltemp))
											(ssdel name jsel)
											(setq i (1+ i))
										)
									)
								)
							)
						)
						(t
							(prompt (strcat "\nUne erreur de saisie a �t� d�tect�e sur les arguments \""
									(vl-princ-to-string tag)
									"\" (suppos� �tre une cha�ne de caract�re) et \""
									(vl-princ-to-string value)
									"\" (suppos� �tre une liste)."
									"\n"
								)
							)
						)
					)
					(if (and jsel (= (getvar "CMDACTIVE") 0))
						(progn
							(setq i 0)
							(while (< i (sslength jsel))
								(setq name (ssname jsel i))
								(if (or (/= (cdr (assoc 0 (entget name))) "INSERT") (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list name "BlockTableRecord/Name"))))
									(ssdel name jsel)
									(if (not (wcmatch (strcase (getpropertyvalue name "BlockTableRecord/Name")) (strcase blocname)))
										(ssdel name jsel)
										(setq BlocList (cons (getpropertyvalue name "BlockTableRecord/Name") BlocList)
										      i (1+ i)
										)
									)
								)
							)
							(setq n 0)
							(prompt (strcat "\nNombre d'objet(s) s�lectionn�(s) = "
									(itoa (sslength jsel))
									" u"
									"\nListe des blocs trouv�s : "
									(apply 'strcat (mapcar '(lambda (x)
													(strcat "\n  - "
														x
														" ("
														(itoa	(progn
																(setq n 0)
																(repeat (setq i (sslength jsel))
																	(if (lst_equal (vl-string->list (strcase (getpropertyvalue (ssname jsel (setq i (1- i))) "BlockTableRecord/Name"))) (vl-string->list (strcase x)))
																		(setq n (1+ n))
																	)
																)
																n
															)
														)
														"u - "
														(rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
														"%)"
													)
												)
												(DXF_List Bloclist nil nil t nil)
											)
									)
									"\n"
								)
							)
						)
						(prompt "\Aucun bloc n'a �t� trouv� dans le dessin...\n")
					)
				)
				(prompt "\nAucune d�finition de bloc correspond � la recherche...\n")
			)
		)
		((= mode "ATT")
			(if (flt_tbl "BLOCK" blocname)
				(progn
					(setq i 0)
					(if (not (setq jsel (last (ssgetfirst))))
						(setq jsel (ssget "_X" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
						(setq jsel (ssget "I" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
					)
					(if (and jsel (= (getvar "CMDACTIVE") 0))
						(progn
							(while (< i (sslength jsel))
								(setq name (ssname jsel i)
								      att-list (if (= (cdr (assoc 0 (entget name))) "INSERT") (Get-att-list name))
								)
								(if (and
									att-list
									(not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list name "BlockTableRecord/Name"))))
									(wcmatch (strcase (getpropertyvalue name "BlockTableRecord/Name")) (strcase blocname))
									(setq att-list (vl-remove-if-not '(lambda (x) (wcmatch (strcase (car x)) (strcase tag))) att-list))
									(vl-remove-if-not '(lambda (x) (wcmatch (strcase (cdr x)) (strcase value))) att-list)
								    )
									(setq BlocList (cons (getpropertyvalue name "BlockTableRecord/Name") BlocList)
									      AttList (append att-list AttList)
									      i (1+ i)
									)
									(ssdel name jsel)
								)
							)
							(setq lst (sort-cons AttList)
							      txt ""
							)
							(prompt (strcat "\nNombre d'objet(s) s�lectionn�(s) = "
									(itoa (sslength jsel))
									" u selon la recherche des propri�t�s :"
									(while lst
										(setq txt (strcat txt
												  "\n  - \""
												  (vl-princ-to-string (caar lst))
												  "\" = \""
												  (vl-princ-to-string (cdar lst))
												  "\" ( "
												  (itoa (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst)))
												  "u - "
												  (rtos (* 100 (/ (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst)) (atof (rtos (length AttList) 2 2)))) 2 2)
												  "%)"
											  )
										      lst (vl-remove (car lst) lst)
										)
										txt
									)
									"\nListe des blocs trouv�s : "
									(apply 'strcat (mapcar '(lambda (x)
													(strcat "\n  - "
														x
														" ("
														(itoa	(progn
																(setq n 0)
																(repeat (setq i (sslength jsel))
																	(if (lst_equal (vl-string->list (strcase (getpropertyvalue (ssname jsel (setq i (1- i))) "BlockTableRecord/Name"))) (vl-string->list (strcase x)))
																		(setq n (1+ n))
																	)
																)
																n
															)
														)
														"u - "
														(rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
														"%)"
													)
												)
												(DXF_List Bloclist nil nil t nil)
											)
									)
									"\n"
								)
							)
						)
						(prompt "\Aucun bloc n'a �t� trouv� dans le dessin...\n")
					)
				)
				(prompt "\nAucune d�finition de bloc correspond � la recherche...\n")
			)
		)
		((= mode "DYN")
			(if (flt_tbl "BLOCK" blocname)
				(progn
					(setq i 0)
					(if (not (setq jsel (last (ssgetfirst))))
						(setq jsel (ssget "_X" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
						(setq jsel (ssget "I" (list '(0 . "INSERT") (cons 2 (DXF_List (list blocname "`*U*") "," "right" t t)))))
					)
					(if (and jsel (= (getvar "CMDACTIVE") 0))
						(progn
							(while (< i (sslength jsel))
								(setq name (ssname jsel i)
								      dyn-list (if (= (cdr (assoc 0 (entget name))) "INSERT") (Get-DynProp-list name))
								)
								(if (and
									dyn-list
									(not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list name "BlockTableRecord/Name"))))
									(wcmatch (strcase (getpropertyvalue name "BlockTableRecord/Name")) (strcase blocname))
									(setq dyn-list (vl-remove-if-not '(lambda (x) (wcmatch (strcase (car x)) (strcase tag))) dyn-list))
									(vl-remove-if-not '(lambda (x) (wcmatch (strcase (vl-princ-to-string (cdr x))) (strcase (vl-princ-to-string value)))) dyn-list)
								    )
									(setq BlocList (cons (getpropertyvalue name "BlockTableRecord/Name") BlocList)
									      DynList (append dyn-list DynList)
									      i (1+ i)
									)
									(ssdel name jsel)
								)
							)
							(setq lst (sort-cons DynList)
							      txt ""
							)
							(prompt (strcat "\nNombre d'objet(s) s�lectionn�(s) = "
									(itoa (sslength jsel))
									" u selon la recherche des propri�t�s :"
									(while lst
										(setq txt (strcat txt
												  "\n  - \""
												  (vl-princ-to-string (caar lst))
												  "\" = \""
												  (vl-princ-to-string (cdar lst))
												  "\" ( "
												  (itoa (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst)))
												  "u - "
												  (rtos (* 100 (/ (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst)) (atof (rtos (length DynList) 2 2)))) 2 2)
												  "%)"
											  )
										      lst (vl-remove (car lst) lst)
										)
										txt
									)
									"\nListe des blocs trouv�s : "
									(apply 'strcat (mapcar '(lambda (x)
													(strcat "\n  - "
														x
														" ("
														(itoa	(progn
																(setq n 0)
																(repeat (setq i (sslength jsel))
																	(if (lst_equal (vl-string->list (strcase (getpropertyvalue (ssname jsel (setq i (1- i))) "BlockTableRecord/Name"))) (vl-string->list (strcase x)))
																		(setq n (1+ n))
																	)
																)
																n
															)
														)
														"u - "
														(rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
														"%)"
													)
												)
												(DXF_List Bloclist nil nil t nil)
											)
									)
									"\n"
								)
							)
						)
						(prompt "\Aucun bloc n'a �t� trouv� dans le dessin...\n")
					)
				)
				(prompt "\nAucune d�finition de bloc correspond � la recherche...\n")
			)
		)
		(t
			(prompt (strcat "\nLe mode \""
					(vl-princ-to-string mode)
					"\" ne correspond � aucun mode connu..."
					"\nCet argument � 4 valeurs d�finies : \"DXF\", \"BLC\", \"ATT\" ou \"DYN\".\n"
				)
			)
		)
	)
	(sssetfirst nil jsel)
	(if (and jsel (> (sslength jsel) 0))
		jsel
		nil
	)

)

	; Supprime les doublons au sein d'une liste sp�cifi�e en argument :
;--- La fonction (Remove-Double) poss�de 1 argument
;--- lst1 correspond � la liste devant �tre �valu�e pour la suppression des doublons

;--- Renvoie la liste modifi�e sans ses doublons
(defun Remove-Double (lst1 / lst2)

	(while (car lst1)
		(setq Lst2 (cons (car lst1) lst2)
		      Lst1 (vl-remove (car lst1) lst1)
		)
	)
	(reverse lst2)

)

	; Trie une liste de paires point�es dans l'ordre croissant selon la t�te de chaque paire sans supprimer les doublons :
;--- La fonction (sort-cons) poss�de 1 argument
;--- lst correspond � la liste devant �tre tri�e

;--- Renvoie la liste tri�e sous r�serve que cette liste soit compos�e uniquement de liste de paires point�es, sinon renvoit la liste de d�part
(defun sort-cons (lst / lg-list n)

	(if (and (not (member nil (mapcar 'vl-consp lst)))
		 (null (vl-remove nil (mapcar 'vl-list-length lst)))
	    )
		(progn
			(while lst
				(setq lg-list (cons (cons (car lst) (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst))) lg-list)
				      lst (vl-remove (car lst) lst)
				)
			)
			(setq lg-list
			      	(vl-sort
				  	lg-list
					'(lambda (e1 e2)
						(if (= (vl-princ-to-string (caar e1)) (vl-princ-to-string (caar e2)))
							(< (vl-princ-to-string (cdar e1)) (vl-princ-to-string (cdar e2)))
							(< (vl-princ-to-string (caar e1)) (vl-princ-to-string (caar e2)))
						)
					 )
				)
			)
			(foreach n lg-list
				(repeat (cdr n)
					(setq lst (cons (car n) lst))
				)
			)
			(setq lst (reverse lst))
		)
		lst
	)

)

	; Permet de trier une liste en ignorant la la casse et en �vitant les erreurs de num�rotations induites par la fonction (vl-sort) :
;--- La fonction (sort-list) poss�de 2 arguments
;--- lst correspond � la liste devant �tre tri�e, pouvant contenir tout type d'�l�ment (cha�nes de caract�res � conseiller)
;--- flag d�fini si l'on souhaite trier la liste dans l'ordre croissant, ou d�croissant
;	flag = 0 -> Trie dans l'ordre croissant
;	flag = 1 -> Trie dans l'ordre d�croissant

;--- Renvoie la liste une fois tri�e
(defun Sort-list (lst flag / a b)

	(if (not (member flag '(0 1)))
		(setq flag 0)
	)
	(mapcar 'car
		(vl-sort
			(get-pattern-list lst)
			'(lambda (a b)
			 	(if (= (caadr a) (caadr b))
					((cond ((= flag 0) <) ((= flag 1) >)) (atoi (cdadr a)) (atoi (cdadr b)))
					((cond ((= flag 0) <) ((= flag 1) >)) (caadr a) (caadr b))
				)
			)
		)
	)

)

	; Permet de d�composer chaque membre d'une liste sous forme de sous-liste compos�e de l'�l�ment original, puis de sa racine (ou pattern) et de son incr�ment :
;--- La fonction (get-pattern-list) poss�de 1 argument
;--- lst correspond � la liste que l'on souhaite �tudier

;--- Renvoie une liste avec la d�composition des membres sous la forme (str . (pattern . num)) avec :
;	- str l'�l�ment (avec conservation du type) qui sera d�composer
;	- pattern correspond � une cha�ne de caract�res majuscule correspondant � l'�l�ment str dont on a retir� les chiffres situ�s en fin de cha�ne uniquement (hypoth�se)
;	- num correspond � une ch�ine de caract�res correspondant � l'incr�ment du pattern, soit l'ensemble des chiffres situ�s en fin de cha�ne (hypoth�se)
;		Ex. : Avec str = "Calque1", on a pattern = "CALQUE" et num = "1" ; Avec str = "Test", on a pattern = "TEST" et num = "" ; Avec str = 15, on a pattern = "" et num = "15" ; ...
(defun get-pattern-list (lst / str pattern)

	(mapcar '(lambda (str / pattern)
			(if (distof (vl-princ-to-string str) 2)
				(cons str (list (cons "" (vl-princ-to-string str))))
				(progn
					(setq pattern (vl-princ-to-string str))
					(while (wcmatch pattern "*#")
						(setq pattern (substr pattern 1 (1- (strlen pattern))))
					)
					(cons str (list (cons (strcase pattern) (substr (vl-princ-to-string str) (1+ (strlen pattern))))))
				)
			)
		 )
		lst
	)

)

	; Permet de modifier une liste de cha�nes de caract�res en une liste remani�e aux d�sirs de l'utilisateur :
;--- La fonction (DXF_List) poss�de 5 arguments
;--- Lst d�fini la liste que l'on va �valuer et modifier avec cette fonction (ex : ("Ceci" "est" "un" "exemple") )
;--- string d�termine avec quelle cha�ne de caract�res on souhaite lier nos �l�ments (ex : ",", " ", "\n - ", ...)
;--- Pos d�termine de quel c�t� on souhaite ajouter la cha�ne de caract�res de liaison ("left" ou "right", tout le reste renverra la List)
;--- Tri d�termine si l'on souhaite trier les valeurs de la liste en entr�e. Tri si la valeur de Tri est diff�rente de "" (ex : ("Ceci" "est" "exemple" "un") )
;--- Supp d�termine si l'on souhaite supprimer la cha�ne de caract�res de liaison � l'extr�mit� gauche ou droite en fonction de la valeur de Pos. Effectue la suppression si Supp est diff�rent de "".

;--- Renvoie la liste remani�e en fonction des diff�rents param�tres (ex : (DXF_List '("Ceci" "est" "un" "exemple") " " "right" "" "oui") = ("Ceci est un exemple") )
(defun DXF_List (Lst string Pos Tri Supp / New_List)

  	(if Tri
		(progn
			(setq Lst (Remove-Double Lst))
			(setq Lst (sort-list Lst 0))
		)
	)
	(cond
		((and string (= Pos "left"))
		 	(setq New_List (apply 'strcat (mapcar '(lambda (x) (strcat string (vl-princ-to-string x))) Lst)))
			(if Supp
			  	(setq New_List (vl-string-left-trim string New_List))
			)
		)
		((and string (= Pos "right"))
			(setq New_List (apply 'strcat (mapcar '(lambda (x) (strcat (vl-princ-to-string x) string)) Lst)))
			(if Supp
			  	(setq New_List (vl-string-right-trim string New_List))
			)
		)
		(t (setq New_List Lst))
	)
  	New_List

)

	; Permet de renvoyer une sous-liste d'une liste sp�cifi�e en argument (similaire � la fonction (substr)) :
;--- La fonction (sublist) poss�de 3 arguments
;--- lst correspond � la liste que l'on souhaite d�couper
;--- start correspond � la position de la premi�re valeur conserv�e. Le premier �l�ment de la liste correspond � 1
;--- lngth correspond au nombre d'�l�ments � conserver dans la liste retourn�e

;--- Renvoit la sous-liste
(defun sublist (lst s l)

	(repeat (1- s) (setq lst (cdr lst)))
	(setq lst (reverse lst))
	(if (or (null l)
		(minusp l)
		(> l (length lst))
	    )
		lst
		(repeat (- (length lst) l)
			(setq lst (cdr lst))
		)
	)
	(reverse lst)

)

	; Permet de diviser une liste sp�cifi�e en argument en plusieurs listes de longueurs �gale (except� la derni�re qui correspond au reste) :
;--- La fonction (divlist) poss�de 2 arguments
;--- lst correspond � la liste que l'on souhaite diviser
;--- l correspond au nombre d'�l�ments dans chaque division de la liste

;--- Renvoit la liste une fois divis�e, nil si les arguments ne sont pas corrects
(defun divlist (lst l / i newlist)

	(if (or (null l)
		(minusp l)
		(> l (length lst))
	    )
		lst
	  	(progn
			(setq i 0)
			(while (< i (length lst))
				(setq newlist (cons (sublist lst (1+ i) l) newlist)
				      i (+ i l)
				)
			)
			(reverse newlist)
		)
	)

)

	; Permet de calculer une division �galitaire enti�re par rapport au nombre total et un nombre de d'�l�ment dans chaque division � ne pas d�passer (en lien avec la fonction (divlist)) :
;--- La fonction (fix-div) poss�de 2 arguments
;--- n correspond au nombre total d'�l�ment dans une liste -> (length list) par exemple
;--- l correspond au nombre d'�l�ments dans chaque division de la liste que l'on ne souhaite pas d�passer

;--- Renvoit le nombre d'�l�ment n�cessaire pour une s�paration �gale entre chaque division d'une liste
(defun fix-div (n l / d s)

	(cond
		((not (member nil (mapcar '(lambda (x) (= x (ascii "0"))) (vl-string->list (setq s (substr (vl-princ-to-string (setq d (/ n (atof (rtos l 2 1))))) (+ 2 (vl-string-position (ascii ".") (vl-princ-to-string d)))))))))
			(setq  l (fix l))
		)
		(t
			(if (not (member nil (mapcar '(lambda (x) (= x (ascii "0"))) (vl-string->list (setq s (substr (vl-princ-to-string (setq l (/ n (atof (rtos (1+ (fix d)) 2 1))))) (+ 2 (vl-string-position (ascii ".") (vl-princ-to-string l)))))))))
				(setq l (fix l))
				(setq l (1+ (fix l)))
			)
		)
	)
	l

)

	; V�rifie si le calque sp�cifi� dans l'argument "name" est pr�sent dans la liste des calques, et l'ajoute si tel n'est pas le cas :
;--- La fonction (Verif_Calque) poss�de 3 arguments
;--- name correspond au nom du calque que l'on souhaite �valuer (ex : "UBS-100-Champ PV" ou "0", ...)
;--- Color correspond � la couleur du calque lors de sa cr�ation (ex : 130 ou "130")
;--- LineType correspond au type de ligne du calque lors de sa cr�ation (ex : "LIMITE1" ou "Continuous", ...)

; Renvoit le nom du calque sp�cifi� en argument
(defun Verif_Calque (name Color LineType)

	(if (not (tblsearch "LAYER" name))
		(command "_LAYER" "N" name "CO" Color name "TL" LineType name "")
	)
	name

)

	; Pour chaque d�finition de bloc pr�sentes dans le dessin courant, s�lectionne l'ensemble des entit�s composant le bloc et modifie leur calque s'il correspond � la recherche :
;--- La fonction (BlockEntity_Layer) poss�de 4 arguments
;--- BlockSearch correspond au nom du/des bloc(s) que l'on souhaite modifier
;--- search correspond au nom de l'ancien calque que l'on souhaite remplacer. Les recherches relatives sont �galement possibles (ex : "UBS-100-Champ PV" ou "*Champ*", ...)
;--- replace correspond au nom du calque rempla�ant. (ex : "UBS-300-Champ PV", ...)
;--- Display d�finit si la fonction doit renvoyer un message r�capitulatif ("prompt") ou non et si le r�cap' s'affiche, la liste des blocs est remise � z�ro

;--- Renvoit la liste de blocs dont une ou plusieurs sous-entit�s a chang� de calque ou un message r�capitulatif sp�cifiant le nombre de sous-entit�s concern�es, les calques anciens trouv�s et la liste des blocs ayant �t� modifi�s
(defun BlockEntity_Layer (BlockSearch search replace Display / Bloc_name_List Bloc_name Bloc_Data EntB EntB_DXF)

	(setq Bloc_name_List (flt_tbl "BLOCK" BlockSearch)
	      Ent_Obj 0
	)
	(if (not Nb_Obj) (setq Nb_Obj 0))
	(if (= Display "prompt") (setq Bloc_List nil Clq_List nil))
	(foreach Bloc_name Bloc_name_List
		(setq Bloc_Data (tblsearch "BLOCK" Bloc_name)
		      EntB (cdr (assoc -2 Bloc_Data))
		      EntB_DXF (entget EntB)
		)
		(if (wcmatch (strcase (setq Old_Clq (cdr (assoc 8 EntB_DXF)))) (strcase search))
			(progn
				(setq EntB_DXF (subst (cons 8 replace) (assoc 8 EntB_DXF) EntB_DXF))
				(entmod EntB_DXF)
				(entupd EntB)
				(setq Nb_Obj (1+ Nb_Obj)
				      Ent_Obj (1+ Ent_Obj)
				      Bloc_List (cons (cdr (assoc 2 Bloc_Data)) Bloc_List)
				      Clq_List (cons Old_Clq Clq_List)
				)
			)
		)
		(while (setq EntB (entnext EntB))
			(setq EntB_DXF (entget EntB))
			(if (wcmatch (strcase (setq Old_Clq (cdr (assoc 8 EntB_DXF)))) (strcase search))
				(progn
					(setq EntB_DXF (subst (cons 8 replace) (assoc 8 EntB_DXF) EntB_DXF))
					(entmod EntB_DXF)
					(entupd EntB)
					(setq Nb_Obj (1+ Nb_Obj)
					      Ent_Obj (1+ Ent_Obj)
					      Bloc_List (cons (cdr (assoc 2 Bloc_Data)) Bloc_List)
					      Clq_List (cons Old_Clq Clq_List)
					)
				)
			)
		)
	)
	(if (= Display "prompt")
		(prompt
			(strcat "\nLa fonction BlockEntity_Layer a trouv� "
				(itoa Ent_Obj)
				" objet(s) sur le(s) calque(s) "
				"\""
				(if Clq_List (DXF_List Clq_List ", " "right" t t) "")
				"\""
				". Le nouveau calque pour chacun de ces objets est d�sormais "
				"\""
				replace
				"\""
				"."
				"\n"
				"\nVoici la liste des blocs ayant �t� modifi� (les blocs dynamiques appara�ssent sous la forme \"*U###\" ou \"*D###\") :"
				(if Bloc_List (DXF_List Bloc_List "\n  - " "left" t nil) "")
				"\n"
			)
		)
		(if (> Ent_Obj 0) t nil)
	)
	
)

	; Permet de r�cup�rer les propri�t�s essentielles et succintes d'un r�seau :
;--- La fonction (Array-Def) poss�de 1 argument
;--- name correspond au nom d'entit� du r�seau �tudi�

;--- Renvoie une liste de paires point�es constitu�es comme suit, sinon nil :
;	("AxesAngle" . 0.0) 		-> l'angle, en radian, entre les axes X et Y du r�seau (par d�faut : 90�)
;	("RowSpacing" . 0.0) 		-> la distance entre chaque rang�e (suivant l'axe Y du SCO)
;	("Rows" . 1)			-> le nombre de rang�es
;	("ColumnSpacing" . 0.0) 	-> la distance entre chaque colonne (suivant l'axe X du SCO)
;	("Columns" . 1)			-> le nombre de colonnes
;	("LevelSpacing" . 0.0)	 	-> la distance entre chaque niveau (suivant l'axe Z du SCO)
;	("Levels" . 1)			-> le nombre de niveaux
;	(90 . 1)			-> le nombre d'objets source
;	(330 . <Nom d'entit�: 483f218b>)-> la liste des entit�s sources (pouvant �tre modifi�es/�tudi�es)
(defun Array-Def (name / i ent Array-Properties Array-Definition Array-ItemList ItemList ent-lst lst n p)

	(if (and (= (cdr (assoc 0 (entget name))) "INSERT")
		 (wcmatch (getpropertyvalue name "Classname") "AcDbAssociative*Array")
	    )
		(progn
			(setq Array-properties (entget (cdr (assoc 330 (entget (cdr (assoc 330 (entget name)))))))
			      Array-ItemList (entget (cdr (assoc 360 Array-Properties)))
			      i (get-DXF-value Array-ItemList 90 4)
			)
			(while (setq Array-ItemList (member '(100 . "AcDbAssocArrayItem") Array-ItemList))
				(setq n (strcat
						(itoa (get-DXF-value Array-ItemList 90 2))
						","
						(itoa (get-DXF-value Array-ItemList 90 3))
						","
						(itoa (get-DXF-value Array-ItemList 90 4))
					)
				      p (cond
						((= (logand (get-DXF-value Array-ItemList 90 5) 1) 1) 0)
						((= (logand (get-DXF-value Array-ItemList 90 5) 8) 8) 1)
					)
				      ItemList (cons (cons n p) ItemList)
				      Array-ItemList (cdr Array-ItemList)
				)
			)
			(setq Array-definition (tblsearch "BLOCK" (cdr (assoc 2 (entget (cdr (assoc -2 (tblsearch "BLOCK" (cdr (assoc 2 (entget name))))))))))
			      ent (cdr (assoc -2 Array-Definition))
			      lst (list	(cons "TotalObject" (apply '+ (mapcar 'cdr ItemList)))
					(cons "ColumnSpacing" (cdr (assoc 40 (member '(1 . "ItemSpacing") Array-Properties))))
					(cons "Columns" (cdr (assoc 90 (cdddr (member '(1 . "Items") Array-Properties)))))
					(cons "RowSpacing" (cdr (assoc 40 (member '(1 . "RowSpacing") Array-Properties))))
					(cons "Rows" (cdr (assoc 90 (cdddr (member '(1 . "Rows") Array-Properties)))))
					(cons "LevelSpacing" (cdr (assoc 40 (member '(1 . "LevelSpacing") Array-Properties))))
					(cons "Levels" (cdr (assoc 90 (cdddr (member '(1 . "Levels") Array-Properties)))))
				  )
			)
			(while ent
				(setq ent-lst (cons ent ent-lst)
				      ent (entnext ent)
				)
			)
			(setq lst (append lst (list (cons 90 (length ent-lst)) (cons 330 ent-lst))))
		)
	)
	lst

)

	;--- LISP de bonusCAD publi� sur CADXP.com le 11/01/2016 11:34

	;--- Version modifi�e de la fonction (ListBox), poss�de 5 arguments
;--- title correspond � l'ent�te de la bo�te de dialogue
;--- msg correspond au message affich� au dessus de la liste
;--- lst correspond � la liste � afficher
;--- value correspond � la valeur d�finie par d�faut
;--- flag correspond au type de liste souhait�e
;	flag = 0  ->  liste d�roulante (choix unique)
;	flag = 1  ->  liste avec barre de d�filement (choix unique)
;	flag = 2  ->  liste avec barre de d�filement (choix multiple)

; Renvoie la liste des calques ayant �t� s�lectionn�s
(defun ListBox (title msg lst value flag / vl-list-search tmp file DCL_ID choice tlst)

  (defun vl-list-search (p l)
    (vl-remove-if-not '(lambda (x) (wcmatch x p)) l)
  )
  (setq tmp (vl-filename-mktemp "tmp.dcl")
        file (open tmp "w")
        tlst lst
  )
  (write-line
    (strcat "ListBox:dialog{width=" (itoa (+ (apply 'max (mapcar 'strlen (mapcar 'vl-princ-to-string lst))) 5)) ";label=\"" title "\";")
    file
  )
  (write-line
    ":edit_box{key=\"filter\";}"
    file
  )
  (if (and msg (/= msg ""))
    (write-line (strcat ":text{label=\"" msg "\";}") file)
  )
  (write-line
    (cond
      ((= 0 flag) "spacer;:popup_list{key=\"lst\";}")
      ((= 1 flag) "spacer;:list_box{height=15;key=\"lst\";}")
      (t "spacer;:list_box{height=15;key=\"lst\";multiple_select=true;}")
    )
    file
  )
  (write-line ":text{key=\"count\";}" file)
  (write-line "spacer;ok_cancel;}" file)
  (close file)
  (setq DCL_ID (load_dialog tmp))
  (if (not (new_dialog "ListBox" DCL_ID))
    (exit)
  )
  (set_tile "filter" "*")
  (set_tile "count" (strcat (itoa (length lst)) " / " (itoa (length lst))))
  (start_list "lst")
  (mapcar 'add_list lst)
  (end_list)
  (set_tile "lst" (if (member value lst) (itoa (vl-position value lst)) (itoa 0)))
  (action_tile
    "filter"
    "(start_list \"lst\")
     (mapcar 'add_list (setq tlst (vl-list-search $value lst)))
     (end_list)
     (set_tile \"count\" (strcat (itoa (length tlst)) \" / \" (itoa (length lst))))"
  )
  (action_tile
    "accept"
    "(or 
      (= (get_tile \"lst\") \"\")
      (if (= 2 flag)
        (progn
          (foreach n (str2lst (get_tile \"lst\") \" \")
            (setq choice (cons (nth (atoi n) tlst) choice))
          )
          (setq choice (reverse choice))
        )
        (setq choice (nth (atoi (get_tile \"lst\")) tlst))
      )
    )
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete tmp)
  choice

)

;--- LISP de bonusCAD publi� sur CADXP.com le 11/01/2016 11:34

;--- N�cessaire au bon fonctionnement de la fonction (ListBox) ci-dessus

(defun str2lst (str sep / pos)
        (if (setq pos (vl-string-search sep str))
                (cons
                        (substr str 1 pos)
                        (str2lst (substr str (+ (strlen sep) pos 1)) sep)
                )
                (list str)
        )
)

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (mapcar 'min (vlax-safearray->list llp) (cond (ls1) ((vlax-safearray->list llp))))
                  ls2 (mapcar 'max (vlax-safearray->list urp) (cond (ls2) ((vlax-safearray->list urp))))
            )
        )
    )
    (if (and ls1 ls2) (list ls1 ls2))
)

	; Permet de r�cup�rer les coordonn�es du point central de l'emprise d'un objet :

(defun Get-MidBoundingBox (jsel / pt)

	(if (setq pt (LM:ssboundingbox jsel))
		(trans (polar (car pt) (angle (car pt) (cadr pt)) (/ (distance (car pt) (cadr pt)) 2.0)) 0 1)
	)

)

	; Permet de dessiner une bulle de r�f�rencement autour d'un jeu de s�lection (cf. LM:ssboundingbox) pointant un point pr�cis :
; Cr�ation le 02/06/2020 v1.0.0
(defun SetBubble (jsel / BdB_Pt Center Ray Bulge Ang Dist Int_1 Int_2 pt)

	(if (and
		(setq BdB_Pt (LM:ssboundingbox jsel))
		(setq Center (trans (polar (car BdB_Pt) (angle (car BdB_Pt) (cadr BdB_Pt)) (setq Ray (/ (distance (car BdB_Pt) (cadr BdB_Pt)) 2.0))) 0 1))
		(setq pt (getpoint Center "\nVeuillez sp�cifier le point de r�f�rencement pour cette s�lection : "))
		(setq Dist (distance Center pt))
		(setq Ang (angle Center pt))
		(setq Int_1 (polar Center (+ Ang (angtof "20.0" 1)) (* 1.1 Ray)))
		(setq Int_2 (polar Center (- Ang (angtof "20.0" 1)) (* 1.1 Ray)))
		(setq Bulge (/ (sin (/ (angtof "320.0" 1) 4.0)) (cos (/ (angtof "320.0" 1) 4.0))))
		(setq Bubble 	(entmakex
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
		(prompt "Succ�s")
		(prompt "Echec...")
	)

)

	; Permet de r�cup�rer la date et l'heure au moment de l'ex�cution sous la forme "XX/XX/XXXX - XX:XX:XX" :
; La fonction (get-date) ne poss�de aucun argument

;--- Renvoie la date et l'heure sous la forme "JJ/MM/AAAA - HH:MM:SS"
(defun Get-Date (/ Date Jour Heure)

	(setq Date (rtos (getvar "CDATE") 2 6)
	      Jour (strcat (substr Date 7 2) "/" (substr Date 5 2) "/" (substr Date 1 4))
	      Heure (strcat (substr Date 10 2) ":" (substr Date 12 2) ":" (substr Date 14 2))
	      Date (strcat Jour " - " Heure)
	)

)

	; Permet de mettre � jour l'attribut "DATE" des cartouches UBS sur les pr�sentations sp�cifi�es et d'apr�s la date sp�cifi�e :
; La fonction (Set-Date) poss�de 2 arguments
;--- laylist correspond � la liste des pr�sentations devant �tre prises en compte pour la red�finition de la date des cartouches (issue d'une fen�tre de s�lection des pr�sentations)
;--- value correspond � la date que l'on souhaite sp�cifier pour la nouvelle valeur, si value ne poss�de pas de format typ� "date" ou nil, la fonction d�finira la date comme �tant la date actuelle

;--- Renvoie nil.
(defun Set-Date (laylist value / jsel i name lst)

	(cond
		((not (listp laylist)) (setq laylist (list laylist)))
	)
	(if (and
	    	value
		(= (strlen (vl-princ-to-string value)) 10)
		(wcmatch (vl-princ-to-string value) "##.##.####")
	    )
		(setq value (vl-princ-to-string value))
		(setq value (substr (get-date) 1 10))
	)
	(if (setq jsel (Select-filter "BLC" "Cartouche*" "_X" (list (cons 410 (DXF_List laylist "," "right" t t)))))
		(progn
			(setq i 0)
			(while (< i (sslength jsel))
				(setq name (ssname jsel i))
				(if (assoc "DATE" (get-att-list name))
					(progn
						(Set-Att-Value name "DATE" value)
						(setq i (1+ i)
						      lst (cons (cdr (assoc 410 (entget name))) lst)
						)
					)
					(ssdel name jsel)
				)
			)
			(prompt (strcat "\nModification de la date (\""
					value
					"\") de "
					(itoa (sslength jsel))
					" cartouche(s)."
					"\nListe des pr�sentations : \""
					(DXF_List lst "\", \"" "right" t t)
					"\"."
				)
			)
		)
		(prompt (strcat "\nImpossible de mettre � jour la date sur la/les pr�sentation(s) \""
				(DXF_List (vl-remove-if '(lambda (x) (member x lst)) laylist) "\", \"" "right" t t)
				"\" car aucun cartouche n'a �t� trouv�."
			)
		)
	)
	(sssetfirst nil nil)
	(princ)

)

(defun get-init-layout-pos (/ i tabs lay-list lay Init_Pos-list)

	(vl-load-com)
	(setq i 0
	      tabs (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
	      lay-list (layoutlist)
	)
	(foreach lay (reverse lay-list)
		(setq Init_Pos-list (cons (cons lay (vla-get-taborder (vla-item tabs lay))) Init_Pos-list))
	)
	Init_Pos-list

)

(defun set-layout-pos (/ i d tabs Init_Pos lay-list lay lay-move)

	(vl-load-com)
	(setq i 0
	      d 0
	      tabs (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
	      lay-list (layoutlist)
	      Init_Pos (get-init-layout-pos)
	)
	(foreach lay lay-list
		(vla-put-taborder (vla-item tabs lay) (setq i (1+ i)))
		(if (/= (cdr (assoc lay Init_Pos)) i)
			(setq d (1+ d)
			      lay-move (cons lay lay-move)
			)
		)
	)
	(prompt (strcat "\nUn total de "
			(itoa (length lay-list))
			" pr�sentations ont �t� prises en compte."
			"\nSur les "
			(itoa (length lay-list))
			", "
			(itoa (length lay-move))
			" pr�sentations ont chang� de position."
			"\nVoici la liste des pr�sentations ayant �t� d�plac� :"
			(DXF_List lay-move "\"\n  - \"" "left" t nil)
			"\n"
		)
	)

)

	; Permet de r�cup�rer la liste des calques gel�s ou libres d'une fen�tre de pr�sentation :
;--- La fonction (VP-layer-list) poss�de 2 arguments
;--- name correspond au nom d'entit� de la fen�tre de pr�sentation
;--- flag correspond au choix de la liste devant �tre retourn�e
;	flag = 0, retourne la liste des calques gel�s
;	flag = 1, retourne la liste des calques non gel�s

;--- Renvoie la liste des calques gel�s ou non en fonction du flag choisi
(defun VP-layer-list (name flag / layer layer-list)

	(if (= (cdr (assoc 0 (entget name))) "VIEWPORT")
		(progn
			(setq layer (entget name))
			(while (setq layer (member (assoc 331 layer) layer))
				(setq layer-list (cons (cdr (assoc 2 (entget (cdr (assoc 331 layer))))) layer-list)
				      layer (cdr layer)
				)
			)
			(cond
				((= flag 0)
					(DXF_List layer-list nil nil t nil)
				)
				((= flag 1)
					(DXF_List (vl-remove-if '(lambda (x) (member x layer-list)) (flt_tbl "LAYER" "*")) nil nil t nil)
				)
				(t (prompt "\nErreur : M�thode sp�cifi�e invalide..."))
			)
		)
	)

)


;; ============================================================= DEFINITION DES ROUTINES LISP ============================================================= ;;





	; Apr�s s�lection d'une fen�tre de pr�sentation, purge l'ensemble des calque et objets gel�s dans la fen�tre et ne conserve que la pr�sentation s�lectionn�e :
;--- Cette commande a la possibilit� d'�tre annul�e en refusant la derni�re entr�e utilisateur ou via l'option "Retour" de la commande ANNULER (apr�s acceptation du r�sultat).
;--- Aucune entr�e utilisateur ne peut �tre d�finie sur une valeur par d�faut en raison de la "brutalit�" de la commande

(defun c:VP-RADPURGE (/ *error* doc TmpDCL file t0 line DCL ListLay LayoutTmp LayerTable LayerList Layer Layout jsel name i freeze CTAB Echo chx modlist acet)

	(defun *error* (msg)

		(setvar "CMDECHO" Echo)
		(if acet (acet-ui-progress))
		(if LayerList (command-s "_UNDO" "R"))
		(princ msg)

	)
  
	(setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
	      LayerTable (vla-get-layers doc)
	      Echo (getvar "CMDECHO")
	)
	(if (= (getvar "CTAB") "Model")
		(setvar "CTAB" (ListBox "VP-RADPURGE : S�lection de la pr�sentation source"
					     "La commande VP-RADPURGE ne peut pas �tre fonctionnelle depuis l'espace objet, veuillez s�lectionner la pr�sentation � rendre active :"
					     (DXF_List (vl-remove "TOOLKIT" (layoutlist)) nil nil t nil)
					     nil
					     0
				)
		)
	)
	(command "_PSPACE")
	(prompt "\nVeuillez s�lectionner une fen�tre de pr�sentation : ")
	(while (null (setq jsel (ssget '((0 . "VIEWPORT")))))
		(prompt "\nVeuillez s�lectionner une fen�tre de pr�sentation : ")
	)
	(repeat (setq i (sslength jsel))
		(setq name (ssname jsel (setq i (1- i)))
		      LayerList (append LayerList (VP-layer-list name 0))
		)
	)
	(setq t0 (* 84600 (getvar "TDUSRTIMER"))
	      LayerList (DXF_List LayerList nil nil t nil)
	      jsel (ssget "_X" (list (cons 410 (setq CTAB (getvar "CTAB")))))
	      i 0
	)
	(while (< i (sslength jsel))
		(setq name (ssname jsel i)
		      LayerList (vl-remove (cdr (assoc 8 (entget name))) LayerList)
		      i (1+ i)
		)
	)
	(prompt (strcat "\nLa commande que vous �tes sur le point de lancer va supprimer la totalit� des calques gel�s dans la fen�tre s�lectionn�e (ainsi que les objets pr�sents sur ces calques)."
			"\nVeuillez vous assurer que les calques visibles sur le dessin actuel sont les seuls que vous souhaitez conserver."
		)
	)
	(initget 1 "Oui Non")
	(if (= (getkword "\nSouhaitez-vous continuer [Oui/Non] ? ") "Oui")
		(progn
			(setvar "CMDECHO" 0)
			(setvar "CTAB" "Model")
			(command-s "_UNDO" "M")
			(foreach layer (setq LayerList (vl-remove-if '(lambda (x) (wcmatch x "*|*")) LayerList))
				(vla-put-lock (vlax-ename->vla-object (tblobjname "LAYER" layer)) :vlax-false)
			)
			(setq i 0)
			(setq acet (acet-ui-progress "VP-RADPURGE en cours d'ex�cution :" (length LayerList)))
			(command "-SUPCALQUE")
			(foreach layer LayerList
				(command "N" layer)
				(acet-ui-progress (setq i (1+ i)))
			)
			(command "" "O")
			(setq acet (acet-ui-progress))
			(setq i 0)
			(repeat (1- (length (divlist (DXF_List LayerList nil nil t nil) (fix-div (length LayerList) 50))))
				(alert (strcat "Voici la liste des calques ayant �t� purg� de ce dessin :"
					       (DXF_List (nth i (divlist (DXF_List LayerList nil nil t nil) (fix-div (length LayerList) 50))) "\n  - " "left" t nil)
					       "\n"
					       "\n( page "
					       (itoa (setq i (1+ i)))
					       " / "
					       (itoa (length (divlist (DXF_List LayerList nil nil t nil) (fix-div (length LayerList) 50))))
					       " )"
					)
				)
			)
			(alert (strcat "Voici la liste des calques ayant �t� purg� de ce dessin :"
				       (DXF_List (nth i (divlist (DXF_List LayerList nil nil t nil) (fix-div (length LayerList) 50))) "\n  - " "left" t nil)
				       "\n"
				       "\nLa commande a mis "
				       (rtos (- (* 84600 (getvar "TDUSRTIMER")) t0) 2 0)
				       " secondes pour supprimer l'ensemble des objets."
				       "\n"
				       "\n( page "
				       (itoa (setq i (1+ i)))
				       " / "
				       (itoa (length (divlist (DXF_List LayerList nil nil t nil) (fix-div (length LayerList) 50))))
				       " )"
				)
			)
			(setq layout-list (ListBox "VP-RADPURGE : S�lection des pr�sentation � conserver"
						   "En continuant, seule(s) la/les pr�sentation(s) s�lectionn�e(s) sont/seront conserv�e(s) :"
						   (DXF_List (vl-remove "TOOLKIT" (layoutlist)) nil nil t nil)
						   CTAB
						   2
					  )
			)
			(initget 1 "Oui Non")
			(if (= (getkword "\nLe dessin vous semble-t-il coh�rent [Oui/Non] ? ") "Non")
				(progn
					(command-s "_UNDO" "R")
					(prompt "\nLa commande VP-RADPURGE a �t� annul�e.")
				)
				(progn
					(foreach Layout (vl-remove-if '(lambda (x) (member x layout-list)) (layoutlist))
						(command-s "_-LAYOUT" "EF" Layout)
					)
					(command-s "_-PURGE" "TO" "*" "N")
					(prompt (strcat "Voici la liste des calques ayant �t� purg� de ce dessin :"
							(DXF_List LayerList "\n  - " "left" t nil)
						)
					)
				)
			)
			(setvar "CMDECHO" 1)
		)
		(prompt "\nLa commande VP-RADPURGE a �t� annul�e.")
	)
	(princ)

)


	; Renomme la pr�sentation en fonction des donn�es param�tr�es dans le cartouche de la pr�sentation :
;--- Cette commande permet de r�cup�rer le code projet et la phase � partir du nom du fichier DWG de la forme Cxxxx_x_x_* et l'indice projet, la planche et le titre du plan dans le cartouche
;--- On peut soit renommer la pr�sentation active, soit l'ensemble des pr�sentations
;--- On peut soit les renommer de mani�re d�taill�e "Cxxxx_x_xxxx-xx_*" soit de mani�re simplifi�e "xxxx.xx"

(defun c:NAMECART (/ Choix name jsel i Code_Projet Vis_Phase Phase Ind_Projet Planche_Projet Titre_Projet layout-list layout acadDoc acadDocSummaryInfo DWG_name Explode_name Name_List DWG_Prop_Code Name)

	(setq DWG_name (getvar "DWGNAME")
	      Phase (substr DWG_name (+ 2 (vl-string-position (ascii "_") DWG_name)) 1)
	      Code_Projet (substr DWG_name 1 5)
	)
	(cond
		((= Phase "S") (setq Vis_Phase "ESQ"))
		((= Phase "A") (setq Vis_Phase "APS"))
		((= Phase "E") (setq Vis_Phase "APD"))
		((= Phase "C") (setq Vis_Phase "DCE"))
		((= Phase "P") (setq Vis_Phase "PRO"))
		((= Phase "X") (setq Vis_Phase "EXE"))
		((= Phase "D") (setq Vis_Phase "DOE"))
		(Phase (setq Vis_Phase "Masquer"))
	)
	(initget "Active Toutes S�lection")
		(if (null (setq Choix (getkword "\nQuelles pr�sentations souhaitez-vous renommer [Active/Toutes/S�lection] <Active> ? ")))
			(setq Choix "Active")
		)
		(cond
			((= Choix "Active")
				(if (and (setq jsel (Select-filter "BLC" "Cartouche*" "_X" (list (cons 410 (getvar "CTAB"))))) (= (sslength jsel) 1))
					(progn
						(sssetfirst nil nil)
						(setq name (ssname jsel 0))
						(setq Name_List name
						      Att_List (Get-att-list name)
						      Ind_Projet (vl-string-right-trim " -" (cdr (assoc "N�_DESSIN" Att_List)))
						      Planche_Projet (cdr (assoc "PLANCHE" Att_List))
						      Titre_Projet (cdr (assoc "TITRE_2" Att_List))
						)
						(if (null (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name (strcat "AcDbDynBlockProperty" "Visibilit�1") Vis_Phase))))
							(setpropertyvalue name (strcat "AcDbDynBlockProperty" "Visibilit�1") Vis_Phase)
						)
						(if (or 
							(= Code_Projet "[...]")
							(= Vis_Phase "Masquer")
							(= Ind_Projet "")
							(= Titre_Projet "")
							(vl-position t (mapcar '(lambda (x) (wcmatch (strcase Titre_Projet) (strcase x))) '("*`,*" "*`?*" "*`;*" "*`/*" "*`:*" "*`\*" "*`**" "*`=*" "*``*")))
						    )
							(alert (strcat "Le programme a �chou� lorsqu'il a renomm� la pr�sentation."
								       "\nCela peut provenir des �l�ments suivants :"
								       "\n -  Indice du plan non renseign�"
								       "\n -  Phase non renseign�e ou mal positionn�e (7�me caract�re) dans le nom du fichier"
								       "\n -  Titre principal comportant des caract�res non autoris�s"
								       "\n     (ex : \",\" \"?\" \";\" \"/\" \":\" \"\\\" \"*\" \"=\" \"`\" )"
								       "\n"
								       "\nVeuillez corriger ces informations avant de relancer la commande si une erreur provient bien de l'un de ces �l�ments, merci."
								)
							)
							(progn
								(initget "D�taill� Simplifi�")
								(if (null (setq Type_name (getkword "\nComment souhaitez-vous nommer la pr�sentation [D�taill�/Simplifi�] <D�taill�> ?  ")))
									(setq Type_name "D�taill�")
								)
								(cond
									((or (and (= Type_name "D�taill�") (= (getvar "CTAB") (strcat Code_Projet "_" Phase "_" Ind_Projet "-" Planche_Projet "_" Titre_Projet)))
									     (and (= Type_name "Simplifi�") (= (getvar "CTAB") (strcat Ind_Projet "-" Planche_Projet)))
									 )
										(prompt (strcat "\nLa pr�sentation \"" (getvar "CTAB") "\" poss�de d�j� la bonne d�nomination."))
									)
									((= Type_name "D�taill�") (Set-layout-name (getvar "CTAB") (strcat Code_Projet "_" Phase "_" Ind_Projet "-" Planche_Projet "_" Titre_Projet)))
									((= Type_name "Simplifi�") (Set-layout-name (getvar "CTAB") (strcat Ind_Projet "-" Planche_Projet)))
								)
							)
						)
					)
					(cond
						((or (= jsel nil) (= (sslength jsel) 0)) (alert (strcat "Aucun cartouche n'a �t� trouv� sur la pr�sentation " (getvar "CTAB") ".")))
						((> (sslength jsel) 1) (alert (strcat "Plusieurs cartouches se trouvent sur la pr�sentation " (getvar "CTAB") ".")))
					)
				)
			)
			((member Choix '("Toutes" "S�lection"))
				(progn
					(cond
						((= Choix "Toutes")
							(setq layout-list (DXF_List (vl-remove-if '(lambda (x) (member (strcase x) '("TOOLKIT" "TRAVAIL"))) (layoutlist)) nil nil t nil))
						)
						((= Choix "S�lection")
							(setq layout-list (ListBox "NAMECART : S�lection des pr�sentations"
										   "Veuillez d�finir la ou les pr�sentation(s) � renommer :"
										   (DXF_List (vl-remove-if '(lambda (x) (member (strcase x) '("TOOLKIT" "TRAVAIL"))) (layoutlist)) nil nil t nil)
										   (getvar "CTAB")
										   2
									  )
							)
						)
					)
					(initget "D�taill� Simplifi�")
					(if (null (setq Type_name (getkword "\nComment souhaitez-vous nommer les pr�sentations [D�taill�/Simplifi�] <D�taill�> ?  ")))
						(setq Type_name "D�taill�")
					)
					(foreach layout layout-list
						(if (and (setq jsel (Select-filter "BLC" "Cartouche*" "_X" (list (cons 410 layout)))) (= (sslength jsel) 1))
							(progn
								(sssetfirst nil nil)
								(setq name (ssname jsel 0))
								(setq Name_List (cons name Name_List)
								      Att_List (Get-att-list name)
								      Ind_Projet (vl-string-right-trim " -" (cdr (assoc "N�_DESSIN" Att_List)))
								      Planche_Projet (cdr (assoc "PLANCHE" Att_List))
								      Titre_Projet (cdr (assoc "TITRE_2" Att_List))
								)
								(if (null (vl-catch-all-error-p (vl-catch-all-apply 'setpropertyvalue (list name (strcat "AcDbDynBlockProperty" "Visibilit�1") Vis_Phase))))
									(setpropertyvalue name (strcat "AcDbDynBlockProperty" "Visibilit�1") Vis_Phase)
								)
								(if (or (= Code_Projet "[...]")
									(= Vis_Phase "Masquer")
									(= Ind_Projet "")
									(= Titre_Projet "")
									(vl-position t (mapcar '(lambda (x) (wcmatch (strcase Titre_Projet) (strcase x))) '("*`,*" "*`?*" "*`;*" "*`/*" "*`:*" "*`\*" "*`**" "*`=*" "*``*")))
								    )
									(alert (strcat "Le programme a �chou� lorsqu'il a renomm� la pr�sentation \""
										       layout
										       "\"."
										       "\nCela peut provenir des �l�ments suivants :"
										       "\n -  Indice du plan non renseign�"
										       "\n -  Phase non renseign�e ou mal positionn�e (7�me caract�re) dans le nom du fichier"
										       "\n -  Titre principal comportant des caract�res non autoris�s"
										       "\n     (ex : \",\" \"?\" \";\" \"/\" \":\" \"\\\" \"*\" \"=\" \"`\" )"
										       "\n"
										       "\nVeuillez corriger ces informations avant de relancer la commande si une erreur provient bien de l'un de ces �l�ments, merci."
										)
									)
									(cond
										((or (and (= Type_name "D�taill�") (= layout (strcat Code_Projet "_" Phase "_" Ind_Projet "-" Planche_Projet "_" Titre_Projet)))
										     (and (= Type_name "Simplifi�") (= layout (strcat Ind_Projet "-" Planche_Projet)))
										 )
											(prompt (strcat "\nLa pr�sentation \"" layout "\" poss�de d�j� la bonne d�nomination."))
										)
										((= Type_name "D�taill�") (Set-layout-name layout (strcat Code_Projet "_" Phase "_" Ind_Projet "-" Planche_Projet "_" Titre_Projet)))
										((= Type_name "Simplifi�") (Set-layout-name layout (strcat Ind_Projet "-" Planche_Projet)))
									)
								)
							)
							(cond
								((or (= jsel nil) (= (sslength jsel) 0)) (alert (strcat "Aucun cartouche n'a �t� trouv� sur la pr�sentation \"" layout "\".")))
								((> (sslength jsel) 1) (alert (strcat "Plusieurs cartouches se trouvent sur la pr�sentation \"" layout "\".")))
							)
						)
					)
				)
			)
		)
	(set-layout-pos)
	(princ)

)


	; Filtrage des calques dans les fen�tres de pr�sentation (except�es les Vues a�riennes si elles n'ont pas boug�es d'apr�s le toolkit) d'apr�s un fichier EXCEL pour harmonisation des rendus des plans :

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
	(setq VP_Ind (vl-string-right-trim " -" (cdr (assoc "N�_DESSIN" Att_List))))
	(setq List_Proj (list "*Toiture*" "*CS*" "*Ombri�re*" "*Serre*" "*Syn.*"))
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
	(setq VP_Sel (ssget "_X" (list '(0 . "VIEWPORT") '(8 . "UBS-900-Fen�tre de pr�sentation") (cons 410 (getvar "CTAB")))))
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
		(alert (strcat "\nVoici la liste des calques UBS ayant �t� conserv�s au cours de la commande :\n" List_Flt))
		(alert "Le programme a �chou� lors de la s�lection des fen�tres de pr�sentation ...")
	)

)


	; G�le la totalit� des calques except� "UBS-100-Champ PV" et "UBS-900-Vue a�rienne" dans les fen�tres s�lectionn�es :

(defun c:FNTAERIENNE (/ VP_Sel i)

	(if (setq VP_Sel (ssget "_:L" (list '(0 . "VIEWPORT") '(8 . "UBS-900-Fen�tre de pr�sentation"))))
		(progn
			(sssetfirst nil VP_Sel)
			(cond
				((> (sslength VP_Sel) 1)
					(progn
						(setq i 0)
						(while (< i (sslength VP_Sel))
							(command "_-VPORTS" "A" (ssname VP_Sel i) "")
							(command "_MSPACE")
							(command "_VPLAYER" "G" "*" "C" "L" "UBS-100-Champ PV*,UBS-900-Vue a�rienne*" "C" "")
							(if (= (getvar "WORLDUCS") 0)
								(progn
									(command "_UCS" "")
									(command "_PLAN" "G")
								)
							)
							(command "_PSPACE")
							(setq i (1+ i))
						)
					)
				)
				((= (sslength VP_Sel) 1)
					(progn
						(command "_-VPORTS" "A" (ssname VP_Sel 0) "")
						(command "_MSPACE")
						(command "_VPLAYER" "G" "*" "C" "L" "UBS-100-Champ PV*,UBS-900-Vue a�rienne*" "C" "")
						(if (= (getvar "WORLDUCS") 0)
							(progn
								(command "_UCS" "")
								(command "_PLAN" "G")
							)
						)
						(command "_PSPACE")
					)
				)
			)
		)
		((= VP_Sel nil) (alert (strcat "\nAucune fen�tre n'a �t� s�lectionn�e..."
					       "\nVoici une liste des probl�mes pouvant �tre en causes :"
					       "\n  - Les objets s�lectionn�s ne sont pas des fen�tres"
					       "\n  - La fen�tre ne se trouve pas sur le calque \"UBS-900-Fen�tre de pr�sentation\""
					)
				)
		)
	)
	(alert (strcat "Le programme a d�fini les calques suivants dans " (if VP_Sel (itoa (sslength VP_Sel)) 0) " fen�tres de la pr�sentation :\n  - UBS-100-Champ PV\n  - UBS-900-Vue a�rienne"))
	(sssetfirst)

)


	; Calcul la longueur cumul�e de polylignes s�lectionn�es manuellement ou selon leur calque :

(defun C:LongCumul (/ Chx Method jsel i name Layer-list Unit Lg-Ent Lg-Arc Lg-Line Lg-LwPline Lg-Pline Lg-Spline Lg-Total Lg-Moy pt-list Coeff)

	(while (/= Chx "Oui")
		(progn
			(initget "Calques Objets")
			(if (null (setq Method (getkword "\nVeuillez d�finir la m�thode de calcul [Calques/Objets] <Objets> : ")))
				(setq Method "Objets")
			)
		)
		(cond
			((= Method "Calques")
				(if (setq Layer-list (ListBox "LONGCUMUL : Fen�tre de s�lection des calques"
							      "Veuillez choisir le(s) calque(s) :"
							      (DXF_List (vl-remove-if '(lambda (x) (wcmatch x "*|*")) (flt_tbl "LAYER" "*")) nil nil t nil)
							      (getvar "CLAYER")
							      2
						     )
				    )
					(progn
						(setq jsel (Select-filter "DXF" nil "_X" (list '(0 . "*LINE,ARC") (cons 8 (DXF_List layer-list "," "right" t t)) '(410 . "Model")))
						      i 0
						)
						(while (and jsel (< i (sslength jsel)))
							(setq name (ssname jsel i)
							      i (1+ i)
							      Lg-ent (vlax-curve-getDistAtParam (vlax-ename->vla-object name) (vlax-curve-getEndParam (vlax-ename->vla-object name)))
							)
							(cond
								((= (cdr (assoc 0 (entget name))) "ARC")
									(setq Lg-Arc (cons (cons Lg-Ent name) Lg-Arc))
								)
								((= (cdr (assoc 0 (entget name))) "LINE")
									(setq Lg-Line (cons (cons Lg-Ent name) Lg-Line))
								)
								((= (cdr (assoc 0 (entget name))) "LWPOLYLINE")
									(setq Lg-LwPline (cons (cons Lg-Ent name) Lg-LwPline))
								)
								((= (cdr (assoc 0 (entget name))) "POLYLINE")
									(setq Lg-Pline (cons (cons Lg-Ent name) Lg-Pline))
								)
								((= (cdr (assoc 0 (entget name))) "SPLINE")
									(setq Lg-Spline (cons (cons Lg-Ent name) Lg-Spline))
								)
							)
						)
					)
					(exit)
				)
			)
			((= Method "Objets")
				(setq jsel (Select-filter "DXF" nil pause '((0 . "*LINE,ARC")))
				      i 0
				)
				(while (and jsel (< i (sslength jsel)))
					(setq name (ssname jsel i)
					      i (1+ i)
					      Lg-ent (vlax-curve-getDistAtParam (vlax-ename->vla-object name) (vlax-curve-getEndParam (vlax-ename->vla-object name)))
					      Layer-list (if (not (member (cdr (assoc 8 (entget name))) Layer-list)) (cons (cdr (assoc 8 (entget name))) Layer-list) Layer-list)
					)
					(cond
						((= (cdr (assoc 0 (entget name))) "ARC")
							(setq Lg-Arc (cons (cons Lg-Ent name) Lg-Arc))
						)
						((= (cdr (assoc 0 (entget name))) "LINE")
							(setq Lg-Line (cons (cons Lg-Ent name) Lg-Line))
						)
						((= (cdr (assoc 0 (entget name))) "LWPOLYLINE")
							(setq Lg-LwPline (cons (cons Lg-Ent name) Lg-LwPline))
						)
						((= (cdr (assoc 0 (entget name))) "POLYLINE")
							(setq Lg-Pline (cons (cons Lg-Ent name) Lg-Pline))
						)
						((= (cdr (assoc 0 (entget name))) "SPLINE")
							(setq Lg-Spline (cons (cons Lg-Ent name) Lg-Spline))
						)
					)
				)
			)
			(t
				(prompt "\nLa m�thode choisie n'existe pas...")
				(exit)
			)
		)
		(setq pt-list (LM:ssboundingbox jsel))
		(vla-zoomwindow (vlax-get-acad-object) (vlax-3d-point (car pt-list)) (vlax-3d-point (last pt-list)))
		(progn
			(initget "Non")
			(if (or (null (setq Coeff (getreal "\nAppliquer un coefficient aux longueurs �tudi�es <Non> = "))) (= Coeff "Non"))
				(setq Coeff 1.00)
			)
		)
		(setq Lg-Arc (mapcar '(lambda (x) (cons (* (car x) Coeff) (cdr x))) Lg-Arc)
		      Lg-Line (mapcar '(lambda (x) (cons (* (car x) Coeff) (cdr x))) Lg-Line)
		      Lg-LwPline (mapcar '(lambda (x) (cons (* (car x) Coeff) (cdr x))) Lg-LwPline)
		      Lg-Pline (mapcar '(lambda (x) (cons (* (car x) Coeff) (cdr x))) Lg-Pline)
		      Lg-Spline (mapcar '(lambda (x) (cons (* (car x) Coeff) (cdr x))) Lg-Spline)
		      Lg-Total (append Lg-Arc Lg-Line Lg-LwPline Lg-Pline Lg-Spline)
		      Layer-list (DXF_List Layer-list "\", \"" "right" t t)
		)
		(cond
			((= (getvar "INSUNITS") 0)
				(setq Unit "")
			)
			((= (getvar "INSUNITS") 1)
				(setq Unit "\"")
			)
			((= (getvar "INSUNITS") 2)
				(setq Unit "'")
			)
			((= (getvar "INSUNITS") 4)
				(setq Unit " mm")
			)
			((= (getvar "INSUNITS") 5)
				(setq Unit " cm")
			)
			((= (getvar "INSUNITS") 6)
				(setq Unit " m")
			)
			((= (getvar "INSUNITS") 7)
				(setq Unit " km")
			)
		)
		(if (null Lg-Total)
			(alert (strcat "La commande n'a pas r�ussi � calculer de longueur avec les donn�es d'entr�es..."
				       "\n"
				       (cond
						((= Method "Calques") "\nIl est possible que le(s) calque(s) sp�cifi�(s) ne contient(nent) aucuns objets poss�dant une propri�t� de longueur.")
						((= Method "Objets") "\nIl est possible que le(s) objet(s) sp�cifi�(s) ne poss�de(nt) pas de propri�t� de longueur.")
				       )
				)
			)
		)
		(if (=  2
			(acet-ui-message
				(strcat "\nListe des calques : " (if Layer-list (strcat "\"" Layer-list "\"") "") "."
					(if Lg-Arc
						(strcat	"\n  * Arc (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Arc)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
							"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Arc)) 2 2) Unit
							"\n	- Nombre d'objets = " (itoa (length Lg-Arc)) " u"
							"\n0======================================0"
							"\n"
						)
						""
					)
					(if Lg-Line
						(strcat	"\n  * Ligne (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Line)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
							"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Line)) 2 2) Unit
							"\n	- Nombre d'objets = " (itoa (length Lg-Line)) " u"
							"\n0======================================0"
							"\n"
						)
						""
					)
					(if Lg-LwPline
						(strcat	"\n  * Polyligne (" (rtos (* (/ (apply '+ (mapcar 'car Lg-LwPline)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
							"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-LwPline)) 2 2) Unit
							"\n	- Nombre d'objets = " (itoa (length Lg-LwPline)) " u"
							"\n0======================================0"
							"\n"
						)
						""
					)
					(if Lg-Pline
						(strcat	"\n  * Polyligne2D/3D (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Pline)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
							"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Pline)) 2 2) Unit
							"\n	- Nombre d'objets = " (itoa (length Lg-Pline)) " u"
							"\n0======================================0"
							"\n"
						)
						""
					)
					(if Lg-Spline
						(strcat	"\n  * Spline (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Spline)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
							"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Spline)) 2 2) Unit
							"\n	- Nombre d'objets = " (itoa (length Lg-Spline)) " u"
							"\n0======================================0"
							"\n"
						)
						""
					)
					(if (or Lg-Arc Lg-Line Lg-LwPline Lg-Pline Lg-Spline)
						(strcat	"\n  * TOTAL : "
							"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Total)) 2 2) Unit
							"\n	- Nombre d'objets = " (itoa (length Lg-Total)) " u"
							"\n	- Longueur max = " (rtos (apply 'max (mapcar 'car Lg-Total)) 2 2) Unit
							"\n	- Longueur min = " (rtos (apply 'min (mapcar 'car Lg-Total)) 2 2) Unit
							"\n	- Longueur moy. = " (rtos (setq Lg-Moy (/ (apply '+ (mapcar 'car Lg-Total)) (atof (itoa (length Lg-Total))))) 2 2) Unit
							"\n	- Ecart moyen = " (rtos (/ (apply '+ (mapcar '(lambda (x) (abs (- x Lg-Moy))) (mapcar 'car Lg-Total))) (atof (itoa (length Lg-Total)))) 2 2) Unit
							"\n	- Coefficient = " (rtos Coeff 2 2)
							"\n"
						)
						""
					)
				)
				"R�sultat de la commande LONGCUMUL"
				1
			)
		    )
			(progn
				(setq Chx "Non"
				      Method nil
				      jsel nil
				      i nil
				      name nil
				      Layer-List nil
				      Unit nil
				      Lg-Ent nil
				      Lg-Arc nil
				      Lg-Line nil
				      Lg-LwPline nil
				      Lg-Pline nil
				      Lg-Spline nil
				      Lg-Total nil
				      Lg-Moy nil
				      pt-list nil
				      coeff nil
				)
				(sssetfirst nil nil)
			)
			(progn
				(setq Chx "Oui")
				(prompt (strcat "\nR�sultats de la command LONGCUMUL : "
						"\nListe des calques : " (if Layer-list (strcat "\"" Layer-list "\"") "") "."
						(if Lg-Arc
							(strcat	"\n  * Arc (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Arc)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
								"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Arc)) 2 2) Unit
								"\n	- Nombre d'objets = " (itoa (length Lg-Arc)) " u"
								"\n0=================================0"
								"\n"
							)
							""
						)
						(if Lg-Line
							(strcat	"\n  * Ligne (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Line)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
								"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Line)) 2 2) Unit
								"\n	- Nombre d'objets = " (itoa (length Lg-Line)) " u"
								"\n0=================================0"
								"\n"
							)
							""
						)
						(if Lg-LwPline
							(strcat	"\n  * Polyligne (" (rtos (* (/ (apply '+ (mapcar 'car Lg-LwPline)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
								"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-LwPline)) 2 2) Unit
								"\n	- Nombre d'objets = " (itoa (length Lg-LwPline)) " u"
								"\n0=================================0"
								"\n"
							)
							""
						)
						(if Lg-Pline
							(strcat	"\n  * Polyligne2D/3D (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Pline)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
								"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Pline)) 2 2) Unit
								"\n	- Nombre d'objets = " (itoa (length Lg-Pline)) " u"
								"\n0=================================0"
								"\n"
							)
							""
						)
						(if Lg-Spline
							(strcat	"\n  * Spline (" (rtos (* (/ (apply '+ (mapcar 'car Lg-Spline)) (apply '+ (mapcar 'car Lg-Total))) 100) 2 2) " %) : "
								"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Spline)) 2 2) Unit
								"\n	- Nombre d'objets = " (itoa (length Lg-Spline)) " u"
								"\n0=================================0"
								"\n"
							)
							""
						)
						(if (or Lg-Arc Lg-Line Lg-LwPline Lg-Pline Lg-Spline)
							(strcat	"\n  * TOTAL : "
								"\n	- Longueur totale = " (rtos (apply '+ (mapcar 'car Lg-Total)) 2 2) Unit
								"\n	- Nombre d'objets = " (itoa (length Lg-Total)) " u"
								"\n	- Longueur max	  = " (rtos (apply 'max (mapcar 'car Lg-Total)) 2 2) Unit
								"\n	- Longueur min	  = " (rtos (apply 'min (mapcar 'car Lg-Total)) 2 2) Unit
								"\n	- Longueur moy.   = " (rtos (setq Lg-Moy (/ (apply '+ (mapcar 'car Lg-Total)) (atof (itoa (length Lg-Total))))) 2 2) Unit
								"\n	- Ecart moyen     = " (rtos (/ (apply '+ (mapcar '(lambda (x) (abs (- x Lg-Moy))) (mapcar 'car Lg-Total))) (atof (itoa (length Lg-Total)))) 2 2) Unit
								"\n	- Coefficient     = " (rtos Coeff 2 2)
								"\n"
							)
							""
						)
					)
				)
			)
		)
		(sssetfirst nil nil)
	)
	(princ)

)


	; Permet de modifier la date d'un ou plusieurs cartouche(s) s�lectionn�s via une BdD des diff�rentes pr�sentation du dessin et en sp�cifiant la date souhait�e (avec prise en compte de r�ponse rapide) :

(defun C:DATECART (/ layout date)

	(if (setq layout (ListBox "DATECART : S�lection des pr�sentations" "Veuillez choisir les pr�sentations devant changer de date :" (layoutlist) (getvar "CTAB") 2))
		(progn
			(if (= "" (setq date (getstring (strcat "\nVeuillez d�finir la date souhait�e (ENTER pour \"" (substr (get-date) 1 10) "\") :"))))
				(setq date nil)
			)
			(set-date layout date)
		)
		(exit)
	)

)


	; Permet de modifier les attributs "Modification_*" et "Auteur_*" en les incr�mentant de 1 indice :

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

	(if (setq laylist (ListBox "Commande MODCART : S�lection des pr�sentations"
				   "Veuillez s�lectionner les pr�sentations � modifier :"
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
					(prompt (strcat "\nLe cartouche \"" (cdr (assoc 410 (entget name))) "\" est d�j� � l'indice A."))
				)
			)
			(set-date laylist nil)
			(prompt (strcat "\n"
					(itoa (sslength jsel))
					" cartouches pr�sents sur les pr�sentations \""
					(DXF_List lst "\", \"" "right" t t)
					"\" ont �t� red�fini � l'indice A : \"CREATION DU PLAN\"."
				)
			)
		)
		((and
			(= Mod "+1")
			jsel
			(> (sslength jsel) 0)
			(setq Mod (strcase (getstring t (strcat "\nVeuillez sp�cifier la modification � afficher dans " (itoa (sslength jsel)) " cartouche(s) : "))))
		 )
			(if (= "" (setq Auteur (getstring (strcat "\nVeuillez sp�cifier l'auteur pour cette modification <" (vlax-get-property (vlax-get-property (vla-get-activedocument (vlax-get-acad-object)) 'SummaryInfo) 'Author) "> : "))))
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
					" cartouches pr�sents sur les pr�sentations \""
					(DXF_List (mapcar '(lambda (x) (strcat (car x) " (" (cdr x) ")")) lst) "\", \"" "right" t t)
					"\" ont �t� modifi�s."
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
			(setq Mod (ListBox "Commande MODCART : S�lection des modifications � retirer"
					   (strcat "Veuillez s�lectionner les modifications � retirer dans " (itoa (sslength jsel)) " cartouche(s) :")
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
					" cartouches pr�sents sur les pr�sentations \""
					(DXF_List laylist "\", \"" "right" t t)
					"\" ont �t� modifi�s."
				)
			)
		)
		((or (null laylist)
		     (null jsel)
		     (= (sslength jsel) 0)
		 )
			(prompt "\nErreur: Commande MODCART annul�e...")
		)
		(prompt (strcat "\nAucune modification apport�e pour le(s) "
				(itoa (sslength jsel))
				" cartouche(s) pr�sents sur la/les pr�sentation(s) \""
				(DXF_List laylist "\", \"" "right" t t)
				"\"..."
			)
		)
	)
	(princ)

)


	; Permet de mettre � jour le gestionnaire de calques du dessin courant en fonction d'une liste de calques jug�s obsol�tes (liste exhaustive) sur le fichier EXCEL "Toolkit - Filtres.xlsm" et de la nouvelle liste de calques :

(defun c:MAJCALQUE (/ *error* Type_Projet Old_List New_List New_Color_List New_LineType_List Old_name New_name New_color New_LineType Obj_Sel i Nb_Obj Nb_Rename Nb_Old Replace_Obj Layout_List layout Init_layout Calque_List Color_List LineType_List Excel_List)

  	(setq OldEcho (getvar "CMDECHO"))
	(setq Init_layout (getvar "CTAB"))
	(setvar "CMDECHO" 0)

	(defun *error* (msg)

		(if (/= msg "Fonction annul�e")
			(princ (strcat "\nErreur : " msg))
		)
		(setvar "CMDECHO" OldEcho)
		(setvar "CTAB" Init_Layout)

	)

	(initget 1 "Toiture CS Ombri�re Serre Synoptique")
	(setq Type_Projet (getkword "\nVeuillez pr�ciser le type de projet : [Toiture/CS/Ombri�re/Serre/Synoptique]  "))
	(setq Old_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Calques - " "Anciens") "U" 2 "U" 3))
	(setq New_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Calques - " "Remplacement") "U" 2 "U" 3))
	(setq New_Color_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Couleur - " "Remplacement") "U" 2 "U" 3))
	(setq New_Linetype_List (ExcelReader_List "Toolkit - Filtres.xlsm" "Liste des Calques" "Tab_V" (strcat "Type Ligne - " "Remplacement") "U" 2 "U" 3))
	(if (open (findfile "UBS 2019 - acadiso.lin") "R")
		(command "_-LINETYPE" "CH" "*" "UBS 2019 - acadiso.lin" (while (= (getvar "CMDACTIVE") 1) (command "")))
		(prompt "\nLe fichier \"UBS 2019 - acadiso.lin\" n'a pas �t� trouv�.\nRisque d'erreur plus important !!!")
	)
	(Verif_Calque "UBS-900-Calques obsol�tes" 7 "Continuous")
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
										(if (= New_name "UBS-900-Calques obsol�tes")
											(setq Replace_Obj (+ Replace_Obj (sslength Obj_Sel)))
										)
										(command "_CHPROP" Obj_Sel "" "CA" New_name "")
										(command "_LAYER" "CO" New_Color New_name "TL" New_LineType New_name "")
									)
								)
							)
							(BlockEntity_Layer "*" Old_name New_name "")
							(if (and (= New_name "UBS-900-Calques obsol�tes") (> Ent_Obj 0))
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
	(command "-CALQUE" "T" "A" (DXF_List '("UBS-100-Limites de prox. onduleurs" "UBS-900-Calques obsol�tes" "UBS-900-Fen�tre de Pr�sentation" "UBS-900-Non imprimable") "," "right" t t) "")
	(foreach layer New_Calque_List
		(if (not (member layer Calque_List))
			(setq UndeadLayer (cons layer UndeadLayer))
		)
	)
	(setvar "CTAB" Init_layout)
	(setvar "CMDECHO" OldEcho)
	(alert
		(strcat "Le programme MAJCALQUE a trouv� "
		       	(itoa Nb_Old)
		       	" calques obsol�tes."
		       	"\nAu cours de cette op�ration, "
		       	(itoa Nb_Rename)
		       	" anciens calques ont �t� renomm� et "
		       	(itoa Nb_Obj)
		       	" objets ont chang� de calque de mani�re forc�e."
		       	"\n"
		       	"\nParmi ces objets, "
		       	(itoa Replace_Obj)
		       	" ont �t� plac� sur le calque \"UBS-900-Calques obsol�tes\" en raison de la purge des calques sur lesquels ils se trouvaient."
			"\n"
		       	"\nVoici la liste compl�te des calques ayant �t� jug�s obsol�tes par le programme :"
			"\n"
		       	(DXF_List Supp_List "\n  - " "left" t nil)
			"\n"
		)
	)
	(alert
		(strcat "Le programme MAJCALQUE a �galement trouv� "
			(itoa (length UndeadLayer))
			" calques n'appartenant ni � la liste des anciens calques jug�s obsol�tes, ni � la liste des nouveaux calques."
			"\nCes calques n'ont pas �t� supprim� au cours de la commande car il peut s'agir de calques de travail en raison de l'�chec de leur PURGE."
			"\n"
			"\nVoici la liste des calques suppl�mentaires conserv�s :"
			"\n"
			(DXF_List UndeadLayer "\n  - " "left" t nil)
			"\n"
			"\nA savoir que seul les calques d�marrant par \"UBS\" sont pris en compte dans cette liste."
		)
	)
	(prompt
		(strcat "\nLe programme MAJCALQUE a trouv� "
				(itoa Nb_Old)
		       	" calques obsol�tes."
		       	"\nAu cours de cette op�ration, "
		       	(itoa Nb_Rename)
		       	" anciens calques ont �t� renomm� et "
		       	(itoa Nb_Obj)
		       	" objets ont chang� de calque de mani�re forc�e."
		       	"\n"
		       	"\nParmi ces objets, "
		       	(itoa Replace_Obj)
		       	" ont �t� plac� sur le calque \"UBS-900-Calques obsol�tes\" en raison de la purge des calques sur lesquels ils se trouvaient."
		       	"\nVoici la liste compl�te des calques ayant �t� jug�s obsol�tes par le programme :"
		       	(DXF_List Supp_List "\n  - " "left" t nil)
		       	"\n"
			"\nLe programme MAJCALQUE a �galement trouv� "
			(itoa (length UndeadLayer))
			" calques n'appartenant ni � la liste des anciens calques jug�s obsol�tes, ni � la liste des nouveaux calques."
			"\nCes calques n'ont pas �t� supprim� au cours de la commande car il peut s'agir de calques de travail en raison de l'�chec de leur PURGE."
			"\n"
			"\nVoici la liste des calques suppl�mentaires conserv�s :"
			"\n"
			(DXF_List UndeadLayer "\n  - " "left" t nil)
			"\n"
			"\nA savoir que seul les calques d�marrant par \"UBS\" sont pris en compte dans cette liste."
		)
	)

)

	; Version custom de la commande AIRE native d'autoCAD permettant de calculer des sommes et des soustractions d'aires avec une s�lection libre et la possibilit� d'annuler toute s�lection faite :
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

	(prompt "\nCe programme est actuellement non fonctionnel avec la s�lection de hachures en raison de son mode de fonctionnement.")
	(while (/= Option "Quitter")
		(initget "Ajouter Soustraire annUler Quitter")
		(if (null (setq Option (getkword "\nVeuillez choisir la m�thode de calcul de l'aire [Ajouter/Soustraire/annUler/Quitter] <Quitter> : ")))
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
					(if (null (setq Method (getpoint "\nAddition -> Veuillez sp�cifier un point interne ou [Objets/annUler/Quitter] <Quitter> :  ")))
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
								(prompt "\nImpossible d'annuler une nouvelle fois car vous �tes revenu au point de d�part.\nVeuillez de nouveau s�lectionner des objets ou choisir un point interne, merci.\n")
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
					(prompt (strcat "\nR�sultat de la m�thode \"Addition\", s�lection n�"
							(itoa s)
							" : "
							"\nAire pr�c�dente = "
							(rtos (if (> (length Area_List) 1) (nth 1 Area_List) 0) 2 2)
							" m� pour un total de "
							(itoa (if (> (length Undo_Obj) 1) (nth 1 Undo_Obj) 0))
							" objet(s)."
							"\nAire actuelle = "
							(rtos (car Area_List) 2 2)
							" m� pour un total de "
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
					(if (null (setq Method (getpoint "\nSoustraction -> Veuillez sp�cifier un point interne ou [Objets/annUler/Quitter] <Quitter> :  ")))
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
								(prompt "\nImpossible d'annuler une nouvelle fois car vous �tes revenu au point de d�part.\nVeuillez de nouveau s�lectionner des objets ou choisir un point interne, merci.\n")
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
					(prompt (strcat "\nR�sultat de la m�thode \"Soustraction\", s�lection n�"
							(itoa s)
							" : "
							"\nAire pr�c�dente = "
							(rtos (if (> (length Area_List) 1) (nth 1 Area_List) 0) 2 2)
							" m� pour un total de "
							(itoa (if (> (length Undo_Obj) 1) (nth 1 Undo_Obj) 0))
							" objet(s)."
							"\nAire actuelle = "
							(rtos (car Area_List) 2 2)
							" m� pour un total de "
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
						(prompt "\nImpossible d'annuler une nouvelle fois car vous �tes revenu au point de d�part.\nVeuillez de nouveau choisir une m�thode de calcul, merci.\n")
					)
				)
				(setq m (if (null (cdr m)) (list 0) (cdr m)))
				(prompt (strcat "\nR�sultat de la m�thode \"Annulation m�thode\", s�lection n�"
						(itoa s)
						" : "
						"\nAire pr�c�dente = "
						(rtos (if (> (length Area_List) 1) (nth 1 Area_List) 0) 2 2)
						" m� pour un total de "
						(itoa (if (> (length Undo_Obj) 1) (nth 1 Undo_Obj) 0))
						" objet(s)."
						"\nAire actuelle = "
						(rtos (car Area_List) 2 2)
						" m� pour un total de "
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
			" m� pour un ensemble de "
			(itoa Nb_Obj)
			" objet(s) apr�s "
			(itoa s)
			" manipulation(s)."
			"\n"
		)
	)					      

)

(defun c:RPCustom (/ h ce)

	(setvar "CMDECHO" 0)
	(setq h (getvar "VIEWSIZE"))
	(setq ce (trans (getvar "VIEWCTR") 1 0))
	(command "_PLAN" "")
	(command "_ZOOM" "C" (trans ce 0 1) h)
	(princ)

)


	; Permet de supprimer des sommets de polylignes appartenant � un jeu de s�lection :
(defun C:POLYDELPOINT (/ *error* msg osmode jsel i name n d p pt pt-list)


	(defun *error* (msg)
		(setvar "OSMODE" osmode)
		(princ msg)
	)

	(if (not (setq jsel (last (ssgetfirst))))
		(setq jsel (ssget '((0 . "ARC,LINE,LWPOLYLINE,POLYLINE"))))
		(setq jsel (ssget "_I" '((0 . "ARC,LINE,LWPOLYLINE,POLYLINE"))))
	)
	(setq osmode (getvar "OSMODE"))
	(setvar "OSMODE" 1)
	(sssetfirst nil jsel)
	(if jsel
		(while (and (> (sslength jsel) 0)
			    (null (prompt (strcat "\n" (itoa (sslength jsel)) " objet(s) actuellement s�lectionn�s")))
			    (setq pt (getpoint "\nS�lectionner un point � supprimer (ENTER pour terminer) : "))
		       )
			(setq i 0
			      n 0
			      d 0
			)
			(while (< i (sslength jsel))
				(setq name (ssname jsel i)
				      pt-list (get-pt-list name)
				      p (if (< (length (car pt-list)) (length (setq p (trans pt 1 0)))) (reverse (cdr (reverse p))) p)
				      i (1+ i)
				)
				(if (pt-member p pt-list 0.01)
					(progn
						(cond
							((member (cons 10 p) (entget name))
								(entmod (vl-remove (cons 10 p) (entget name)))
							)
							((member (cons 11 p) (entget name))
								(entmod (vl-remove (cons 11 p) (entget name)))
							)
						)
						(if (or (wcmatch (cdr (assoc 0 (entget name))) "LINE,ARC")
							(<= (length (get-pt-list name)) 1)
						    )
							(progn
								(ssdel name jsel)
								(entdel name)
								(setq d (1+ d)
								      i (1- i)
								)
							)
						)
						(setq n (1+ n))
					)
				)
			)
			(sssetfirst nil jsel)
			(prompt (strcat "\nLe sommet sp�cifi� appartenait � "
					(itoa n)
					" / "
					(itoa (+ d (sslength jsel)))
					" entit�(s) lin�aire(s)."
					(if (/= d 0)
						(strcat " Parmi ces "
							(itoa n)
							" entit�(s), "
							(itoa d)
							" ont �t� supprim� du dessin."
						)
						""
					)
					"\n"
				)
			)
		)
		(prompt "\nAucun jeu de s�lection n'a �t� trouv� apr�s application du filtre.")
	)
	(sssetfirst nil nil)
	(setvar "OSMODE" osmode)
	(princ)

)


	; Permet d'ajouter des sommets � des polylignes appartenant � un jeu de s�lection :
(defun C:POLYADDPOINT (/ jsel i name n pt Add-pt)

	(if (not (setq jsel (last (ssgetfirst))))
		(setq jsel (ssget '((0 . "LWPOLYLINE"))))
		(setq jsel (ssget "_I" '((0 . "LWPOLYLINE"))))
	)
	(sssetfirst nil jsel)
	(if jsel
		(while (setq pt (getpoint "\nS�lectionner le point pr�c�dant le nouveau point � ajouter (ENTER pour terminer) : "))
			(setvar "LASTPOINT" pt)
			(setq i 0
			      n 0
			      Add-pt (getpoint "\nSp�cifier un nouveau point : ")
			)
			(while (< i (sslength jsel))
				(setq name (ssname jsel i)
				      i (1+ i)
				)
				(if (Add-Poly2D-Point name (trans pt 1 0) (trans Add-pt 1 0))
					(setq n (1+ n))
				)
			)
			(prompt (strcat "\nLe sommet sp�cifi� appartient d�sormais � "
					(itoa n)
					" / "
					(itoa (sslength jsel))
					" polyligne(s)."
					"\n"
				)
			)
		)
		(prompt "\nAucun jeu de s�lection n'a �t� trouv� apr�s application du filtre.")
	)
	(sssetfirst nil nil)
	(princ)

)


	; Permet d'inverser le sens d'une ou plusieurs "LWPOLYLINE" en inversant l'ordre des sommets (attention dans le cas de largeur non globale) :
(defun c:InvPol (/ jsel i name pt-list pt)

	(if (setq jsel (ssget '((0 . "LWPOLYLINE"))))
		(progn
			(repeat (setq i (sslength jsel))
				(setq name (ssname jsel (setq i (1- i)))
				      pt-list (get-pt-list name)
				      pt 0
				)
				(entmod
					(mapcar
						'(lambda (x)
							(if (= 10 (car x))
								(cons 10 (nth (- (length pt-list) (setq pt (1+ pt))) pt-list))
								x
							)
						)
						(entget name)
					)
				)
			)
			(prompt (strcat "\nUn total de "
					(itoa (sslength jsel))
					" polylignes ont �t� trait�."
				)
			)
		)
		(prompt "\nAucune polyligne s�lectionn�e...")
	)
	(princ)

)


(defun c:SETCOLOR (/ Color doc LayerTable ClqName Layer)

	(prompt "\nCette commande permet de changer la couleur d'un calque en s�lectionnant des objets")
	(setq Color (acad_colordlg 256)
	      doc (vla-get-activedocument (vlax-get-acad-object))
	      LayerTable (vla-get-layers doc)
	)
	(while (setq ClqName (car (nentsel "\nVeuillez s�lectionner un objet pour modifier les propri�t�s du calque : ")))
		(setq ClqName (cdr (assoc 8 (entget ClqName)))
		      Layer (vla-item LayerTable ClqName)
		)
		(if (not (= ClqName "0"))
			(progn
				(vla-put-color Layer Color)
				(command "_REGEN")
				(prompt (strcat "\nLe calque \""
						ClqName
						"\" a d�sormais la couleur "
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

(defun C:PSLTSCALECUSTOM (/ *error* InitLayout layout-list acet lay Value Nb)

	(defun *error* (msg)

		(if acet
			(setq acet (acet-ui-progress))
		)
		(princ)

	)

	(setq InitLayout (getvar "CTAB")
	      Nb 0
	)
	(initget "0 1")
	(if (or (null (setq Value (getint "\nQuelle valeur souhaitez-vous affecter � la variable syst�me \"PSLTSCALE\" [0/1] <0> : "))) (and (/= Value 0) (/= Value 1)))
		(setq Value 0)
	)
	(setq layout-list (ListBox "PSLTSCALECUSTOM : S�lection des pr�sentations"
				   "Veuillez s�lectionner la/les pr�sentation(s) : "
				   (layoutlist)
				   InitLayout
				   2
			  )
	      acet (acet-ui-progress "PSLTSCALECUSTOM en cours d'ex�cution :" (length layout-list))
	      i 0
	)
	(foreach lay layout-list
		(setvar "CTAB" lay)
		(if (= (getvar "PSLTSCALE") Value)
			(setq Nb (1+ Nb))
		)
		(acet-ui-progress (setq i (1+ i)))
		(setvar "PSLTSCALE" Value)
	)
	(setq acet (acet-ui-progress))
	(setvar "CTAB" InitLayout)
	(prompt (strcat "\nLa variable syst�me \"PSLTSCALE\" est d�sormais d�finie � "
			(itoa Value)
			" sur la totalit� des pr�sentations du fichier \""
			(getvar "DWGNAME")
			"\"."
			"\n"
			(itoa Nb)
			" / "
			(itoa (length layout-list))
			" pr�sentation(s) poss�daient d�j� la variable \"PSLTSCALE\" � "
			(itoa Value)
			".\n"
		)
	)

)

(defun C:UBS-PVCASE-Entraxe (/ jsel name i lst n)

	(if (setq jsel (ssget "_X" '((0 . "MTEXT") (8 . "PVcase Rows Numbers"))))
		(progn
			(repeat (setq i (sslength jsel))
				(setq name (ssname jsel (setq i (1- i))))
				(if (wcmatch (cdr (assoc 1 (entget name))) "*.*")
					(setq lst (cons (cdr (assoc 1 (entget name))) lst))
					(setq n (cons (cdr (assoc 1 (entget name))) lst))
				)
			)
			(alert (strcat "Nombre de rang�es = "
					(itoa (apply 'max (mapcar 'atoi n)))
					" u"
					"\nEntraxe max = "
					(rtos (apply 'max (mapcar 'atof lst)) 2 2)
					" m"
					"\nEntraxe min = "
					(rtos (apply 'min (mapcar 'atof lst)) 2 2)
					" m"
					"\nEntraxe moyen = "
					(rtos (/ (apply '+ (mapcar 'atof lst)) (atof (rtos (length lst) 2 1))) 2 2)
					" m"
				)
			)
		)
		(prompt "\nAucun objet s�lectionn�...\n")
	)

)

	; Permet de r�-assigner la valeur inscrite dans l'attribut "ALT" dans la position Z du bloc. Cela permet de corriger les plan topo ayant �t� remis � plat par inadvertance :

(defun c:ALTPOINT (/ jsel i name blc n alt mat pt lst)

	(cond
		((or (null (progn (initget "S�lectionner Auto") (setq jsel (getkword "\nSouhaitez-vous utiliser la m�thode automatique pour s�lectionner les blocs du nom de \"TCPOINT\" ou s�lectionner le bloc [Auto/S�lectionner] <Auto> : "))))
		     (= jsel "Auto")
		 )
			(setq jsel (select-filter "BLC" "TCPOINT*" "_X" '((410 . "Model")))
			      blc "TCPOINT*"
			)
		)
		((= jsel "S�lectionner")
			(setq jsel (select-filter "BLC" (setq blc (strcat (caadar (get-pattern-list (list (getpropertyvalue (ssname (progn (while (null (setq jsel (ssget "_:S+." '((0 . "INSERT"))))) (ssget "_:S+." '((0 . "INSERT")))) jsel) 0) "BlockTableRecord/Name")))) "*")) "_X" '((410 . "Model"))))
		)
		(t
			(setq jsel nil
			      blc ""
			)
		)
	)
	(sssetfirst nil nil)
	(if (setq n 0
		  jsel jsel
	    )
		(progn
			(repeat (setq i (sslength jsel))
				(setq name (ssname jsel (setq i (1- i))))
				(if (and (null (vl-catch-all-error-p (setq mat (vl-catch-all-apply 'getpropertyvalue (list name "MAT")))))
					 (null (vl-catch-all-error-p (setq alt (vl-catch-all-apply 'getpropertyvalue (list name "ALT")))))
					 (setq pt (cdr (assoc 10 (entget name)))
					       pt (list (car pt) (cadr pt) (atof alt))
					 )
					 (entmod (subst (cons 10 pt) (assoc 10 (entget name)) (entget name)))
				    )
					(setq n (1+ n))
				)
				(if (or mat (setq mat "")) (setq lst (cons (cons mat (last (cdr (assoc 10 (entget name))))) lst)))
			)
			(setq lst (vl-sort lst '(lambda (a1 a2) (< (cdr a1) (cdr a2))))
			      i (atoi (rtos (* (length lst) 0.05) 2 0))
			)
			(sssetfirst nil (ssget "_X" (list '(0 . "INSERT") (cons 2 blc) '(-4 . "<OR") '(-4 . "*,*,=") (cons 10 (list 0.0 0.0 (cdr (last lst)))) '(-4 . "*,*,=") (cons 10 (list 0.0 0.0 (cdar lst))) '(-4 . "OR>"))))
			(alert (strcat "\nUn total de "
					(itoa n)
					" / "
					(itoa (sslength jsel))
					" bloc du nom de \""
					blc
					"\" ont �t� red�finis."
					"\nVoici un aper�u des r�sultats :"
					"\n  - Altitude maximale = "
					(rtos (cdr (last lst)) 2 2)
					" m (MAT : "
					(DXF_List (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cdr x) (cdr (last lst)))) lst)) ", " "right" t t)
					")"
					"\n  - Altitude minimale = "
					(rtos (cdar lst) 2 2)
					" m (MAT : "
					(DXF_List (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cdr x) (cdar lst))) lst)) ", " "right" t t)
					")"
					"\n  - Altitude moyenne = "
					(rtos (/ (apply '+ (mapcar 'cdr lst)) (atof (itoa (length lst)))) 2 2)
					" m"
				)
			)
			(prompt (strcat "\nUn total de "
					(itoa n)
					" / "
					(itoa (sslength jsel))
					" bloc du nom de \""
					blc
					"\" ont �t� red�finis."
					"\nVoici un aper�u des r�sultats :"
					"\n  - Altitude maximale = "
					(rtos (cdr (last lst)) 2 2)
					" m (MAT : "
					(DXF_List (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cdr x) (cdr (last lst)))) lst)) ", " "right" t t)
					")"
					"\n  - Altitude minimale = "
					(rtos (cdar lst) 2 2)
					" m (MAT : "
					(DXF_List (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cdr x) (cdar lst))) lst)) ", " "right" t t)
					")"
					"\n  - Altitude moyenne = "
					(rtos (/ (apply '+ (mapcar 'cdr lst)) (atof (itoa (length lst)))) 2 2)
					" m"
					"\nListe des altitudes inf�rieures � 5% :\n"
					(vl-string-right-trim ", " (apply 'strcat (mapcar '(lambda (x) (strcat (rtos (cdr x) 2 2) " m (MAT : " (car x) "), ")) (sublist lst 1 i))))
					"\n"
					"\nListe des altitudes sup�rieures � 95% :\n"
					(vl-string-right-trim ", " (apply 'strcat (mapcar '(lambda (x) (strcat (rtos (cdr x) 2 2) " m (MAT : " (car x) "), ")) (sublist lst (- (length lst) i) nil))))
					"\n"
				)
			)					
		)
		(prompt (strcat "\nAucun bloc du nom de \"" blc "\" existe dans ce fichier..."))
	)

)

	; Permet de compter les objets pr�sents dans les r�seaux s�lectionn�s sans avoir � les exploser :

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
								"\nR�seau n�"
								(itoa n)
								(cond ((wcmatch (getpropertyvalue name "Classname") "AcDbAssociativeRectangularArray") " (Rectangulaire)")
								      ((wcmatch (getpropertyvalue name "Classname") "AcDbAssociativePolarArray") " (Polaire)")
								      ((wcmatch (getpropertyvalue name "Classname") "AcDbAssociativePathArray") " (Trajectoire)")
								      (t "")
								)
								" :"
								"\n     Nombre d'objets total = "
								(itoa (cdr (assoc "TotalObject" Array))) "u / " (itoa (* r c l)) "u"
								"\n     Nombre de rang�es = "
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
								"\n  Composition de la source du r�seau :"
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
					"\nSynth�se des "
					(itoa n)
					" r�seaux s�lectionn�s :"
					"\n     Nombre d'objets total = "
					(itoa (apply '+ (mapcar 'car o-list))) " u"
					"\n     Nombre d'objets global (avec sources) = "
					(itoa (apply '+ (mapcar '(lambda (x) (apply '* x)) o-list))) " u"
					"\nSe r�f�rer au d�tail de chaque r�seau pour la r�partition des objets et des objets sources."
				)
			)
		)
	)
	(princ)		 

)
(defun c:PLINE3Dto2D (/ *error* acet jsel i name n pt-list lst)
  (defun *error* (msg)
    (if acet (acet-ui-progress))
    (princ msg)
  )
  (if (setq jsel (ssget '((0 . "POLYLINE"))))
    (progn
      (repeat
        (setq
          n (sslength jsel)
          acet (acet-ui-progress "PLINE3Dto2D en cours d'ex�cution :" n)
          i n
        )
        (setq
          name (ssname jsel (setq i (1- i)))
          pt-list (get-pt-list name)
          ent-lst
            (vl-remove
              nil
              (append
                (list
                  '(0 . "LWPOLYLINE")
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbPolyline")
                  (assoc 8 (entget name))
                  (cons 90 (length pt-list))
                  (cons 38 (/ (apply '+ (mapcar 'caddr pt-list)) (float (length pt-list))))
                  (assoc 70 (entget name))
                  (assoc 62 (entget name))
                )
                (mapcar '(lambda (p) (cons 10 p)) pt-list)
              )
            )
        )
        (if (entmake ent-lst)
          (progn
            (ssdel name jsel)
            (entdel name)
          )
        )
        (acet-ui-progress (- n i))
      )
      (setq acet (acet-ui-progress))
      (princ
        (strcat
          "\nUn total de "
          (itoa (- n (sslength jsel)))
          " / "
          (itoa n)
          " polylignes 3D ont �t� trait�es avec succ�s."
        )
      )
    )
  )
  (princ)
)

(defun c:GETLAYER (/ name str)

	(while	(and
			(setq name (car (nentsel "\nVeuillez s�lectionner un objet : ")))
			(princ	(substr (setq str (cdr (assoc 8 (entget name))))
					(if (wcmatch str "*|*")
						(+ 2 (vl-string-position (ascii "|") str))
						1
					)
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
				(prompt "\nVeuillez s�lectionner les r�seaux � modifier : ")
				(setq jsel (ssget '((0 . "INSERT") (2 . "`*U*"))))
			)
		)
		(setq layer (ListBox "S�lection du calque"
				     "Veuillez sp�cifier le nom du calque de remplacement :"
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
					" objets sont des r�seaux."
					"\nLe calque de remplacement choisi est : \""
					layer
					"\". Un total de "
					(itoa n)
					" / "
					(itoa tt)
					" objets ont chang� de calque."
				)
			)
		)
		(prompt (strcat (if (null jsel) "\nAucun bloc dynamique n'a �t� s�lectionn�...")
				(if (null layer) "\nAucun calque de remplacement n'a �t� sp�cifi�...")
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

(defun c:MID_MOVE (/ jsel BdB_Pt Center)

	(command
		"_MOVE"
		(setq jsel (ssget))
		""
		(Get-MidBoundingBox jsel)
		pause
	)
	(princ)

)

(defun c:TILEMODESWITCH ()

	(setvar "TILEMODE" (- 1 (getvar "TILEMODE")))
	(princ)

)

;--- Auteur :		Luna
;--- Date de cr�ation :	30/09/2021
;--- Version :		v1.0.0
;--- Date :		30/09/2021
(defun c:PVcaseDELETE ()

	(if (setq entlist (dictsearch (namedobjdict) "PVcase"))
		(entdel (cdr (assoc -1 entlist)))
	)
	(command "_-PURGE" "CA" "PVcase*" "N")
	(prompt "\nApplication PVcase supprim�e avec succ�s !")
	(princ)

)

;--- Auteur :		Luna
;--- Date de cr�ation :	10/12/2021
;--- Version :		v1.0.0
;--- Date :		10/12/2021
(defun c:LCtoFC (/ color# ent-s ent-t layer color62 color420 color430 jsel i)

	(defun color# (color key name)

		(cond
			( (and color
			       (assoc key (entget name))
			  )
				(entmod (subst color (assoc key (entget name)) (entget name)))
			)
			( (and color
			       (not (assoc key (entget name)))
			  )
				(entmod (append (entget name) (list color)))
			)
			( (and (not color)
			       (assoc key (entget name))
			  )
				(entmod (vl-remove (assoc key (entget name)) (entget name)))
			)
		)

	)

	(while (and
		(setq ent-s (car (entsel "\nS�lectionner l'objet source : ")))
		(if (not (assoc 62 (entget ent-s)))
			(setq layer (entget (tblobjname "LAYER" (cdr (assoc 8 (entget ent-s)))))
			      color420 (assoc 420 layer)
			      color430 (assoc 430 layer)
			      color62 (assoc 62 layer)
			)
			(setq color420 (assoc 420 (entget ent-s))
			      color430 (assoc 430 (entget ent-s))
			      color62 (assoc 62 (entget ent-s))
			)
		)
	       )
		(if (progn
		    	(princ "\nS�lectionner le(s) objet(s) cible(s) : ")
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