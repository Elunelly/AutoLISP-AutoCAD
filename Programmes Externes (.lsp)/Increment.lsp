;|
Ces commandes sont toutes bas�es sur la routine Insuff qui permet d'incr�menter les caract�res num�riques et/ou alphab�tiques � la fin d'une cha�ne.
INCTXT et INCATT ins�rent respectivement un texte ou un bloc avec attribut dont une partie est incr�ment�e.
INSEL et INCSUF incr�mentent respectivement une partie ou la fin des textes/attributs successivement s�lectionn�s � fonction du premier s�lectionn�.
INCADD ajoute au d�but ou � la fin des textes/attributs s�lectionn�s une cha�ne contenant une valeur incr�ment�e.
INCR permet de choisir parmi ces commandes.

Revision : 11/05/2008
|;

;;=================== SOUS-ROUTINES ===================;;

;; INCSUFF (version 1.2) -Gilles Chanteau- 15/01/08
;; Incr�mente le suffixe d'une cha�ne de caract�res de la valeur sp�cifi�e.
;; Est pris en compte comme suffixe, l'ensemble des caract�res [0-9] et/ou [A-Z]
;; et/ou [a-z] depuis la fin du texte, en fonction de la valeur de alpha
;;
;; Arguments
;; str : une cha�ne avec un suffixe valide
;; inc : un entier positif
;; alpha : un entier, somme des codes binaires suivant
;; 1 pour les caract�res [0-9]
;; 2 pour les caract�res [A-Z]
;; 4 pour les caract�res [a-z]
;;
;; Retour
;; la cha�ne avec son suffixe incr�ment� (ou nil si aucun suffixe valide)
;;
;; Exemples :
;; (incsuff "N�002" 12 1) = "N�014"
;; (incsuff "Dessin9" 1 1) = "Dessin10"
;; (incsuff "test_ZZ9" 1 3) = "test_AAA0"
;; (incsuff "test_ZZ9" 1 1) = "test_ZZ10"
;; (incsuff "12-" 1 1) = nil
;;
;; Modification (13/02/08) : codes binaires pour l'argument alpha

(defun incsuff (str inc alpha / lst crt pas ind val quo ret)
  (setq lst (reverse (vl-string->list str)))
  (while
    (and
      (setq crt (car lst))
      (cond
	((and (< 47 crt 58) (= 1 (logand 1 alpha)))
	 (setq pas 10
	       ind 48
	 )
	)
	((and (< 64 crt 91) (= 2 (logand 2 alpha)))
	 (setq pas 26
	       ind 65
	 )
	)
	((and (< 96 crt 123) (= 4 (logand 4 alpha)))
	 (setq pas 26
	       ind 97
	 )
	)
	((< 0 quo)
	 (setq crt (if (= 10 pas)
		     ind
		     (1- ind)
		   )
	       lst (cons (car lst) lst)
	 )
	)
      )
    )
     (setq val (- crt ind)
	   quo (/ (+ val inc) pas)
	   ret (cons (+ ind (rem (+ val inc) pas)) ret)
     )
     (if (zerop quo)
       (setq ret (append (reverse (cdr lst)) ret)
	     lst nil
       )
       (if (cdr lst)
	 (setq lst (cdr lst)
	       inc quo
	 )
	 (setq lst (list ind)
	       inc (if (= 10 pas)
		     quo
		     (1- quo)
		   )
	 )
       )
     )
  )
  (if ret
    (vl-list->string ret)
  )
)

;;==================================================;;

;; IncValue
;; Incr�mente un valeur dans les textes ou attributs s�lectionn�s
;;
;; Arguments
;; pref : pr�fixe (string)
;; val : valeur � incr�menter (string)
;; suff : suffixe (string)
;; inc : valeur d'incr�ment (entier, positif)
;; bin : type de caract�res � incr�menter (entier, code binaire)
;; typ : type d'entit� (string, valeur du code de groupe DXF 0)
;; tag : Etiquette d'attribut (string)
;; save : liste de sauvegarde des anciennes valeurs de texte (liste d'association)

(defun incvalue	(pref val suff inc bin typ save prop / ent elst)
  (while (or (initget 1 "annUler")
	     (setq ent (nentsel
			 (strcat "\nS�lectionnez le texte suivant"
				 (if save
				   " ou [annUler]: "
				   ":"
				 )
			 )
		       )
	     )
	 )
    (if	(= ent "annUler")
      (if save
	(progn
	  (setq elst (car save))
	  (entmod elst)
	  (and (= typ "ATTRIB") (entupd (cdr (assoc 330 elst))))
	  (setq	val  (incsuff val (- inc) bin)
		save (cdr save)
	  )
	)
	(princ "\nPlus rien � annuler.")
      )
      (and (setq elst (entget (car ent)))
	   (= (cdr (assoc 0 elst)) typ)
	   (setq save (cons elst save))
	   (setq val (incsuff val inc bin))
	   (if prop
	     (entmod (append (vl-remove-if
			       (function
				 (lambda (x)
				   (member (car x) '(1 8 62 40 7))
				 )
			       )
			       elst
			     )
			     (if (null (assoc 62 prop))
			       '((62 . 256))
			     )
			     (cons (cons 1 (strcat pref val suff)) prop)
		     )
	     )
	     (entmod
	       (subst (cons 1 (strcat pref val suff)) (assoc 1 elst) elst)
	     )
	   )
	   (and (= typ "ATTRIB") (entupd (cdr (assoc 330 elst))))
      )
    )
  )
)

;;==================================================;;

;;; Getblock (gile) 03/11/07
;;; Retourne le nom du bloc entr� ou choisi par l'utilisateur 
;;; dans une liste d�roulante de la boite de dialogue ou depuis la boite
;;; de dialogue standard d'AutoCAD
;;; Argument : le titre (string) ou nil (d�faut : "Choisir un bloc")

(defun getblock	(titre / bloc n lst tmp file what_next dcl_id nom ent)
  (while (setq bloc (tblnext "BLOCK" (not bloc)))
    (setq lst (cons (cdr (assoc 2 bloc)) lst)
    )
  )
  (setq	lst  (acad_strlsort
	       (vl-remove-if
		 (function (lambda (n) (= (substr n 1 1) "*")))
		 lst
	       )
	     )
	tmp  (vl-filename-mktemp "Tmp.dcl")
	file (open tmp "w")
  )
  (write-line
    (strcat
      "getblock:dialog{label="
      (cond (titre (vl-prin1-to-string titre))
	    ("\"Choisir un bloc\"")
      )
      ";initial_focus=\"bl\";:boxed_column{
      :row{:text{label=\"S�lectionner\";alignment=left;}
      :button{label=\">>\";key=\"sel\";alignment=right;fixed_width=true;}}
      spacer;
      :column{:button{label=\"Parcourir...\";key=\"wbl\";alignment=right;fixed_width=true;}}
      :column{:text{label=\"Nom :\";alignment=left;}}
      :edit_box{key=\"tp\";edit_width=25;}
      :popup_list{key=\"bl\";edit_width=25;}spacer;}
      spacer;
      ok_cancel;}"
    )
    file
  )
  (close file)
  (setq dcl_id (load_dialog tmp))
  (setq what_next 2)
  (while (>= what_next 2)
    (if	(not (new_dialog "getblock" dcl_id))
      (exit)
    )
    (start_list "bl")
    (mapcar 'add_list lst)
    (end_list)
    (if	(setq n	(vl-position
		  (strcase (getvar "INSNAME"))
		  (mapcar 'strcase lst)
		)
	)
      (setq nom (nth n lst))
      (setq nom	(car lst)
	    n	0
      )
    )
    (set_tile "bl" (itoa n))
    (action_tile "sel" "(done_dialog 5)")
    (action_tile "bl" "(setq nom (nth (atoi $value) lst))")
    (action_tile "wbl" "(done_dialog 3)")
    (action_tile "tp" "(setq nom $value) (done_dialog 4)")
    (action_tile
      "accept"
      "(setq nom (nth (atoi (get_tile \"bl\")) lst)) (done_dialog 1)"
    )
    (setq what_next (start_dialog))
    (cond
      ((= what_next 3)
       (if (setq nom (getfiled "S�lectionner un fichier" "" "dwg" 0))
	 (setq what_next 1)
	 (setq what_next 2)
       )
      )
      ((= what_next 4)
       (cond
	 ((not (read nom))
	  (setq what_next 2)
	 )
	 ((tblsearch "BLOCK" nom)
	  (setq what_next 1)
	 )
	 ((findfile (setq nom (strcat nom ".dwg")))
	  (setq what_next 1)
	 )
	 (T
	  (alert (strcat "Le fichier \"" nom "\" est introuvable."))
	  (setq	nom nil
		what_next 2
	  )
	 )
       )
      )
      ((= what_next 5)
       (if (and	(setq ent (car (entsel)))
		(= "INSERT" (cdr (assoc 0 (entget ent))))
	   )
	 (setq nom	 (cdr (assoc 2 (entget ent)))
	       what_next 1
	 )
	 (setq what_next 2)
       )
      )
      ((= what_next 0)
       (setq nom nil)
      )
    )
  )
  (unload_dialog dcl_id)
  (vl-file-delete tmp)
  nom
)

;;=================== COMMANDES ===================;;

;; INCSUF (gile) 14/02/08 -version 2.2-
;; Incr�mente le suffixe des textes, mtextes ou attributs successivement s�lectionn�s.
;; Le suffixe est d�termin� automatiquement en fonction de la nature des carac�res qui
;; le compose et du param�tre "Type(s) de carat�re du suffixe" courant.
;; L'utilisateur peut modifier les param�tres courants dans une boite dialogue.
;;
;; Modif 10/04/08 plus de contr�le sur l'�tiquette de l'attribut

(defun c:incsuf	(/	 ValidSel	 temp	 typ	 inc	 cp
		 file	 dcl_id	 what_next	 ent	 elst	 val
		 prop	 save
		)

  (defun ValidSel (elst / val)
    (setq val (cdr (assoc 1 elst))
	  val (ascii (substr val (strlen val)))
    )
    (or
      (and (= 1 (logand 1 *suffixbin*)) (< 47 val 58))
      (and (= 2 (logand 2 *suffixbin*)) (< 64 val 91))
      (and (= 4 (logand 4 *suffixbin*)) (< 96 val 123))
    )
  )

  (or *suffixbin* (setq *suffixbin* 1))
  (or *incrvalue* (setq *incrvalue* 1))
  (setq	typ  *suffixbin*
	inc  *incrvalue*
	cp   0
	temp (vl-filename-mktemp "Tmp.dcl")
	file (open temp "w")
  )
  (write-line
    (strcat
      "IncsuffInputbox:dialog{"
      "label=\"Param�tres\";"
      ":boxed_column{"
      "label=\"Type(s) de caract�re du suffixe\";"
      ":toggle{"
      "label=\"Nombres [0-9]\";key=\"num\";}"
      ":toggle{"
      "label=\"Majuscules [A-Z]\";key=\"maj\";}"
      ":toggle{"
      "label=\"Minuscules [a-z]\";key=\"min\";}}"
      "spacer;"
      ":edit_box{"
      "label=\"Incr�ment\";key=\"inc\";edit_width=6;allow_accept=true;}"
      ":toggle{"
      "label=\"Copier les propri�t�s\";key=\"cp\";}"
      "spacer;ok_cancel; }"
     )
    file
  )
  (close file)
  (setq	dcl_id	  (load_dialog temp)
	what_next 2
  )
  (while (>= what_next 2)
    (if	(not (new_dialog "IncsuffInputbox" dcl_id))
      (exit)
    )
    (if	(= 1 (logand 1 typ))
      (set_tile "num" "1")
    )
    (if	(= 2 (logand 2 typ))
      (set_tile "maj" "1")
    )
    (if	(= 4 (logand 4 typ))
      (set_tile "min" "1")
    )
    (set_tile "inc" (itoa inc))
    (action_tile "cp" "(setq cp (atoi $value))")
    (action_tile
      "accept"
      "(setq typ (+
       (if (= (get_tile \"num\") \"1\") 1 0)
       (if (= (get_tile \"maj\") \"1\") 2 0)
       (if (= (get_tile \"min\") \"1\") 4 0))
       inc (read (get_tile \"inc\")))
       (cond
       ((zerop typ)
       (alert \"Au moins un type de caract�re doit �tre coch�.\")
       (setq typ *suffixbin*) (done_dialog 2))
       ((or (/= (type inc) 'INT) (< inc 0))
       (alert \"L'incr�ment doit �tre un entier positif.\")
       (setq inc *incrvalue*)  (done_dialog 2))
       (T (done_dialog 1)))"
    )
    (action_tile "cancel" "(setq typ nil) (done_dialog 0)")
    (setq what_next (start_dialog))
  )
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if typ
    (progn
      (setq *suffixbin*	typ
	    *incrvalue*	inc
      )
      (while
	(not
	  (and
	    (setq ent (nentsel
			"\nS�lectionnez le texte de d�part: "
		      )
	    )
	    (setq elst (entget (car ent)))
	    (setq typ (cdr (assoc 0 elst)))
	    (member typ '("ATTRIB" "TEXT" "MTEXT"))
	    (setq val (cdr (assoc 1 elst)))
	    (or	(zerop cp)
		(setq prop (vl-remove nil
				      (mapcar
					(function
					  (lambda (n)
					    (assoc n elst)
					  )
					)
					'(8 62 40 7)
				      )
			   )
		)
	    )
	  )
	)
      )
      (if (ValidSel elst)
	(IncValue "" val "" *incrvalue* *suffixbin* typ save prop)
	(princ "\nParam�tre de suffixe incorrect.")
      )
    )
  )
  (princ)
)

;;==================================================;;

;; INCTXT (gile) 31/03/08
;; Insertions multiples d'un texte avec une valeur � incr�menter.
;; La valeur � incr�menter peut �tre de type num�rique, alphab�tique,
;; ou une combinaison alphanum�rique.

(defun c:inctxt	(/ temp	file dcl_id slst st jlst ju ht ro val inc pref suff hor
		 vert nor dlst pt)

  (setq	temp (vl-filename-mktemp "Tmp.dcl")
	file (open temp "w")
  )
  (write-line
    (strcat
      "IncTxt:dialog{"
      "label=\"Texte incr�ment�\";"
      ":boxed_column{"
      "label=\"Mise en forme\";"
      ":row{"
      ":column{"
      ":popup_list{"
      "label=\"Style\";key=\"st\";edit_width=16;}"
      ":popup_list{"
      "label=\"Justification\";key=\"ju\";edit_width=16;}}"
      ":column{"
      ":edit_box{"
      "label=\"Hauteur\";key=\"ht\";edit_width=5;allow_accept=true;}"
      ":edit_box{"
      "label=\"Rotation\";key=\"ro\";edit_width=5;allow_accept=true;}}}}"
      ":boxed_row{"
      "label=\"Texte\";"
      ":column{"
      ":edit_box{"
      "label=\"Valeur de d�part\";key=\"val\";edit_width=5;allow_accept=true;}"
      ":edit_box{"
      "label=\"Incr�ment\";key=\"inc\";edit_width=5;allow_accept=true;}}"
      "spacer;"
      ":column{"
      ":edit_box{"
      "label=\"Pr�fixe\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{"
      "label=\"Suffixe\";key=\"suff\";edit_width=16;allow_accept=true;}}}"
      "ok_cancel;}"
     )
    file
  )
  (close file)
  (setq dcl_id (load_dialog temp))
  (if (not (new_dialog "IncTxt" dcl_id))
    (exit)
  )
  (while (setq st (tblnext "STYLE" (not st)))
    (if	(/= (cdr (assoc 2 st)) "")
      (setq slst (cons (cdr (assoc 2 st)) slst))
    )
  )
  (setq slst (reverse slst))
  (start_list "st")
  (mapcar 'add_list slst)
  (end_list)
  (setq	jlst '("Gauche"		 "Centre"	   "Droite"
	       "Milieu"		 "Haut Gauche"	   "Haut Centre"
	       "Haut Droite"	 "Milieu Gauche"   "Milieu Centre"
	       "Milieu Droite"	 "Bas Gauche"	   "Bas Centre"
	       "Bas Droite"
	      )
  )
  (start_list "ju")
  (mapcar 'add_list jlst)
  (end_list)
  (or st (setq st (getvar "TEXTSTYLE")))
  (or ju (setq ju "Gauche"))
  (or ht (setq ht (getvar "TEXTSIZE")))
  (or ro (setq ro 0.0))
  (or val (setq val "1"))
  (or inc (setq inc 1))
  (or pref (setq pref ""))
  (or suff (setq suff ""))
  (set_tile "st" (itoa (vl-position st slst)))
  (set_tile "ju" (itoa (vl-position ju jlst)))
  (set_tile "ht" (rtos ht))
  (set_tile "ro" (angtos ro))
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (action_tile "st" "(setq st (nth (atoi $value) slst))")
  (action_tile "ju" "(setq ju (nth (atoi $value) jlst))")
  (action_tile
    "ht"
    "(if (and (numberp (distof $value))
     (< 0 (distof $value)))
     (setq ht (distof $value))
     (progn
     (alert \"N�cessite un nombre r�el strictement positif\")
     (set_tile \"ht\" (rtos ht))
     (mode_tile \"ht\" 2))))"
  )
  (action_tile
    "ro"
    "(if (numberp (angtof $value))
     (setq ro (angtof $value))
     (progn
     (alert \"N�cessite une valeur d'angle valide\")
     (set_tile \"ro\" (angtos ro))
     (mode_tile \"ro\" 2))))"
  )
  (action_tile
    "inc"
    "(if (and (numberp (read $value))
     (<= 0 (read $value)))
     (setq inc (atoi $value))
     (progn
     (alert \"N�cessite un entier positif\")
     (set_tile \"inc\" (itoa inc))
     (mode_tile \"inc\" 2))))"
  )
  (action_tile
    "val"
    "(if (wcmatch $value \"~*.*\")
     (setq val $value)
     (progn
     (alert \"N�cessite uniquement des caract�res alphab�tiques et/ou num�riques\")
     (set_tile \"val\" val)
     (mode_tile \"val\" 2))))"
  )
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(setq ju nil)")
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if ju
    (progn
      (setq hor	 (cond
		   ((wcmatch ju "*Gauche") 0)
		   ((wcmatch ju "*Centre") 1)
		   ((wcmatch ju "*Droite") 2)
		   (T 4)
		 )
	    vert (cond
		   ((wcmatch ju "Haut *") 3)
		   ((wcmatch ju "Milieu *") 2)
		   ((wcmatch ju "Bas *") 1)
		   (T 0)
		 )
	    nor	 (trans '(0 0 1) 1 0 T)
	    dlst (reverse (vl-string->list val))
      )
      (while (setq pt (getpoint "\nSp�cifiez le point d'insertion: "))
	(setq pt (trans pt 1 nor))
	(entmake
	  (list
	    '(0 . "TEXT")
	    (cons 10 pt)
	    (cons 40 ht)
	    (cons 50
		  (+ ro (angle '(0 0 0) (trans (getvar "UCSXDIR") 0 nor)))
	    )
	    (cons 7 st)
	    (cons 11 pt)
	    (cons 72 hor)
	    (cons 73 vert)
	    (cons 1 (strcat pref val suff))
	    (cons 210 nor)
	  )
	)
	(setq val (incsuff val inc 7))
      )
    )
  )
  (princ)
)

;;==================================================;;

;;; INCATT (gile) 03/04/08
;;; Insertions multiple d'un bloc avec incr�mentation d'un attribut
;;; La valeur � incr�menter peut �tre de type num�rique, alphab�tique,
;;; ou une combinaison alphanum�rique.

(defun c:incatt	(/ space name lst tmp file dcl_id scl rot tag val inc pref suff
		 ins)

  (vl-load-com)
  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (setq	space (if (= (getvar "CVPORT") 1)
		(vla-get-PaperSpace *acdoc*)
		(vla-get-ModelSpace *acdoc*)
	      )
  )
  (if (setq name (getblock nil))
    (progn
      (or (tblsearch "BLOCK" name)
	  (vla-delete
	    (vla-InsertBlock
	      space
	      (vlax-3d-point '(0 0 0))
	      name
	      1
	      1
	      1
	      0
	    )
	  )
      )
      (setq name (vl-filename-base name))
      (vlax-for	e (vla-item (vla-get-Blocks *acdoc*) name)
	(if (and
	      (= (vla-get-ObjectName e) "AcDbAttributeDefinition")
	      (= (vla-get-Constant e) :vlax-false)
	    )
	  (setq lst (cons (vla-get-TagString e) lst))
	)
      )
      (if (setq lst (reverse lst))
	(progn
	  (setq	tmp  (vl-filename-mktemp "Tmp.dcl")
		file (open tmp "w")
	  )
	  (write-line
	    (strcat
	      "incins:dialog{"
	      "label=\"Attribut inc�ment�\";"
	      ":boxed_column{"
	      "label=\"Bloc\";"
	      ":edit_box{"
	      "label=\"Echelle globale\";key=\"scl\";edit_width=5;}"
	      ":edit_box{"
	      "label=\"Rotation\";key=\"rot\";edit_width=5;}} "
	      ":boxed_column{"
	      "label=\"Attribut\";"
	      ":popup_list{"
	      "label=\"Etiquette\";key=\"tag\";edit_width=16;}"
	      ":edit_box{"
	      "label=\"Valeur de d�part\";key=\"val\";edit_width=5;allow_accept=true;}"
	      ":edit_box{"
	      "label=\"Incr�ment\";key=\"inc\";edit_width=5;allow_accept=true;}"
	      ":edit_box{"
	      "label=\"Pr�fixe\";key=\"pref\";edit_width=16;allow_accept=true;}"
	      ":edit_box{"
	      "label=\"Suffixe\";key=\"suff\";edit_width=16;allow_accept=true;}}"
	      "ok_cancel;}"
	     )
	    file
	  )
	  (close file)
	  (setq	scl    1.0
		rot    0.0
		val    "1"
		inc    1
		pref   ""
		suff   ""
		dcl_id (load_dialog tmp)
	  )
	  (if (not (new_dialog "incins" dcl_id))
	    (exit)
	  )
	  (start_list "tag")
	  (mapcar 'add_list lst)
	  (end_list)
	  (set_tile "scl" (rtos scl))
	  (set_tile "rot" (rtos rot))
	  (set_tile "val" val)
	  (set_tile "inc" (itoa inc))
	  (action_tile
	    "scl"
	    "(if (and (distof $value)
             (< 0 (distof $value)))
             (setq scl (distof $value))
             (progn
	     (alert \"N�cessite un nombre r�el strictement positif\")
	     (set_tile \"scl\" (rtos scl))
	     (mode_tile \"scl\" 2)))"
	  )
	  (action_tile
	    "rot"
	    "(if (numberp (angtof $value))
             (setq rot (angtof $value))
             (progn
	     (alert \"N�cessite une valeur d'angle valide\")
	     (set_tile \"rot\" (angtos rot))
	     (mode_tile \"rot\" 2)))"
	  )
	  (action_tile
	    "inc"
	    "(if (and (numberp (read $value))
             (<= 0 (read $value)))
             (setq inc (atoi $value))
             (progn
	     (alert \"N�cessite un entier positif\")
	     (set_tile \"inc\" (itoa inc))
	     (mode_tile \"inc\" 2))))"
	  )
	  (action_tile
	    "val"
	    "(if (wcmatch $value \"~*.*\")
             (setq val $value)
             (progn
	     (alert \"N�cessite uniquement des caract�res alphab�tiques et/ou num�riques\")
	     (set_tile \"val\" val)
	     (mode_tile \"val\" 2))))"
	  )
	  (action_tile "pref" "(setq pref $value)")
	  (action_tile "suff" "(setq suff $value)")
	  (action_tile
	    "accept"
	    "(setq tag (atoi (get_tile\"tag\"))) (done_dialog)"
	  )
	  (action_tile "cancel" "(setq tag nil)")
	  (start_dialog)
	  (unload_dialog dcl_id)
	  (vl-file-delete tmp)
	  (if tag
	    (while (setq ins (getpoint "\nSp�cifiez le point d'insertion: "))
	      (vla-put-Textstring
		(nth tag
		     (vlax-invoke
		       (vla-InsertBlock
			 space
			 (vlax-3d-point (trans ins 1 0))
			 name
			 scl
			 scl
			 scl
			 rot
		       )
		       'getAttributes
		     )
		)
		(strcat pref val suff)
	      )
	      (setq val (incsuff val inc 7))
	    )
	  )
	)
	(princ "\nCe bloc ne contient pas d'attributs.")
      )
    )
  )
  (princ)
)

;;==================================================;;

;; INCSEL (gile) 05/04/08
;; Incr�mente par s�lection successive une valeur contenue dans des textes,
;; mtextes ou attributs.
;;
;; Modif 10/04/08 plus de contr�le sur l'�tiquette de l'attribut

(defun c:incsel	(/ temp	file val inc pref suff cp dcl_id ent elst typ prop save)
  (setq	temp (vl-filename-mktemp "Tmp.dcl")
	file (open temp "w")
  )
  (write-line
    (strcat
      "IncselInputbox:dialog{label=\"Param�tres\";"
      ":edit_box{"
      "label=\"Valeur de d�part\";key=\"val\";edit_width=5;allow_accept=true;}"
      ":edit_box{"
      "label=\"Incr�ment\";key=\"inc\";edit_width=5;allow_accept=true;}"
      ":edit_box{"
      "label=\"Pr�fixe\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{"
      "label=\"Suffixe\";key=\"suff\";edit_width=16;allow_accept=true;}"
      ":toggle{"
      "label=\"Copier les propri�t�s\";key=\"cp\";}"
      "spacer;ok_cancel;}"
     )
    file
  )
  (close file)
  (setq	val    "1"
	inc    1
	pref   ""
	suff   ""
	cp     0
	dcl_id (load_dialog temp)
  )
  (if (not (new_dialog "IncselInputbox" dcl_id))
    (exit)
  )
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (set_tile "cp" (itoa cp))
  (action_tile
    "inc"
    "(if (and (numberp (read $value))
     (<= 0 (read $value)))
     (setq inc (atoi $value))
     (progn
     (alert \"N�cessite un entier positif\")
     (set_tile \"inc\" (itoa inc))
     (mode_tile \"inc\" 2))))"
  )
  (action_tile
    "val"
    "(if (wcmatch $value \"~*.*\")
     (setq val $value)
     (progn
     (alert \"N�cessite uniquement des caract�res alphab�tiques et/ou num�riques\")
     (set_tile \"val\" val)
     (mode_tile \"val\" 2))))"
  )
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile "cp" "(setq cp (atoi $value))")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(setq inc nil)")
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if inc
    (progn
      (while
	(not
	  (and
	    (setq ent (nentsel
			"\nS�lectionnez le texte de d�part: "
		      )
	    )
	    (setq elst (entget (car ent)))
	    (setq typ (cdr (assoc 0 elst)))
	    (member typ '("ATTRIB" "TEXT" "MTEXT"))
	    (or	(zerop cp)
		(setq prop (vl-remove nil
				      (mapcar
					(function
					  (lambda (n)
					    (assoc n elst)
					  )
					)
					'(8 62 40 7)
				      )
			   )
		)
	    )
	  )
	)
      )
      (setq save (cons elst save))
      (entmod
	(subst (cons 1 (strcat pref val suff)) (assoc 1 elst) elst)
      )
      (if (= typ "ATTRIB")
	(entupd (cdr (assoc 330 elst)))
      )
      (IncValue pref val suff inc 7 typ save prop)
    )
  )
  (princ)
)

;;==================================================;;

;; INCADD (gile) 10/04/08
;; Ajoute une valeur incr�ment�e au d�but ou � la fin des textes,
;; mtextes ou attributs successivement s�lectionn�s.

(defun c:incadd
		(/ bin inc val pref suff temp typ file dcl_id pos ent elst str
		 save)
  (setq	temp (vl-filename-mktemp "Tmp.dcl")
	file (open temp "w")
  )
  (write-line
    (strcat
      "IncselInputbox:dialog{label=\"Param�tres\";"
      ":boxed_radio_column{"
      "label=\"Position\";key=\"pos\";"
      ":radio_button{"
      "label=\"Au d�but\";key=\"start\";}"
      ":radio_button{"
      "label=\"� la fin\";key=\"end\";value=\"1\";}}"
      ":edit_box{"
      "label=\"Valeur de d�part\";key=\"val\";edit_width=5;allow_accept=true;}"
      ":edit_box{"
      "label=\"Incr�ment\";key=\"inc\";edit_width=5;allow_accept=true;}"
      ":edit_box{"
      "label=\"Pr�fixe\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{"
      "label=\"Suffixe\";key=\"suff\";edit_width=16;allow_accept=true;}"
      "spacer;ok_cancel;}"
     )
    file
  )
  (close file)
  (setq	val    "1"
	inc    1
	pref   ""
	suff   ""
	dcl_id (load_dialog temp)
  )
  (if (not (new_dialog "IncselInputbox" dcl_id))
    (exit)
  )
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (action_tile
    "inc"
    "(if (and (numberp (read $value))
     (<= 0 (read $value)))
     (setq inc (atoi $value))
     (progn
     (alert \"N�cessite un entier positif\")
     (set_tile \"inc\" (itoa inc))
     (mode_tile \"inc\" 2))))"
  )
  (action_tile
    "val"
    "(if (wcmatch $value \"~*.*\")
     (setq val $value)
     (progn
     (alert \"N�cessite uniquement des caract�res alphab�tiques et/ou num�riques\")
     (set_tile \"val\" val)
     (mode_tile \"val\" 2))))"
  )
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile
    "accept"
    "(setq pos (get_tile \"pos\")) (done_dialog)"
  )
  (action_tile "cancel" "(setq inc nil)")
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if inc
    (while (or (initget 1 "annUler")
	       (setq ent (nentsel
			   (strcat "\nS�lectionnez un texte"
				   (if save
				     " ou [annUler]: "
				     ":"
				   )
			   )
			 )
	       )
	   )
      (if (= ent "annUler")
	(if save
	  (progn
	    (setq elst (car save))
	    (entmod elst)
	    (and (= "ATTRIB" (cdr (assoc 0 elst)))
		 (entupd (cdr (assoc 330 elst)))
	    )
	    (setq val  (incsuff val (- inc) 7)
		  save (cdr save)
	    )
	  )
	  (princ "\nPlus rien � annuler.")
	)
	(and (setq elst (entget (car ent)))
	     (member (cdr (assoc 0 elst)) '("ATTRIB" "MTEXT" "TEXT"))
	     (setq save (cons elst save))
	     (setq str (cdr (assoc 1 elst)))
	     (entmod
	       (subst (cons 1
			    (if	(= "start" pos)
			      (strcat pref val suff str)
			      (strcat str pref val suff)
			    )
		      )
		      (assoc 1 elst)
		      elst
	       )
	     )
	     (setq val (incsuff val inc 7))
	     (and (= "ATTRIB" (cdr (assoc 0 elst)))
		  (entupd (cdr (assoc 330 elst)))
	     )
	)
      )
    )
  )
  (princ)
)

;; INCR
;; Permet de choisir une fonction d'incr�mentation

(defun c:incr (/ temp file dcl_id fun)
  (setq	temp (vl-filename-mktemp "Tmp.dcl")
	file (open temp "w")
  )
  (write-line
    (strcat
      "IncrInputbox:dialog{label=\"Incr�menter\";" ":radio_column{key=\"fun\";"
      ":radio_button{"
      "label=\"INCTXT Ins�rer un texte contenant une valeur incr�ment�e\";key=\"c:inctxt\";}"
      ":radio_button{"
      "label=\"INCATT Ins�rer un bloc contenant un attribut incr�ment�\";key=\"c:incatt\";}"
      ":radio_button{"
      "label=\"INCSEL Incr�menter une valeur dans les textes s�lectionn�s\";key=\"c:incsel\";}"
      ":radio_button{"
      "label=\"INCSUF Incr�menter la partie finale des textes s�lectionn�s\";key=\"c:incsuf\";}"
      ":radio_button{"
      "label=\"INCADD Ajouter une valeur incr�ment�e aux textes s�lectionn�s\";key=\"c:incadd\";}}"
      "ok_cancel;}")
    file
  )
  (close file)
  (setq dcl_id (load_dialog temp))
  (if (not (new_dialog "IncrInputbox" dcl_id))
    (exit)
  )
  (set_tile "c:inctxt" "1")
  (action_tile
    "accept"
    "(setq fun (get_tile \"fun\")) (done_dialog)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (and fun (apply (read fun) nil))
  (princ)
)