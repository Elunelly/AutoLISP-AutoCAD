(defun putXData (jsel / n i name att-list dyn-list str pt bj ent)
  (if jsel
    (progn
      (repeat (setq n 0 i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if
          (and
            (wcmatch (getpropertyvalue name "Classname") "")
            (wcmatch (vla-get-EffectiveName (ConvName name 'VLA-OBJECT)) "Harnais*")
            (setq att-list (get-att-list name))
            (setq dyn-list (get-dyn-list name))
            (setq str (strcat (cdr (assoc "T1" att-list)) "-" (cdr (assoc "I01" att-list)) "-" (cdr (assoc "H1" att-list))))
            (setq pt (cdr (assoc 10 (entget name))))
            (setq bj (list (cdr (assoc "BJ_Harnais X" dyn-list)) (cdr (assoc "BJ_Harnais Y" dyn-list)) 0.0))
            (setq bj (trans (mapcar '+ pt bj) 0 1))
            (setq ent (ssget "_C" (mapcar '- bj '(0.5 0.5 0.0)) (mapcar '+ bj '(0.5 0.5 0.0)) '((0 . "LWPOLYLINE") (8 . "UBS-400-Câble CC- Harnais-Ond."))))
            (setq ent (ssname ent 0))
            (vlax-ldata-put name "String_Tag" str)
            (SetAnyProperty name "*" 1 "Color" "221")
            (vlax-ldata-put ent "String_Tag" str)
            (SetAnyProperty ent "*" 1 "Color" "221")
          )
          (cons (setq n (1+ n)) (sslength jsel))
        )
      )
    )
  )
)
;; Ex: (putXData (ssget '((0 . "INSERT") (66 . 1) (8 . "UBS-400-Représentation Harnais"))))

(defun c:putXData (/ name str att-list ent)
  (while
    (not
      (and
        (setq name (entsel "\nSélectionner un bloc : "))
        (setq name (car name))
        (= (cdr (assoc 0 (entget name))) "INSERT")
        (= (cdr (assoc 8 (entget name))) "UBS-400-Représentation Harnais")
        (wcmatch (getpropertyvalue name "ClassName") "")
        (wcmatch (vla-get-effectivename (ConvName name 'vla-object)) "Harnais*")
        (setq att-list (get-att-list name))
        (setq str (strcat (cdr (assoc "T1" att-list)) "-" (cdr (assoc "I01" att-list)) "-" (cdr (assoc "H1" att-list))))
        (vlax-ldata-put name "String_Tag" str)
        (SetAnyProperty name "*" 1 "Color" "221")
      )
    )
  )
  (while
    (not
      (and
        (setq ent (entsel "\nSélectionner une polyligne :"))
        (setq ent (car ent))
        (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
        (= (cdr (assoc 8 (entget ent))) "UBS-400-Câble CC- Harnais-Ond.")
        (vlax-ldata-put ent "String_Tag" str)
        (SetAnyProperty ent "*" 1 "Color" "221")
      )
    )
  )
  (princ)
)

(defun c:XDATA_ADD-ELECTRICAL (/ lst end src tgt str col atl)
  (setq lst (att2str))
  (while (not end)
    (while
      (not
        (and
          (null (initget 0 "Quitter _eXit"))
          (setq src (entsel (LgT "\nSelect a block or [eXit] :" "\nSélectionner un bloc ou [Quitter] :" 0)))
          
        )
      )
    )
  )
)

(defun try (jsel key f / i name lst)
  (if jsel
    (repeat (setq i (sslength jsel))
      (setq name (ssname jsel (setq i (1- i))))
      (if (assoc key (entget name))
        (setq lst (cons (cdr (assoc key (entget name))) lst))
      )
    )
  )
  (if lst
    (princ
      (strcat
        "\nValeur max. : " (vl-prin1-to-string (car (vl-sort lst '>)))
        "\nValeur min. : " (vl-prin1-to-string (car (vl-sort lst '<)))
        "\n"
      )
    )
  )
  (if f lst (princ))
)



(defun c:putXData (/ name str att-list ent)
  (while
    (not
      (and
        (setq name (entsel "\nSélectionner un bloc : "))
        (setq name (car name))
        (= (cdr (assoc 0 (entget name))) "INSERT")
        (= (cdr (assoc 8 (entget name))) "UBS-400-Onduleurs")
        (wcmatch (getpropertyvalue name "ClassName") "")
        (wcmatch (vla-get-effectivename (ConvName name 'vla-object)) "Onduleur*")
        (setq att-list (get-att-list name))
        (setq str (cdr (assoc "NUM" att-list)))
        (vlax-ldata-put name "String_Tag" str)
        (SetAnyProperty name "*" 1 "Color" "221")
      )
    )
  )
  (while
    (not
      (and
        (setq ent (entsel "\nSélectionner une polyligne :"))
        (setq ent (car ent))
        (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
        (wcmatch (cdr (assoc 8 (entget ent))) "UBS-400-Câble AC - Ond.*")
        (vlax-ldata-put ent "String_Tag" str)
        (SetAnyProperty ent "*" 1 "Color" "221")
      )
    )
  )
  (princ)
)
;; Ex: ((lambda (/ name) (while (setq name (entsel "\nSélectionner un bloc : ")) (putXData (car name)))))


(defun extractXData (jsel / *error* i name filename file data leng sep str psel)
  (defun *error* (msg)
    (if file (close file))
    (sssetfirst nil jsel)
    (princ msg)
  )
  (if
    (and
      jsel
      (setq filename (getfiled "File name" "C:\\Users\\lphilip\\OneDrive - URBASOLAR\\Documents\\C2530 - DISNEY T3\\C2530-EXPORT CC HARNESS" "csv" 37))
      (setq file (open filename "W"))
    )
    (progn
      (setq psel (ssadd))
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if
          (and
            (setq data (vlax-ldata-get name "String_Tag"))
            (setq leng (getpropertyvalue name "LENGTH"))
            (setq sep ";")
            (setq str (strcat data sep (rtos leng) sep))
          )
          (write-line str file)
          (ssadd name psel)
        )
      )
      (close file)
      (sssetfirst nil psel)
    )
  )
)
;; Ex: (ExtractXData (ssget '((0 . "LWPOLYLINE") (8 . "UBS-400-*Harnais-Ond.") (62 . 221))))

(defun HarnessExtensionCount (jsel / *error* i name filename file data blk lngG lngD leng n sep str)
  (defun *error* (msg)
    (if file (close file))
    (sssetfirst nil jsel)
    (princ msg)
  )
  (if
    (and
      jsel
      (setq filename (getfiled "File name" "C:\\Users\\lphilip\\OneDrive - URBASOLAR\\Documents\\C2530 - DISNEY T3\\C2530-EXPORT EXTENSION HARNESS COUNT" "csv" 37))
      (setq file (open filename "W"))
    )
    (progn
      (repeat (setq i (sslength jsel))
        (if
          (and
            (setq name (ssname jsel (setq i (1- i))))
            (setq data (vlax-ldata-get name "String_Tag"))
            (setq blk (vla-get-EffectiveName (vlax-ename->vla-object name)))
            (cond
              ( (wcmatch blk "*Standard*")
                (setq lngG (getpropertyvalue name "AcDbDynBlockPropertyDist. Gauche"))
                (setq lngD (getpropertyvalue name "AcDbDynBlockPropertyDist. Droite"))
                (setq n 3)
                (not (equal (setq leng (- (+ lngG lngD) (* 3.785 2.0))) 0.0 1e-3))
              )
              ( (wcmatch blk "*Bi-Module*")
                (setq n 6)
                (setq leng 3.10)
              )
              ( (wcmatch blk "*Tri-Module*")
                (setq n 3)
                (setq leng 4.50)
              )
            )
            (setq sep ";")
            (setq str (strcat data sep (itoa n) sep (rtos leng)))
            (write-line str file)
          )
          (ssdel name jsel)
        )
      )
      (close file)
      (sssetfirst nil jsel)
    )
  )
)
;; Ex: (HarnessExtensionCount (ssget '((0 . "INSERT") (66 . 1) (8 . "UBS-400-Représentation Harnais") (62 . 221))))

(defun HarnessReplaceBlock (jsel tag / i name blk pt att-list dyn-list str)
  (setvar "CMDECHO" 0)
  (setvar "ATTDIA" 0)
  (repeat (setq i (sslength jsel))
    (setq
      name (ssname jsel (setq i (1- i)))
      blk (vla-get-effectivename (vlax-ename->vla-object name))
      pt (cdr (assoc 10 (entget name)))
      rot (cdr (assoc 50 (entget name)))
      lay (cdr (assoc 8 (entget name)))
      att-list (get-att-list name)
      dyn-list (get-dyn-list name)
    )
    (if
      (tblsearch
        "BLOCK"
        (setq str
          (strcat
            (substr blk 1 (- (strlen blk) 6))
            tag
          )
        )
      )
      (progn
        (setq blk (AttInsert str pt rot 1.0 lay))
        (set-att-list blk att-list)
        (set-dyn-list blk (vl-remove-if '(lambda (x) (= "Origin" (car x))) dyn-list))
        (entdel name)
      )
    )
  )
  (setvar "CMDECHO" 1)
  (setvar "ATTDIA" 1)
  (princ)
)
;; Ex: (HarnessReplaceBlock (ssget '((0 . "INSERT") (66 . 1) (8 . "UBS-400-Représentation Harnais"))) "(PL12)")

;; AttInsert  by Jason Piercey 12-31-2002			
;; Example of how to entmake an attributed insert		
;; based on an attributed block definition			
;; Return: T or nil						
;; Notes:							
;; This could be done in a much more programmer			
;; friendly way (like provide additional arguments,		
;; and translate points from block to insert)			
;; but would require much more code to do so,			
;; this is only a generic example.				
;;								
;; For more information on translating points see		
;; www.fleming-group.com -> download page ->			
;; Coordinates ->.						
;;								
;; Modified on 01-24-2020 ADP-Resources Mark Mitchell		
;; Add point of insertion					
;; Add rotation							
;; Add Scale							
;; Add Layer							
;; 								

(defun AttInsert (BlockName				    ; BlockName	Block Name			
		  Pt					    ; Pt	insertion Point			
		  Rot					    ; Rot	Rotation in rad			
		  Sc					    ; Sc	Scale				
		  Layer	/				    ; Layer	Layer				
		  Ename					    ; Ename	Entity Name			
		  NextEnt				    ; NextEnt	Next Entity name		
		  Data					    ; Data	object Data of NextEnt		
		  Attdefs)				    ; Attdefs	Attribute definitions table	
  (cond
    (							    ; condition 1
     ;; get Parent entity name
     (setq Ename (tblobjname "block" BlockName))

     ;; first sub entity
     (setq NextEnt (entnext Ename))

     ;; get ATTDEF subentities
     (while
       NextEnt
	(setq Data (entget NextEnt))
	(if (= "ATTDEF" (cdr (assoc 0 Data)))
	  (setq Attdefs (cons Data Attdefs))
	)						    ; (if (= "ATTDEF"
	(setq NextEnt (entnext NextEnt))
     )							    ; (while NextEnt

     (and
       ;; entmake insert
       (setq name
       (entmakex
	 (list
	   (cons 0 "INSERT")
	   (cons 8 Layer)
	   (cons 66 1)					    ; attribute follow flag
	   (cons 67 0)					    ; model space 0 paper space 1
	   (cons 2 BlockName)
	   (cons 10 Pt)
	   (cons 41 Sc)
	   (cons 42 Sc)
	   (cons 43 Sc)
	   (cons 50 Rot)
	 )						    ; (list
       ))						    ; (entmake

       ;; entmake ATTRIBs based on ATTDEFS
       (foreach	x (reverse Attdefs)
	 (entmakex
	   (list
	     (cons 0 "ATTRIB")
	     (cons 100 "AcDbEntity")
	     (assoc 67 x)				    ; model space 0 paper space 1
	     (assoc 8 x)				    ; layer
	     (assoc 62 x)				    ; color 0 indicates by block
	     (cons 100 "AcDbText")
	     (cons 10
		   (polar Pt
			  (+ Rot (angle '(0 0 0) (cdr (assoc 10 x))))
			  (* (distance '(0 0 0) (cdr (assoc 10 x))) Sc)
		   )
	     )						    ; insertion point (ignored)
	     (cons 40 (* Sc (cdr (assoc 40 x))))  ; text height
	     (assoc 1 x)				    ; attribute text
	     (cons 50 (+ Rot (cdr (assoc 50 x))))	    ; insertion angle
	     (assoc 41 x)                                   ; Relative X Scale factor (width) (optional; default = 1)
	     (assoc 51 x)				    ; Oblique angle (optional; default = 0) 
	     (assoc 7 x)				    ; text style
	     (assoc 71 x)				    ; 0 normal text  2 backwards 4 upside down
	     (assoc 72 x)				    ; Horz 0 = Left; 1= Center; 2 = Right
	     (cons 11
		   (polar Pt
			  (+ Rot (angle '(0 0 0) (cdr (assoc 11 x))))  ; assumes OCS base-point is (0 0 0)
			  (* (distance '(0 0 0) (cdr (assoc 11 x))) Sc)
		   )
	     )						    ; attribute insertion point
	     (cons 100 "AcDbAttribute")
	     (cons 280 0)				    ; version number:  0 = 2010
	     (assoc 2 x)				    ; attribute name
	     (assoc 70 x)				    ; preset value no prompt
	     (assoc 74 x)				    ; Vert 0 = Baseline; 1 = Bottom; 2 = Middle; 3 = Top
	     (cons 280 0)				    ; attribute lock 1 unlock 0
	   )						    ; (list
	 )						    ; (entmake
       )						    ; (foreach	x (reverse Attdefs)
       ;; entmake SEQEND
       (entmakex '((0 . "SEQEND") (8 . "0")))
     )							    ; (and
     name
    )							    ; ( condition 1
    (T nil)						    ; condition 2 (default)
  )							    ; (cond
)							    ; (defun AttInsert

(defun c:RNL (/ vla-collection->list layer pattern)
  (defun vla-collection->list (doc col flag / lst item i)
    (if (null (vl-catch-all-error-p (setq i 0 col (vl-catch-all-apply 'vlax-get (list (if doc doc (vla-get-activedocument (vlax-get-acad-object))) col)))))
      (vlax-for item col
        (setq lst (cons  (cons
              (if (vlax-property-available-p item 'Name)
                (vla-get-name item)
                (strcat "Unnamed_" (itoa (setq i (1+ i))))
              )
              (cond
                ((= flag 0)
                  (vlax-vla-object->ename item)
                )
                (t
                  item
                )
              )
            )
            lst
            )
        )
      )
    )
    (reverse lst)
  )
  (if $str$
    (if (= "" (setq pattern (getstring T (strcat "\nSpécifier la chaîne à remplacer <" $str$ "> : "))))
      (setq pattern $str$)
    )
    (setq pattern (getstring T "\nSpécifier la chaîne à remplacer : "))
  )
  (setq $str$ (getstring T "\nSpécifier la chaîne de remplacement : "))
  (mapcar
    '(lambda (x / layer name)
      (setq
        layer (cdr x)
        name (car x)
      )
      (if (wcmatch name (strcat "*" pattern "*"))
        (entmod (subst (cons 2 (vl-string-subst $str$ pattern name)) (assoc 2 (entget layer)) (entget layer)))
      )
     )
    (vla-collection->list nil 'layers 0)
  )
  (princ)
)

(defun extract (l i)
  (vl-remove-if-not '(lambda (x) (= 0 (rem (1+ (vl-position x l)) (1+ i)))) l)
)

(defun extract (l i / n)
  (defun f (n i / c l)
    (setq c -1)
    (repeat (fix (/ n i))
      (setq l (cons (setq c (+ c i)) l))
    )
    (reverse l)
  )
  (mapcar '(lambda (n) (nth n l)) (f (length l) (1+ i)))
)