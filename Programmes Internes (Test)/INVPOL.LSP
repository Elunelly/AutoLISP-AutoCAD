 
;;---Début---------------------------------------------------dd_ext--------------
;; << extrait une information DXF d'une entité                                 >>
;; <<                                                                          >>
;;
;;  créée le :  mardi 16 février 1999  à 21:12
;;
;; Admet : 
;; =======
;;   cle        : Entier   = code DXF
;;   e          : Nom d'entité = entité ou liste tirée de entget
;;
;; Retourne : varie = résultat
;; ==========
;-------------------------------------------------------------------------------
(Defun dd_ext ( cle e / )
  (if (= 'ENAME (type e)) (setq e (entget e)))
  (cdr (assoc cle e))
)
;;---fin-----------------------------------------------------dd_ext--------------


;;---Début---------------------------------------------------dd_snoc-------------
;; << ajoute un élément à la fin d'une liste                                   >>
;; <<                                                                          >>
;;
;;  créée le :  mardi 16 février 1999  à 21:05
;;
;; Admet : 
;; =======
;;   elem       : element  = élément
;;   li         : Liste    = liste
;;
;; Retourne : Sans intéret = liste complétée
;; ==========
;-------------------------------------------------------------------------------
(Defun dd_snoc ( elem li / )
  (append li (list elem))
)
;;---fin-----------------------------------------------------dd_snoc-------------
 
;;---Début---------------------------------------------------dd_analpol----------
;; << analyse une polyligne                                                    >>
;; <<                                                                          >>
;;
;;  créée le :  mardi 16 février 1999  à 21:06
;;
;; Admet : 
;; =======
;;   pol        : Nom d'entité = nom de la polyligne
;;
;; Retourne : Liste points courbure = ((point courbure) (point courbure)....)
;; ==========
;-------------------------------------------------------------------------------
(Defun dd_analpol ( pol / e ent ll alti)
  (setq ll '() e pol)
  (cond
   ((= "POLYLINE" (dd_ext 0 pol))
    (while (/= "SEQEND" (dd_ext 0 (setq ent (entget (setq e (entnext e))))))
      (setq ll (cons (list (trans (dd_ext 10 ent) pol 1) (dd_ext 42 ent)) ll))
    )
   )
   ((= "LWPOLYLINE" (dd_ext 0 pol))
    (setq ent (entget pol) ptcrb nil alti 0.0)
    (while ent
      (if (= 38 (caar ent)) (setq alti (cdar ent)) )
      (if (= 10 (caar ent)) (setq ptcrb (trans (snoc alti (dd_xy (cdar ent))) pol 1)))
      (if (= 42 (caar ent)) (setq ll (cons (list ptcrb (cdar ent)) ll)))
      (setq ent (cdr ent))
    )
   )
  )
  (if (= 1 (logand (dd_ext 70 pol) 1))
      (dd_snoc (last ll) (reverse ll))
      (setq ll (reverse ll)
	    ll (dd_snoc (list (car (last ll)) 0.0) (reverse (cdr (reverse ll))))
      ) 	    
  )  
)
;;---fin-----------------------------------------------------dd_analpol----------

 
;;---Début---------------------------------------------------dd_reversepol-------
;; << inverse une liste de points courbure                                     >>
;; <<                                                                          >>
;;
;;  créée le :  mardi 16 février 1999  à 21:15
;;
;; Admet : 
;; =======
;;   lpc        : Liste points courbure = ou polyligne
;;
;; Retourne : Liste points courbure = inversée
;; ==========
;-------------------------------------------------------------------------------
(Defun dd_reversepol ( lpc / )
  (if (= 'ENAME (type lpc)) (setq lpc (dd_analpol lpc)))
  (mapcar '(lambda (x1 x2)
              (list (car x1) (* (cadr x2) -1))
           )
          
           (reverse lpc)
           (dd_snoc (last lpc) (cdr (reverse lpc)))
   
  )
)
;;---fin-----------------------------------------------------dd_reversepol-------

 
;;---Début---------------------------------------------------dd_tracepol---------
;; << trace une polyligne à partir d'une liste points courbure                 >>
;; <<                                                                          >>
;;
;;  créée le :  mardi 16 février 1999  à 21:26
;;
;; Admet : 
;; =======
;;   lpc        : Liste points courbure = 
;;
;; Retourne : Sans intérêt = 
;; ==========
;-------------------------------------------------------------------------------
(Defun dd_tracepol ( lpc / lp)
    (setq lp (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity")
                   (cons 67 0)
                   (cons 100 "AcDbPolyline") (cons 90 (length lpc))
                   (cons 70 0) (cons 43 0)
                   (cons 38 0.0) (cons 39 0.0)
                   
             ) 
    )
    (foreach vert lpc
       (setq lp (append lp (list (cons 10 (car vert))
                                 (cons 42 (cadr vert))
                           )
                )
       )
                                                   
    )
    (setq lp (append lp (list (cons 210 (list 0.0 0.0 1.0)))))
    (entmake lp)
)
;;---fin-----------------------------------------------------dd_tracepol---------

 
;;---Début---------------------------------------------------C:Retournepol-------
;; << inverse une polyligne                                                    >>
;; <<                                                                          >>
;;
;;  créée le :  mardi 16 février 1999  à 21:22
;;
;; Admet : 
;; =======
;;
;; Retourne : Liste points courbure = 
;; ==========
;-------------------------------------------------------------------------------
(Defun C:Retournepol ( / e lp)
  (setq e (car (entsel "\npointer une polyligne")))
  (if (and e (member (dd_ext 0 e) '("POLYLINE" "LWPOLYLINE")))
    (progn
      (setq lp (dd_reversepol e))
      (entdel e)
      (dd_tracepol lp)
    )
  )  
)
;;---fin-----------------------------------------------------C:Retournepol-------
(alert "Commande RETOURNEPOL Chargée")