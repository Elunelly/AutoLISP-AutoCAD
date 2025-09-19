;;;======================== Dictionnaires d'extension ========================;;;

;; gc:GetExtDict (gile)
;; Retourne le dictionnaire d'extension de l'entité (ou nil)
;;
;; Argument : ent (ENAME)

(defun gc:GetExtDict (ent)
 (cdadr (member '(102 . "{ACAD_XDICTIONARY") (entget ent)))
)

;; gc:GetOrCreateExtDict (gile)
;; Retourne le dictionnaire d'extension de l'entité
;; Le dictionnaire est créé s'il n'existe pas
;;
;; Argument : ent (ENAME)

(defun gc:GetOrCreateExtDict (ent / dict)
 (cond
   ((cdadr (member '(102 . "{ACAD_XDICTIONARY") (entget ent))))
   ((setq dict  (entmakex
    '((0 . "DICTIONARY") (100 . "AcDbDictionary"))
  )
    )
    (entmod
      (vl-list*
        (assoc -1 elst)
        (assoc 0 elst)
        (assoc 5 elst)
        (cons 102 "{ACAD_XDICTIONARY")
        (cons 360 dict)
        (cons 102 "}")
        (vl-remove-if (function (lambda (x) (member (car x) '(-1 0 5)))) elst)
      )
    )
    dict
   )
 )
)


;;;========================== Dictionnaires nommés ===========================;;;

;; gc:GetNamedDict
;; Retourne le ENAME du dictionnaire nommé (ou nil)
;;
;; Arguments
;; dict : ENAME du dictionnaire parent
;; name : nom du dictionnaire à chercher
(defun gc:GetNamedDict (dict name)
 (cdr (assoc -1 (dictsearch dict name)))
)

;; gc:GetOrCreateNamedDict
;; Retourne le ENAME du dictionnaire nommé
;; Le dictionnaire est créé s'il n'existe pas
;;
;; Arguments
;; dict : ENAME du dictionnaire parent
;; name : nom du dictionnaire à chercher ou créer

(defun gc:GetOrCreateNamedDict (dict name)
 (if (snvalid name)
   (cond
     ((gc:GetNamedDict dict name))
     ((dictadd  dict
  name
  (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary")))
      )
     )
   )
 )
)


;;;========================= Entrées du dictionnaire =========================;;;

;; gc:GetDictEntries
;; Retourne la liste des entrées du dictionnaire
;; sous forme de paires pointées (Nom . ENAME)
;;
;; Argument : dict le dictionnaire (ENAME ou liste DXF)

(defun gc:GetDictEntries (dict / result)
 (and (= (type dict) 'ENAME) (setq dict (entget dict)))
 (while
   (setq dict (vl-member-if (function (lambda (x) (= (car x) 3))) (cdr dict)))
    (setq result (cons (cons (cdar dict) (cdadr dict)) result))
 )
 (reverse result)
)


;;;=========================== Données du XRecord ============================;;;

;; gc:SetXrecData
;; Retourne le ENAME du XRecord auquel sont affectées mes données
;;
;; Arguments
;; dict : ENAME du dictionnaire parent
;; key : nom du XRecord
;; data : liste de paires pointées contenant les données

(defun gc:SetXrecData (dict key data / xrec)
 (if (snvalid key)
   (progn
     (and (setq xrec (dictsearch dict key))
   (entdel (cdr (assoc -1 xrec)))
     )
     (dictadd
dict
key
(entmakex
  (vl-list*
    '(0 . "XRecord")
    '(100 . "AcDbXRecord")
    data
  )
)
     )
   )
 )
)

;; gc:GetXrecData
;; Retourne la liste des données affectées au XRecord (liste de paires pointées)
;;
;; Arguments
;; dict : ENAME du dictionnaire parent
;; key : nom du XRecord

(defun gc:GetXrecData (dict key / xrec)
 (if (and
(setq xrec (dictsearch dict key))
(= (cdr (assoc 0 xrec)) "XRecord")
     )
   (cdr (member (assoc 280 xrec) xrec))
 )
)