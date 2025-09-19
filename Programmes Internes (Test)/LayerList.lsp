(defun c:LAYERLIST (/ len rng htx ech pt doc layers ms i lck coe spt ept tpt obj txt)
  (setq
    len 2.0
    rng 1.0
    htx 0.25
    ech 1.0
    mod "ByLayer"
    pt '(0.0 0.0 0.0)
  )
  (if (= "Expert" (cond ((initget "Expert Auto")) ((getkword "\nComment souhaitez-vous utiliser le programme ? [Expert/Auto] <Auto> ")) ("Auto")))
    (setq
      len (cond ((initget (apply '+ '(2 4 64 1024)))) ((getdist (strcat "\nLongueur des lignes <" (rtos len) "> : "))) (len))
      rng (cond ((initget (apply '+ '(2 4 64 1024)))) ((getdist (strcat "\nEcart entre chaque ligne <" (rtos rng) "> : "))) (rng))
      htx (cond ((initget (apply '+ '(2 4)))) ((getreal (strcat "\nHauteur de texte <" (rtos htx) "> : "))) (htx))
      ech (cond ((initget (apply '+ '(2 4)))) ((getreal (strcat "\nEchelle du type de ligne <" (rtos ech) "> : "))) (ech))
      mod
        (cond
          ((initget "ByLayer Forced"))
          ((getkword (strcat "\nSouhaitez-vous affecter les calques aux lignes ou forcer les propriétés sur le calque courant [ByLayer/Forced] ? <" mod "> ")))
          (mod)
        )
      pt (cond ((getpoint (strcat "\nPoint d'insertion de la liste <" (rtos (car pt)) "," (rtos (cadr pt)) "," (rtos (caddr pt)) "> :"))) (pt))
    )
  )
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    layers (vla-get-layers doc)
    ms (vla-get-modelspace doc)
    i 0
    lck (vlax-get (setq layer (vla-item layers (getvar "CLAYER"))) 'Lock)
  )
  (vla-StartUndoMark doc)
  (if (= lck -1) (vlax-put layer 'Lock 0))
  (vlax-for layer layers
    (setq
      coe (* i rng)
      spt (mapcar '+ pt (list 0 (+ coe (* 0.5 htx)) 0))
      ept (mapcar '+ pt (list len (+ coe (* 0.5 htx)) 0))
      tpt (mapcar '+ pt (list (+ len (* 0.5 rng)) coe 0))
      obj (vla-AddLine ms (vlax-3d-point spt) (vlax-3d-point ept))
      txt (vla-AddText ms (vlax-get layer 'Name) (vlax-3d-point tpt) htx)
      i (1+ i)
    )
    (cond
      ( (= "ByLayer" mod)
        (vlax-put obj 'LinetypeScale ech)
        (vlax-put obj 'Layer (vlax-get layer 'Name))
      )
      ( (= "Forced" mod)
        (vlax-put obj 'TrueColor (vlax-get layer 'TrueColor))
        (vlax-put obj 'Lineweight (vlax-get layer 'Lineweight))
        (vlax-put obj 'Linetype (vlax-get layer 'Linetype))
        (vlax-put obj 'LinetypeScale ech)
      )
    )
  )
  (if (= lck -1) (vlax-put layer 'Lock -1))
  (vla-ZoomWindow (vlax-get-acad-object) (vlax-3d-point pt) (vlax-3d-point tpt))
  (princ
    (strcat
      "\nUn total de "
      (itoa i)
      " calques ont été listé au point d'insertion : "
      (vl-princ-to-string pt)
    )
  )
  (vla-EndUndoMark doc)
  (princ)
)