;--- Author  : Luna
;--- Date    : 14/08/2022
;--- Version : 1.0.0
; Création d'un objet MText détaillant la longueur totale d'une polyligne 3D (champ dynamique), du nombre total de sommets (sauf extrémités) ainsi que
; la différence d'altitude (Z) entre les deux extrémités ignorées (Zdébut - Zfin). Le MText est rattaché à la polyligne 3D via (grread) pour suivre la
; courbe. La rotation du texte est alignée à l'axe X du SCU courant et les valeurs spécifiées dans le MText sont exprimées en mètres, quelque soit la
; valeur de "INSUNITS". Le MText est inséré sur le calque de la polyligne 3D (mais les propriétés de couleurs, transparence, etc... correspondent aux
; valeurs courantes pour les nouveaux objets).

(defun c:Poly3D_Max73 (/ get-poly-info cvunits set-MTEXT move-MTEXT name info layer htxt str text)
  (defun get-poly-info (ent / ob vs ve ps pe ID fa lg pt Dz)
    (if
      (and
        (setq ob (vlax-ename->vla-object ent))
        (setq vs (vlax-curve-getStartPoint ent))
        (setq ve (vlax-curve-getEndPoint ent))
        (setq ps (vlax-curve-getStartParam ent))
        (setq pe (vlax-curve-getEndParam ent))
        (setq ID (vla-get-ObjectID ob))
        (setq fa (getvar "INSUNITS"))
        (setq lg (strcat "%<\\AcObjProp.16.2 Object(%<\\_ObjId " (itoa ID) ">%).Length \\f \"%lu2%pr1%ct8[" (rtos (cvunits 1 fa 6)) "]%th32\">%"))
        (setq pt (1+ (fix pe)))
        (setq Dz (- (last ve) (last vs)))
      )
      (list lg pt (cvunits Dz fa 6))
    )
  )

  (defun cvunits (value from-unit to-unit / f)
    (defun f (u)
      (if (= (type u) 'INT)
        (cond
          ((= u 1) "inch")
          ((= u 2) "foot")
          ((= u 3) "mile")
          ((= u 4) "millimeter")
          ((= u 5) "centimeter")
          ((= u 6) "meter")
          ((= u 7) "kilometer")
          ((= u 8) "microinch")
          ((= u 9) "millipouce")
          ((= u 10) "yard")
          ((= u 11) "Angstrom")
          ((= u 12) "nanometer")
          ((= u 13) "micron")
          ((= u 14) "decimeter")
          ((= u 15) "dekameter")
          ((= u 16) "hectometer")
          ((= u 17) "gigameter")
          ((= u 18) "astronomical_unit")
          ((= u 19) "light_year")
          ((= u 20) "parsec")
          ((= u 21) "survey_foot")
        )
        u
      )
    )
    (cvunit value (f from-unit) (f to-unit))
  )

  (defun set-MTEXT (pt wd str ht rt ly / ms ob)
    (and
      (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq pt (vlax-3D-point pt))
      (setq ob (vla-AddMText ms pt wd str))
      (null (vlax-put ob 'Height ht))
      (null (vlax-put ob 'Rotation rt))
      (null (vlax-put ob 'Layer ly))
    )
    ob
  )

  (defun move-MTEXT (msg curve mtext / gr pt)
    (princ msg)
    (while (and (setq gr (grread T)) (= 5 (car gr)))
      (setq pt (trans (cadr gr) 1 0))
      (setq pt (vlax-curve-getClosestPointTo curve pt))
      (vla-move mtext (vla-get-InsertionPoint mtext) (vlax-3D-point pt))
    )
    mtext
  )

  (and
    (setq name (entsel "\nSélectionner une polyligne 3D : "))
    (setq name (car name))
    (= "POLYLINE" (cdr (assoc 0 (entget name))))
    (setq info (get-poly-info name))
    (setq layer (cdr (assoc 8 (entget name))))
    (null (initget 6))
    (or
      (setq htxt (getreal (strcat "\nSpécifier la hauteur de texte <" (rtos (getvar "TEXTSIZE")) ">: ")))
      (setq htxt (getvar "TEXTSIZE"))
    )
    (setq str
      (strcat
        "Lg = " (car info) "m"
        "\n"
        "So = " (itoa (- (cadr info) 2)) "u"
        "\n"
        "Al = " (rtos (caddr info) 2 1) "m"
      )
    )
    (setq text (set-MTEXT '(0.0 0.0 0.0) (* htxt 10) str htxt 0.0 layer))
    (move-MTEXT "\nSélectionner un point d'insertion : " name text)
  )
  (princ)
)