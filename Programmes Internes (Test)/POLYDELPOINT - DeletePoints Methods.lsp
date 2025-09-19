(defun Poly3D-DeletePoints (obj fun / pts n i ind rmn sup r)
  (and
    (= "AcDb3dPolyline" (vla-get-ObjectName obj))
    (setq pts (vlax-get obj 'Coordinates))
    (setq pts (divlist pts 3))
    (setq n (length pts) i -1)
    (setq ind (mapcar 'eval (mAtom '(setq i (1+ i)) (length pts) 'list)))
    (setq rmn (vl-remove-if fun pts))
    (setq pts (mapcar 'cons ind pts))
    (setq sup (vl-remove-if '(lambda (x) (member (cdr x) rmn)) pts))
    (setq r
      (cond
        ( (= n (length rmn)))
        ( (< 1 (length rmn))
          (vlax-put obj 'Coordinates (apply 'append rmn))
          sup
        )
        ( (null (vla-delete obj)) obj)
      )
    )
  )
  r
)

(defun Poly3D-DeletePoints (obj fun / pts n i ind rmn sup r)
  (if (= "AcDb3dPolyline" (vla-get-ObjectName obj))
    (progn
      (setq pts (vlax-get obj 'Coordinates))
      (setq pts (divlist pts 3))
      (setq n (length pts) i -1)
      (setq ind (mapcar 'eval (mAtom '(setq i (1+ i)) (length pts) 'list)))
      (setq rmn (vl-remove-if fun pts))
      (setq pts (mapcar 'cons ind pts))
      (setq sup (vl-remove-if '(lambda (x) (member (cdr x) rmn)) pts))
      (cond
        ( (= n (length rmn)))
        ( (< 1 (length rmn))
          (vlax-put obj 'Coordinates (apply 'append rmn))
          sup
        )
        ( (null (vla-delete obj)) obj)
      )
    )
    (vla-get-ObjectName obj)
  )
)

(defun Poly-DeletePoints (obj fun / LWPOLY_RemoveNthPoints obn d pts n i ind rmn sup r)
  (defun LWPOLY_RemoveNthPoints (lst n / pos pre pts tmp i rtn)
    (setq
      pos (vl-position (assoc 10 lst) lst)
      pre (sublist lst 1 pos)
      pts (sublist (reverse (cdr (reverse lst))) (1+ pos) nil)
      tmp (divlist pts 5)
      i -1
      n (cond ((listp n) n) ((list n)))
    )
    (foreach pt tmp
      (cond
        ( (member (setq i (1+ i)) n))
        ( (member (1+ i) n) (setq rtn (cons (subst '(42 . 0.0) (assoc 42 pt) pt) rtn)))
        ( (setq rtn (cons pt rtn)))
      )
    )
    (if (< 2 (length rtn))
      (entmod (append (subst (cons 90 (1- (length rtn))) (assoc 90 pre) pre) (apply 'append (reverse rtn)) (list (last lst))))
    )
  )
  
  (setq obn (vla-get-ObjectName obj))
  (cond
    ( (= "AcDb3dPolyline" obn) (setq d 3))
    ( (= "AcDbPolyline" obn) (setq d 2))
  )
  (if d
    (progn
      (setq pts (vlax-get obj 'Coordinates))
      (setq pts (divlist pts d))
      (setq n (length pts) i -1)
      (setq ind (mapcar 'eval (mAtom '(setq i (1+ i)) (length pts) 'list)))
      (setq rmn (vl-remove-if fun pts))
      (setq pts (mapcar 'cons ind pts))
      (setq sup (vl-remove-if '(lambda (x) (member (cdr x) rmn)) pts))
      (cond
        ( (= n (length rmn)))
        ( (and
            (< 1 (length rmn))
            (cond
              ( (= "AcDb3dPolyline" obn)
                (vlax-put obj 'Coordinates (apply 'append rmn))
              )
              ( (= "AcDbPolyline" obn)
                (LWPOLY_RemoveNthPoints (entget (vlax-VLA-Object->ename obj)) (mapcar 'car sup))
              )
            )
          )
          sup
        )
        ( (null (vla-delete obj)) obj)
      )
    )
    ( (vla-get-ObjectName obj))
  )
)

(defun PolyLW-DeletePoints (obj fun / pts n i ind rmn blg sup r)
  (if (= "AcDbPolyline" (vla-get-ObjectName obj))
    (progn
      (setq pts (vlax-get obj 'Coordinates))
      (setq pts (divlist pts 2))
      (setq n (length pts) i -1)
      (setq ind (mapcar 'eval (mAtom '(setq i (1+ i)) (length pts) 'list)))
      (setq rmn (vl-remove-if fun pts))
      (setq pts (mapcar 'cons ind pts))
      (setq blg (mapcar '(lambda (x) (vla-GetBulge obj x)) ind))
      (setq sup (vl-remove-if '(lambda (x) (member (cdr x) rmn)) pts))
      (setq ind (mapcar '(lambda (x) (if (zerop (car x)) (car x) (1- (car x)))) sup))
      (setq i -1)
      (setq blg (vl-remove-if '(lambda (b) (setq i (1+ i)) (member i ind)) blg))
      (cond
        ( (= n (length rmn)))
        ( (< 1 (length rmn))
          (vlax-put obj 'Coordinates (apply 'append rmn))
          (setq i -1)
          (foreach b blg (vla-SetBulge obj (setq i (1+ i)) b))
          sup
        )
        ( (null (vla-delete obj)) obj)
      )
    )
    (vla-get-ObjectName obj)
  )
)


(defun c:vtx-del (/ bulges coords ent idx param pt)
  (vl-load-com)
  (defun removenth (n lst / i rtn)
    (reverse
      (progn
        (setq i -1)
        (foreach x lst
          (if (/= n (setq i (1+ i)))
            (setq rtn (cons x rtn))
          )
        )
        rtn
      )
    )
  )
  
  (command "undo" "be")
  (while 
    (and
      (setq ent (entsel "\nSelect vertex to remove: "))
      (eq (cdr (assoc 0 (entget (car ent)))) "LWPOLYLINE")
      (setq
        pt (osnap (cadr ent) "near")
        ent (vlax-ename->vla-object (car ent))
      )
    )
    (setq param (atoi (rtos (vlax-curve-getparamatpoint ent pt) 2 0)))
    (setq
      coords (vlax-get ent 'coordinates)
      idx -1
      bulges nil
    )
    (repeat (/ (length coords) 2)
      (setq bulges (cons (vla-getbulge ent (setq idx (1+ idx))) bulges))
    )
    (setq bulges (removenth param (reverse bulges)))
    (repeat 2
      (setq coords (removenth (* 2 param) coords))
    )
    (vlax-put ent 'coordinates coords)
    (setq idx -1)
    (foreach bulge bulges
      (vla-setbulge ent (setq idx (1+ idx)) bulge)
    )
  )
  (command "undo" "end")
  (princ)
)


(defun grsnap:circle (msg r / *error* CrosshairColor->RGB osf osm str ce ms pt ci ho co cr gr gd tmp)
  (defun *error* (msg)
    (if ci (vla-delete ci))
    (if ho (vla-delete ho))
    (setvar "CMDECHO" ce)
    (princ msg)
  )
  (defun CrosshairColor->RGB (/ cr oc )
    (setq
      cr (vla-get-ModelCrosshairColor (vla-get-Display (vla-get-Preferences (vlax-get-acad-object))))
      oc (vlax-variant-value (vlax-variant-change-type cr vlax-vbLong))
    )
    (LM:OLE->RGB oc)
  )

  (if (< 0 r)
    (progn
      (setq
        osf (LM:grsnap:snapfunction)
        osm (getvar "OSMODE")
        str ""
        msg (cond (msg) (""))
      )
      (setq
        ce (getvar "CMDECHO")
        ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
        pt (vlax-3D-point '(0.0 0.0 0.0))
        ci (vla-addCircle ms pt r)
        ho (vla-AddHatch ms acHatchPatternTypePredefined "SOLID" :vlax-false acHatchObject)
        co (vla-get-TrueColor ci)
        cr (CrosshairColor->RGB)
      )
      (setvar "CMDECHO" 0)
      (vlax-invoke ho 'AppendOuterLoop (list ci))
      (vla-Evaluate ho)
      (apply 'vla-setRGB (cons co cr))
      (vla-put-TrueColor ci co)
      (vla-put-TrueColor ho co)
      (vlax-put ci 'EntityTransparency "0")
      (vlax-put ho 'EntityTransparency "50")
      (vlax-put ci 'LineType "Continuous")
      (setvar "CMDECHO" ce)
      (princ msg)
      (while
        (progn
          (setq
            gr (grread T 15 0)
            gd (cadr gr)
            gr (car gr)
          )
          (cond
            ( (or (= 5 gr) (= 3 gr))
              (redraw)
              (setq pt (vlax-3D-Point (trans (setq gd (osf gd osm)) 1 0)))
              (vla-move ho (vla-get-Center ci) pt)
              (vla-move ci (vla-get-Center ci) pt)
              gd
              (= 5 gr)
            )
            ( (= 2 gr)
              (cond
                ( (= 6 gd)
                  (if (zerop (logand 16384 (setq osm (setvar "OSMODE" (boole 6 16384 (getvar "OSMODE"))))))
                    (princ (LgT "\n<Osnap on>" "\n<Accrobj actif>" nil))
                    (princ (LgT "\n<Osnap off>" "\n<Accrobj inactif>" nil))
                  )
                  (princ msg)
                )
                ( (= 8 gd)
                  (if (< 0 (strlen str))
                    (progn
                      (princ "\010\040\010")
                      (setq str (substr str 1 (1- (strlen str))))
                    )
                  )
                  T
                )
                ( (< 32 gd 127)
                  (setq str (strcat str (princ (chr gd))))
                )
                ( (member gd '(13 32))
                  (cond
                    ( (= "" str) nil)
                    ( (setq gd (LM:grsnap:parsepoint pt str))
                      (setq osm 16384)
                      nil
                    )
                    ( (setq tmp (LM:grsnap:snapmode str))
                      (setq
                        osm tmp
                        str ""
                      )
                    )
                    ( (setq str "")
                      (princ (LgT "\n2D / 3D Point Required." "\nPoint 2D / 3D requis." nil))
                      (princ msg)
                    )
                  )
                )
              )
            )
          )
        )
      )
      (setq ci (vla-delete ci))
      (setq ho (vla-delete ho))
      (redraw)
      (if (listp gd) gd)
    )
    (apply 'getpoint msg)
  )
)