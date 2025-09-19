(vl-load-com)
(defun c:dyn_read_xdata ( / AcDoc Space UCS save_ucs WCS nw_obj ent_text dxf_ent apps lst_apps data ncol strcatlst Input obj_sel ename)
  (setq
    AcDoc (vla-get-ActiveDocument (vlax-get-acad-object))
    Space
    (if (= 1 (getvar "CVPORT"))
      (vla-get-PaperSpace AcDoc)
      (vla-get-ModelSpace AcDoc)
    )
    UCS (vla-get-UserCoordinateSystems AcDoc)
    save_ucs
    (vla-add UCS
      (vlax-3d-point '(0.0 0.0 0.0))
      (vlax-3d-point (getvar "UCSXDIR"))
      (vlax-3d-point (getvar "UCSYDIR"))
      "CURRENT_UCS"
    )
  )
  (vla-put-Origin save_ucs (vlax-3d-point (getvar "UCSORG")))
  (setq WCS (vla-add UCS (vlax-3d-Point '(0.0 0.0 0.0)) (vlax-3d-Point '(1.0 0.0 0.0)) (vlax-3d-Point '(0.0 1.0 0.0)) "TEMP_WCS"))
  (vla-put-activeUCS AcDoc WCS)
  (setq
    nw_obj
    (vla-addMtext Space
      (vlax-3d-point (trans (getvar "VIEWCTR") 1 0))
      0.0
      ""
    )
  )
  (mapcar
    '(lambda (pr val)
      (vlax-put nw_obj pr val)
    )
    (list 'AttachmentPoint 'Height 'DrawingDirection 'StyleName 'Layer 'Rotation 'BackgroundFill 'Color)
    (list 1 (/ (getvar "VIEWSIZE") 100.0) 5 (getvar "TEXTSTYLE") (getvar "CLAYER") 0.0 -1 176)
  )
  (setq
    ent_text (entlast)
    dxf_ent (entget ent_text)
    dxf_ent (subst (cons 90 1) (assoc 90 dxf_ent) dxf_ent)
    dxf_ent (subst (cons 63 254) (assoc 63 dxf_ent) dxf_ent)
  )
  (entmod dxf_ent)
  (while (and (setq Input (grread T 4 2)) (= (car Input) 5))
    (cond
      ((setq obj_sel (nentselp (cadr Input)))
        (if (eq (type (car (last obj_sel))) 'ENAME)
          (if (member (cdr (assoc 0 (entget (car (last obj_sel))))) '("INSERT" "ACAD_TABLE" "DIMENSION"))
            (if
              (or
                (eq (cdr (assoc 0 (entget (car (last obj_sel))))) "ACAD_TABLE")
                (not (eq (boole 1 (cdr (assoc 70 (tblsearch "BLOCK" (cdr (assoc 2 (entget (car (last obj_sel)))))))) 4) 4))
              )
              (setq obj_sel (cons (car (last obj_sel)) '((0.0 0.0 0.0))))
            )
          )
        )
        (setq
          dxf_ent (entget (car obj_sel) (list "*"))
        )
        (if (eq (cdr (assoc 0 dxf_ent)) "VERTEX")
          (progn
            (while (eq (cdr (assoc 0 dxf_ent)) "VERTEX")
              (setq dxf_ent (entget (entnext (cdar dxf_ent))))
            )
            (setq dxf_ent (entget (cdr (assoc -2 dxf_ent)) (list "*")))
          )
        )
        (setq
          apps (cdr (assoc -3 dxf_ent))
          ncol 0
          lst_apps nil
        )
        (if apps
          (foreach el apps
            (if (not (member (car el) lst_apps)) (setq lst_apps (cons (car el) lst_apps)))
          )
        )
        (if lst_apps
          (foreach xd lst_apps
            (setq
              data (assoc xd apps)
              strcatlst
              (strcat
                (if strcatlst strcatlst "")
                (apply 'strcat
                  (mapcar
                    '(lambda (x)
                      (if (listp x)
                        (strcat
                          "("
                          (itoa (car x))
                          " . "
                          (cond
                            ((eq (car x) 1002) (strcat (if (eq (cdr x) "{") "\"(\"" "\")\"")))
                            ((member (car x) '(1000 1003 1004 1005)) (strcat "\"" (cdr x) "\""))
                            ((member (car x) '(1040 1041 1042)) (rtos (cdr x)))
                            ((member (car x) '(1070 1071)) (itoa (cdr x)))
                            ((member (car x) '(1010 1011 1012 1013 1020 1021 1022 1023 1030 1031 1032 1033)) (strcat "(" (rtos (cadr x)) "," (rtos (caddr x)) "," (rtos (cadddr x)) ")"))
                          )
                          ")\\P"
                        )
                        (strcat "{\\C" (itoa (setq ncol (+ 10 ncol))) " " (car data)"}" "\\P")
                      )
                    )
                    data
                  )
                )
              )
            )
          )
        )
        (if strcatlst
          (progn
            (mapcar 
              '(lambda (pr val)
                (vlax-put nw_obj pr val)
              )
              (list 'InsertionPoint 'Height 'TextString)
              (list (mapcar '- (getvar "VIEWCTR") (list (* (getvar "VIEWSIZE") 0.5) (- (* (getvar "VIEWSIZE") 0.5)) 0.0)) (/ (getvar "VIEWSIZE") 100.0) (strcat "{\\fArial;" strcatlst "}" ))
            )
          )
          (vlax-put nw_obj 'TextString "")
        )
        (setq strcatlst nil)
      )
      (T (vlax-put nw_obj 'TextString ""))
    )
  )
  (vla-Delete nw_obj)
  (and save_ucs (vla-put-activeUCS AcDoc save_ucs))
  (and WCS (vla-delete WCS) (setq WCS nil))
  (prin1)
)