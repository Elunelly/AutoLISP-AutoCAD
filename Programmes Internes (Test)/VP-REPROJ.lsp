(defun c:VP-REPROJ (/ *error* vp-Op2Np doc ctab pto ptn v jsel i name n ptv lst)
  (defun *error* (msg)
    (vla-EndUndoMark doc)
    (setvar "CTAB" ctab)
    (princ msg)
  )
  (defun vp-Op2Np (vp vt / vc tb vl dl pt)
    (defun vc (v / e s c)
      (setq
        e (getvar "CMDECHO")
        s (getvar "VIEWSIZE")
        c (getvar "VIEWCTR")
        c (mapcar '+ c v)
      )
      (setvar "CMDECHO" 0)
      (command-s "_PLAN" "")
      (command-s "_ZOOM" "C" c s)
      (setvar "CMDECHO" e)
      c
    )
    (and
      (setq tb (cdr (assoc 410 (entget vp))))
      (setvar "CTAB" tb)
      (setq vl (vlax-ename->VLA-Object vp))
      (null (command-s "_PSPACE"))
      (null (command-s "_-VPORTS" "_ON" vp ""))
      (setq dl (vla-get-DisplayLocked vl))
      (null (command-s "_MSPACE"))
      (null (vla-put-DisplayLocked vl :vlax-false))
      (setq pt (vc vt))
      (null (command-s "_PSPACE"))
      (null (vla-put-DisplayLocked vl dl))
    )
    pt
  )
  (and
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (null (vla-StartUndoMark doc))
    (setq ctab (getvar "CTAB"))
    (setvar "CTAB" "Model")
    (setq pto (getpoint "\nOld projection point reference : "))
    (setq ptn (getpoint pto "\nNew projection point reference : "))
    (setq pto (trans pto 1 0))
    (setq ptn (trans ptn 1 0))
    (setq v (mapcar '- ptn pto))
    (setq jsel (ssget "_X" '((0 . "VIEWPORT"))))
    (repeat (setq n 0 i (sslength jsel))
      (setq name (ssname jsel (setq i (1- i))))
      (and
        (= :vlax-true (vla-get-UCSIconAtOrigin (vlax-ename->VLA-Object name)))
        (setq ptv (vp-Op2Np name v))
        (setq n (1+ n))
        (setq lst (make-a-list-properties lst (list (cdr (assoc 410 (entget name))) (cdr (assoc 5 (entget name)))) ptv 'cons T))
      )
      lst
    )
    (princ
      (strcat
        "\nUn total de "
        (itoa n)
        " / "
        (itoa (sslength jsel))
        " VIEWPORT ont été modifiées avec le vecteur "
        (vl-princ-to-string v)
        ". Ci-dessous la liste détaillée :"
        "\n - "
        (lst2str
          (mapcar
            '(lambda (lay / tmp)
              (setq
                tmp (cdr lay)
                lay (car lay)
                tmp (mapcar '(lambda (x / h p) (setq h (car x) p (cdr x)) (strcat h " " (vl-prin1-to-string p))) tmp)
              )
              (strcat
                "\"" lay "\""
                " -> "
                (lst2str tmp ", ")
              )
             )
            lst
          )
          "\n - "
        )
      )
    )
    (null (vla-EndUndoMark doc))
  )
  (princ)
)