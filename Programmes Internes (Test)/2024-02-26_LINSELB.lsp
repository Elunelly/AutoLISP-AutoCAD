(defun c:LINSELB (/ get-pt-list blc mode jsel i name pt-lst tmp n obj)
  (defun get-pt-list (name / rp f o s e i l)
    (defun rp (p f)
      (mapcar '(lambda (c) (if (equal c 0.0 f) 0.0 c)) p)
    )
    
    (and
      name
      (setq f 1e-15)
      (setq o (cdr (assoc 0 (entget name))))
      (member o '("LWPOLYLINE" "POLYLINE" "LINE" "SPLINE" "ARC" "CIRCLE" "ELLIPSE"))
      (setq s (vlax-curve-getStartParam name))
      (setq e (vlax-curve-getEndParam name))
      (cond
        ( (member o '("LWPOLYLINE" "POLYLINE"))
          (repeat (setq i (1+ (fix e)))
            (setq l (cons (rp (vlax-curve-getPointAtParam name (setq i (1- i))) f) l))
          )
        )
        ( (member o '("LINE" "SPLINE"))
          (setq l
            (list
              (rp (vlax-curve-getPointAtParam name s) f)
              (rp (vlax-curve-getPointAtParam name e) f)
            )
          )
        )
        ( (member o '("ARC" "CIRCLE" "ELLIPSE"))
          (setq l
            (list
              (rp (cdr (assoc 10 (entget name))) f)
              (rp (vlax-curve-getPointAtParam name s) f)
              (rp (vlax-curve-getPointAtParam name e) f)
            )
          )
        )
      )
    )
    l
  )
  (and
    (setq blc (ssadd))
    (not (initget "Sommet Trajet"))
    (setq mode (cond ((getkword "\nSélectionner les blocs selon [Sommet/Trajet] <Sommet> : ")) ("Sommet")))
    (setq jsel (ssget '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        pt-lst (get-pt-list name)
        tmp (ssget "_F" pt-lst '((0 . "INSERT")))
      )
      (repeat (setq n (sslength tmp))
        (setq
          obj (ssname tmp (setq n (1- n)))
          pt (cdr (assoc 10 (entget obj)))
        )
        (cond
          ( (= mode "Trajet")
            (ssadd obj blc)
          )
          ( (= mode "Sommet")
            (if (member T (mapcar '(lambda (p) (equal p pt 10e-2)) pt-lst))
              (ssadd obj blc)
            )
          )
        )
      )
      blc
    )
    (not (sssetfirst))
    (princ
      (strcat
        "\nUn total de "
        (itoa (sslength blc))
        " blocs ont été sélectionnés à partir des "
        (itoa (sslength jsel))
        " polylignes/lignes sélectionnés."
      )
    )
    (sssetfirst nil blc)
  )
  (princ)
)

(defun c:ChangeLayerBlockOnVertex (/ str2lst lst2str ListBox vla-collection->list nam_blk jsel ss_blk ss_poly i ent_poly nam_lay lst_pt n ent_blk dxf_blk pt name)
  (defun str2lst (str sep / pos)
    (if (setq pos (vl-string-search sep str))
      (cons
        (substr str 1 pos)
        (str2lst (substr str (+ (strlen sep) pos 1)) sep)
      )
      (list str)
    )
  )
  (defun lst2str (lst sep)
    (if lst
      (vl-string-left-trim
        sep
        (apply
          'strcat
          (mapcar '(lambda (x) (strcat sep (vl-princ-to-string x))) lst)
        )
      )
    )
  )
  (defun ListBox (title msg lst value flag h / vl-list-search LB-select tmp file DCL_ID choice tlst)
    (defun vl-list-search (p l)
      (vl-remove-if-not '(lambda (x) (wcmatch x p)) l)
    )
    
    (defun LB-select (str)
      (if (= "" str)
        "0 selected"
        (strcat (itoa (length (str2lst str " "))) " selected")
      )
    )
    
    (setq
      tmp (vl-filename-mktemp "tmp.dcl")
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
        ( (= 0 flag) "spacer;:popup_list{key=\"lst\";}")
        ( (= 1 flag) (strcat "spacer;:list_box{height=" (itoa (1+ (cond (h) (15)))) ";key=\"lst\";}"))
        ( T (strcat "spacer;:list_box{height=" (itoa (1+ (cond (h) (15)))) ";key=\"lst\";multiple_select=true;}:text{key=\"select\";}"))
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
    (set_tile
      "lst"
      (cond
        ( (and
            (= flag 2)
            (listp value)
          )
          (apply 'strcat (vl-remove nil (mapcar '(lambda (x) (if (member x lst) (strcat (itoa (vl-position x lst)) " "))) value)))
        )
        ( (member value lst) (itoa (vl-position value lst)))
        ( (itoa 0))
      )
    )
    (if (= flag 2)
      (progn
        (set_tile "select" (LB-select (get_tile "lst")))
        (action_tile "lst" "(set_tile \"select\" (LB-select $value))")
      )
    )
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
  (defun vla-collection->list (doc col flag / lst item i)
    (if
      (null
        (vl-catch-all-error-p
          (setq
            i 0
            col (vl-catch-all-apply 'vlax-get (list (cond (doc) ((vla-get-activedocument (vlax-get-acad-object)))) col))
          )
        )
      )
      (vlax-for item col
        (setq lst
          (cons
            (cons
              (if (vlax-property-available-p item 'Name)
                (vla-get-name item)
                (strcat "Unnamed_" (itoa (setq i (1+ i))))
              )
              (cond
                ( (= flag 0) (vlax-vla-object->ename item))
                (item)
              )
            )
            lst
          )
        )
      )
    )
    (reverse lst)
  )
  (and
    (setq nam_blk (list "DETECT")) ;; <-- Entrer le(s) nom(s) des blocs pour le filtre de sélection
    (setq nam_blk
      (ListBox
        "Sélection du/des bloc(s)"
        "Veuillez sélectionner un ou plusieurs bloc(s) :"
        (vl-sort (vl-remove-if '(lambda (x) (wcmatch x "`**")) (mapcar 'car (vla-collection->list nil 'blocks 1))) '<)
        nam_blk
        2
        nil
      )
    )
    (setq jsel (ssadd))
    (setq ss_blk (ssget "_X" (list '(0 . "INSERT") (cons 2 (strcat (lst2str nam_blk ",") ",`*U*")))))
    (setq ss_poly (ssget '((0 . "LWPOLYLINE"))))
    (repeat (setq i (sslength ss_poly))
      (setq
        ent_poly (ssname ss_poly (setq i (1- i)))
        nam_lay (assoc 8 (entget ent_poly))
        lst_pt (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) (entget ent_poly)))
      )
      (repeat (setq n (sslength ss_blk))
        (setq
          ent_blk (ssname ss_blk (setq n (1- n)))
          dxf_blk (entget ent_blk)
          pt (cdr (assoc 10 dxf_blk))
          name (vla-get-EffectiveName (vlax-ename->vla-object ent_blk))
        )
        (mapcar
          '(lambda (x)
            (if
              (and
                (member name nam_blk)
                (equal (list (car pt) (cadr pt)) (list (car x) (cadr x)) 1E-08)
              )
              (progn
                (entmod (subst nam_lay (assoc 8 dxf_blk) dxf_blk))
                (ssadd ent_blk jsel)
              )
            )
          )
          lst_pt
        )
        T
      )
    )
    (null (sssetfirst))
    (princ (strcat "\nUn total de " (itoa (sslength jsel)) "/" (itoa (sslength ss_blk)) " bloc(s) nommé(s) \"" (lst2str nam_blk ", ") "\" ont été modifié(s)"))
    (sssetfirst nil jsel)
  )
  (princ)
)