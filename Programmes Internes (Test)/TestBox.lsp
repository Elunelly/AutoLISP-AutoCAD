(defun TestBox1 (title label tile add value width height / *error* filename file DCL_ID rslt)
  (defun *error* (msg)
    (if file (close file))
    (if filename (vl-file-delete filename))
    (princ msg)
  )
  (setq
    filename (vl-filename-mktemp "TestBox.dcl")
    file (open filename "W")
  )
  (write-line
    (strcat
      "TestBox:dialog {"
      "   label = \"" title "\" ;"
      (if width (strcat "   width = " (rtos width) " ;") "")
      (if height (strcat "   height = " (rtos height) " ;") "")
      "   :" tile " {"
      (if label (strcat "     label = \"" label "\" ;") "")
      "     key = \"key\" ;"
      (if add (apply 'strcat add) "")
      "   }"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog filename))
  (if (not (new_dialog "TestBox" DCL_ID))
    (exit)
  )
  (if value (set_tile "key" value))
  (action_tile "Accept" "(setq rslt (get_tile \"key\")) (done_dialog)")
  (action_tile "Cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete filename)
  rslt
)

(defun TestBox2 (title label tile add value width height / *error* filename file DCL_ID rslt)
  (defun *error* (msg)
    (if file (close file))
    (if filename (vl-file-delete filename))
    (princ msg)
  )
  (setq
    filename (vl-filename-mktemp "TestBox.dcl")
    file (open filename "W")
  )
  (write-line
    (strcat
      "TestBox:dialog {"
      "   label = \"" title "\" ;"
      "   :" tile " {"
      (if width (strcat "     width = " (rtos width) " ;") "")
      (if height (strcat "     height = " (rtos height) " ;") "")
      (if label (strcat "     label = \"" label "\" ;") "")
      "     key = \"key\" ;"
      (if add (apply 'strcat add) "")
      "   }"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog filename))
  (if (not (new_dialog "TestBox" DCL_ID))
    (exit)
  )
  (if value (set_tile "key" value))
  (action_tile "Accept" "(setq rslt (get_tile \"key\")) (done_dialog)")
  (action_tile "Cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete filename)
  rslt
)

(defun TestBox3 (/ *error* filename file DCL_ID rslt)
  (defun *error* (msg)
    (if file (close file))
    (if filename (vl-file-delete filename))
    (princ msg)
  )
  (setq
    filename (vl-filename-mktemp "TestBox.dcl")
    file (open filename "W")
  )
  (write-line
    (strcat
      "TestBox:dialog {"
      "   label = \"DCL Test\" ;"
      "   :edit_box {"
      "     label = \"Displayed text\" ;"
      "     key = \"key\" ;"
      "   }"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog filename))
  (if (not (new_dialog "TestBox" DCL_ID))
    (exit)
  )
  (action_tile "Accept" "(progn (setq rslt (get_tile \"key\")) (done_dialog))")
  (action_tile "Cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete filename)
  rslt
)

(defun TestBox4 (lst / *error* filename file DCL_ID rslt)
  (defun *error* (msg)
    (if file (close file))
    (if filename (vl-file-delete filename))
    (princ msg)
  )
  (setq
    filename (vl-filename-mktemp "TestBox.dcl")
    file (open filename "W")
  )
  (write-line
    (strcat
      "TestBox:dialog {"
      "   label = \"DCL Test\" ;"
      "   :list_box {"
      "     label = \"message :\" ;"
      "     key = \"key\" ;"
      "     multiple_select = true ;"
      "   }"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog filename))
  (if (not (new_dialog "TestBox" DCL_ID))
    (exit)
  )
  (start_list "key")
  (mapcar 'add_list lst)
  (end_list)
  (action_tile "accept" "(setq rslt (mapcar '(lambda (n) (nth (atoi n) lst)) (str2lst (get_tile \"key\") \" \"))) (done_dialog)")
  (action_tile "Cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete filename)
  rslt
)

(defun willy95 (lst / *error* filename file DCL_ID rslt)
  (defun *error* (msg)
    (if file (close file))
    (if filename (vl-file-delete filename))
    (princ msg)
  )
  (setq
    filename (vl-filename-mktemp "TestBox.dcl")
    file (open filename "W")
  )
  (write-line
    (strcat
      "TestBox:dialog {"
      "   label = \"1, 2 ou 3 Choix \" ;"
      "   spacer ;"
      "   :list_box {"
      "     label = \"Choisir : \" ;"
      "     key = \"key\" ;"
      "     width = 1 ;"
      "     height = 2 ;"
      "     edit_width = 25 ;"
      "     edit_height = 27 ;"
      "     multiple_select = true ;"
      "   }"
      "   spacer ;"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog filename))
  (if (not (new_dialog "TestBox" DCL_ID))
    (exit)
  )
  (start_list "key")
  (mapcar 'add_list lst)
  (end_list)
  (action_tile "accept" "(setq rslt (mapcar '(lambda (n) (nth (atoi n) lst)) (str2lst (get_tile \"key\") \" \"))) (done_dialog)")
  (action_tile "Cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete filename)
  rslt
)

(defun c:TestBox (/ DCL_ID end)
  (setq DCL_ID (load_dialog "TestBox.dcl"))
  (if (not (new_dialog "TestBox" DCL_ID))
    (exit)
  )
  (action_tile "accept" "(setq end (get_tile \"key\")) (done_dialog 0)")
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog DCL_ID)
  end
)

(defun ListBox2 (title msg lst f value flag / vl-list-search tmp file DCL_ID choice tlst)

  (defun vl-list-search (p l)
    (vl-remove-if-not '(lambda (x) (wcmatch x p)) l)
  )
  (setq tmp (vl-filename-mktemp "tmp.dcl")
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
      ((= 0 flag) "spacer;:popup_list{key=\"lst\";is_enabled=false;}")
      ((= 1 flag) "spacer;:list_box{height=15;key=\"lst\";is_enabled=false;}")
      (t "spacer;:list_box{height=15;key=\"lst\";is_enabled=false;multiple_select=true;}")
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
  (mapcar 'f lst)
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
;;      ((itoa 0))
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