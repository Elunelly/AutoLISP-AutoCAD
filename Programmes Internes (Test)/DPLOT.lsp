(defun c:DPLOT (/ *error* BrowseForFolder ListBox fcd fid dwg doc sum lay fol rev fil)
  (defun *error* (msg)
    (setvar "FILEDIA" fid)
    (princ msg)
  )
  (defun BrowseForFolder (/ ShlObj Folder FldObj Out_Fld) ; BonusCAD
    (setq
      ShlObj (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application")
      Folder (vlax-invoke-method ShlObj 'BrowseForFolder 0 "" 0)
    )
    (vlax-release-object ShlObj)
    (if Folder
      (progn
        (setq
          FldObj (vlax-get-property Folder 'Self)
          Out_Fld (vlax-get-property FldObj 'Path)
        )
        (vlax-release-object Folder)
        (vlax-release-object FldObj)
      )
    )
    Out_Fld
  )
  (defun ListBox (title msg lst value flag / tmp file DCL_ID choice tlst)
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
        ((= 0 flag) "spacer;:popup_list{key=\"lst\";}")
        ((= 1 flag) "spacer;:list_box{height=15;key=\"lst\";}")
        (t "spacer;:list_box{height=15;key=\"lst\";multiple_select=true;}")
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
    (set_tile "lst" (if (member value lst) (itoa (vl-position value lst)) (itoa 0)))
    (action_tile
      "filter"
      "(start_list \"lst\")
      (mapcar 'add_list (setq tlst (vl-list-search $value lst)))
      (end_list)
      (set_tile \"count\" (strcat (itoa (length tlst)) \" / \" (itoa (length lst))))"
    )
    (action_tile
        "accept"
        "(or 	(= (get_tile \"lst\") \"\")
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
  
  (and
    (setq
      fcd "" ; "DWG To PDF.pc3" -> force une imprimante | "" = risque accru car les questions sont différentes en fonction des imprimantes si pas PDF
      fid (getvar "FILEDIA")
      dwg (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))
      doc (vla-get-ActiveDocument (vlax-get-acad-object))
      sum (vla-get-SummaryInfo doc)
    )
    (setq lay (ListBox "Sélection des présentations" "Veuillez sélectionner une ou plusieurs présentations :" (layoutlist) (getvar "CTAB") 2))
    (setq fol (BrowseForFolder))
    (or (vl-catch-all-apply 'vla-GetCustomByKey (list sum "REV_EXE" 'rev)) T) ; <-- Remplacer "REV_EXE" par le nom de la propriété personnalisée
    (setq rev (if rev (strcat "_" rev) ""))
    (setvar "FILEDIA" 0)
    (foreach layout lay
      (setq fil (strcat fol "\\" dwg "-" layout rev ".pdf"))
      (command-s "_-PLOT" "_No" layout "" fcd fil "_No" "_Yes")
      T
    )
    (setvar "FILEDIA" fid)
  )
  (princ)
)