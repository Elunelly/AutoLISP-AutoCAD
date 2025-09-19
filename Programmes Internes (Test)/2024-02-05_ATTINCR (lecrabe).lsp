(defun c:ATTINCR (/ lst2str str2lst getkdh LgT LM:vl-setattributevalue break att val pas pre suf mode jsel i name)
  (vl-load-com)
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
  (defun str2lst (str sep / pos)
    (if (setq pos (vl-string-search sep str))
      (cons
        (substr str 1 pos)
        (str2lst (substr str (+ (strlen sep) pos 1)) sep)
      )
      (list str)
    )
  )
  (defun getkdh (fun pfx arg sfx dft hlp / get bit kwd msg val)
    (defun get (msg / v)
      (apply 'initget arg)
      (if (null (setq v (apply (car fun) (vl-remove nil (mapcar '(lambda (x) (if (vl-symbolp x) (vl-symbol-value x) x)) (cdr fun))))))
        (setq v (cdr dft))
        v
      )
    )
    
    (and
      (member (car fun) (list 'getint 'getreal 'getdist 'getangle 'getorient 'getpoint 'getcorner 'getkword 'entsel 'nentsel 'nentselp))
      (= 'STR (type (cond (pfx) (""))) (type (cond (sfx) (""))))
      (listp arg)
      (if (null (setq bit (car (vl-remove-if-not '(lambda (x) (= 'INT (type x))) arg)))) (setq bit 0) bit)
      (if (null (setq kwd (car (vl-remove-if-not '(lambda (x) (= 'STR (type x))) arg))))
        (not kwd)
        (if (vl-string-search "_" kwd)
          (setq kwd (mapcar 'cons (str2lst (car (str2lst kwd "_")) " ") (str2lst (cadr (str2lst kwd "_")) " ")))
          (setq kwd (mapcar 'cons (str2lst kwd " ") (str2lst kwd " ")))
        )
      )
      (if hlp
        (if (not (assoc "?" kwd))
          (setq kwd (append kwd '(("?" . "?"))))
          T
        )
        T
      )
      (cond
        ( (null dft) (not dft))
        ( (member dft (mapcar 'car kwd))
          (setq dft (assoc dft kwd))
        )
        ( (member dft (mapcar 'cdr kwd))
          (setq dft (nth (vl-position (car (member dft (mapcar 'cdr kwd))) (mapcar 'cdr kwd)) kwd))
        )
        ( T (setq dft (cons (vl-princ-to-string dft) dft)))
      )
      (if (and dft (= 1 (logand 1 bit))) (setq bit (1- bit)) T)
      (setq arg (vl-remove nil (list bit (lst2str (vl-remove nil (list (lst2str (mapcar 'car kwd) " ") (lst2str (mapcar 'cdr kwd) " "))) "_"))))
      (if (or pfx kwd dft sfx)
        (setq
          msg
            (strcat
              (cond (pfx) (""))
              (if kwd (strcat " [" (lst2str (mapcar 'car kwd) "/") "]") "")
              (if dft (strcat " <" (car dft) ">") "")
              (cond (sfx) (""))
            )
        )
        (not (setq msg nil))
      )
      (if hlp
        (while (= "?" (setq val (get msg)))
          (cond
            ( (listp hlp) (eval hlp))
            ((princ hlp))
          )
        )
        (setq val (get msg))
      )
    )
    val
  )
  (defun LgT (en fr)
    (if (= (getvar "LOCALE") "FR")
      fr 
      en
    )
  )
  (defun LM:vl-setattributevalue ( blk tag val )
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
        (if (= tag (strcase (vla-get-tagstring att)))
          (progn (vla-put-textstring att val) val)
        )
      )
      (vlax-invoke blk 'getattributes)
    )
  )

  (if (not *AttI-Tag*) (setq *AttI-Tag* "XXX"))
  (if (not *AttI-Val*) (setq *AttI-Val* 1))
  (if (not *AttI-Pas*) (setq *AttI-Pas* 1))
  (if (not *AttI-Pre*) (setq *AttI-Pre* ""))
  (if (not *AttI-Suf*) (setq *AttI-Suf* ""))
  (while (not break)
    (setq att
      (getkdh
        (quote (nentsel msg))
        (LgT
          "\nPlease select an attribute or"
          "\nVeuillez sélectionner un attribut ou"
        )
        (list (LgT "Name eXit _Name eXit" "Nommer Quitter _Name eXit"))
        " : "
        "Name"
        nil
      )
    )
    (cond
      ( (= "eXit" att) (setq break T))
      ( (= "Name" att)
        (cond
          ( (= "" (setq att (getstring (strcat (LgT "\nSpecify the tag name <" "\nRenseignez le nom d'étiquette <") *AttI-Tag* "> : "))))
            (setq att *AttI-Tag*)
          )
          (att)
        )
        (setq break T)
      )
      ( (and
          (listp att)
          (setq att (car att))
          (= "ATTRIB" (cdr (assoc 0 (entget att))))
        )
        (setq att (cdr (assoc 2 (entget att))))
        (setq break T)
      )
      ( T
        (princ (LgT "\nError on selection, try again please..." "\nErreur lors de la sélection, veuillez réessayer..."))
      )
    )
  )
  (and
    att
    (setq *AttI-Tag* att)
    (not (setq break nil))
    (while (not break)
      (princ
        (strcat
          (LgT "\nStep = " "\nPas = ") (itoa (cond (pas) (*AttI-Pas*)))
          (LgT " | Prefix = \"" " | Préfixe = \"") (cond (pre) (*AttI-Pre*)) "\""
          (LgT " | Suffix = \"" " | Suffixe = \"") (cond (suf) (*AttI-Suf*)) "\""
        )
      )
      (setq val
        (getkdh
          (quote (getint msg))
          (LgT "\nStarting value" "\nValeur de départ")
          (list (LgT "steP prEfix sUffix _steP prEfix sUffix" "Pas prEfixe sUffixe _steP prEfix sUffix"))
          " : "
          *AttI-Val*
          nil
        )
      )
      (cond
        ( (= "steP" val)
          (setq pas (getkdh (quote (getint msg)) (LgT "\nSpecify the step" "\nSpécifiez le pas") (list 2) " : " (itoa *AttI-Pas*) nil))
        )
        ( (= "prEfix" val)
          (setq pre (getstring T (strcat (LgT "\nPrefix <" "\nPréfixe <") *AttI-Pre* "> : ")))
          (if (= "" pre)
            (setq pre *AttI-Pre*)
            (setq *AttI-Pre* pre)
          )
        )
        ( (= "sUffix" val)
          (setq suf (getstring T (strcat (LgT "\nSuffix <" "\nSuffixe <") *AttI-Suf* "> : ")))
          (if (= "" suf)
            (setq suf *AttI-Suf*)
            (setq *AttI-Suf* suf)
          )
        )
        ( (numberp val)
          (setq
            pas (cond (pas) (*AttI-Pas*)) *AttI-Pas* pas
            pre (cond (pre) (*AttI-Pre*)) *AttI-Pre* pre
            suf (cond (suf) (*AttI-Suf*)) *AttI-Suf* suf
            *AttI-Val* val
            break T
          )
        )
      )
    )
    (setq n 0)
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT "\nSelection mode" "\nMode de sélection")
        (list (LgT "Auto Manual _Auto Manual" "Auto Manuel _Auto Manual"))
        " : "
        "Manual"
        nil
      )
    )
    (cond
      ( (and (= "Auto" mode) (setq jsel (ssget '((0 . "INSERT") (66 . 1)))))
        (repeat (setq i (sslength jsel))
          (setq name (ssname jsel (setq i (1- i))))
          (if (LM:vl-setattributevalue (vlax-ename->vla-object name) att (strcat pre (itoa val) suf))
            (setq
              val (+ val pas)
              n (1+ n)
            )
          )
        )
        T
      )
      ( (and (= "Manual" mode) (not (setq break nil)))
        (while (not break)
          (princ (strcat (LgT "\nTag = " "\nEtiquette = ") (strcase att)))
          (setq name
            (getkdh
              (quote (entsel msg))
              (LgT "\nPlease select a block with attributes" "\nVeuillez sélectionner un bloc avec attribut")
              (list (LgT "eXit _eXit" "Quitter _eXit"))
              " : "
              "eXit"
              nil
            )
          )
          (cond
            ( (= "eXit" name) (setq break T))
            ( (and
                (listp name)
                (setq name (car name))
                (= "INSERT" (cdr (assoc 0 (entget name))))
                (= 1 (cdr (assoc 66 (entget name))))
              )
              (if (LM:vl-setattributevalue (vlax-ename->vla-object name) att (strcat pre (itoa val) suf))
                (setq
                  val (+ val pas)
                  n (1+ n)
                )
              )
            )
            ( T (princ (LgT "\nError on selection, try again please..." "\nErreur lors de la sélection, veuillez réessayer...")))
          )
        )
        T
      )
    )
    (princ
      (strcat
        (LgT "\nA total of " "\nUn total de ")
        (itoa n)
        (LgT " block has been modified succesfully..." " blocs ont été modifiés avec succès...")
      )
    )
  )
  (princ)
)