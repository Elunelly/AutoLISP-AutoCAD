
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                            --{  HANDLEPREVIEW  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] HANDLEPREVIEW []-----------------------[]                                       ;
;--- Date of creation       > 06/07/2022                                                                                                            ;
;--- Last modification date > 18/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.3.0                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allows the user to select objects based on their handle and to browse the selection set object by object. The list of handles can be provided   ;
;   manually (with each handle separated by a comma) or by opening a .csv file and get all the informations about the entity. The .csv file has to  ;
;   have a column named "Handle" (at any row and column) to start extracting the handles's list (it's supposed to be a title so each row under this ;
;   one will be used as handle's value). The file has to have the name of the .dwg in its name and the current layout will be used to check if the  ;
;   entity is on the same layout (so be careful about the current layout before running the command)                                                ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves the initial value of VIEWCTR and VIEWSIZE to keep in memory the initial zoom and deselect objects if preselection     ;
; Step n°2        : Asks the user to specify the list of handle or to open a .csv file                                                              ;
; Step n°2.a        : The user can type directly the list manually as answer, each handle must be separated by a comma (not case-sensitive)         ;
; Step n°2.b        : The user can use the option "File" to open a .csv file to import it in the current drawing (and current space). The file have ;
;                     to be a .csv extension, it has to have the name of the current drawing in its name and must have at least a column "Handle"   ;
; Step n°2.b.1        : If the file wasn't empty, prompt a dialog box with the list of handles (all selected by default) to chose wisely the list   ;
;                       of handles you really want to preview within the list imported from .csv file                                               ;
; Step n°3        : For each handle in the list, check if they exist in the current drawing and if they're in the current space (layout or model)   ;
; Step n°4        : Creates a menu with different options to preview all objects or objects one by one. You can browse through the selection set    ;
;                   and check the coherence between the properties of the object in the drawing and the properties from .csv file (default value is ;
;                   the last option used by user                                                                                                    ;
; Step n°4.a        : The option "Next" go to the next entity stored in the list, zoom in on it and displays its properties (if .csv file)          ;
; Step n°4.b        : The option "Previous" go to the previous entity stored in the list, zoom in on it and displays its properties (if .csv file)  ;
; Step n°4.c        : The option "All" select all entities stored in the list, zoom in on them and displays their mutual properties (if .csv file)  ;
; Step n°4.d        : The user can type manually the name of an handle to select it directly or type its position in the list                       ;
; Step n°4.e        : The option "eXit" stop the preview menu and returns to the initial zoom (VIEWCTR and VIEWSIZE). Keeps the previous object(s)  ;
;                     selected                                                                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaFun" ---> mAtom                                         | v1.0.0 - 31/12/2021 (Luna)                                                    ;
;   --•  "BaStr" ---> Wildcard-trim                                 | v1.0.0 - 10/12/2021 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;   --•  "UtFil" ---> read-file                                     | v1.0.1 - 06/07/2022 (Luna)                                                    ;
;   --•  "UtCom" ---> SetVarList                                    | v2.0.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> ZoomObjects                                   | v1.0.0 - 06/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> infolist                                      | v1.0.0 - 06/07/2022 (Luna)                                                    ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 06/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (HANDLEPREVIEW) returns nothing after the preview, except the remaining selected objects. During the preview you can open and modify;
;   the properties of the selected objects if needed. If the list of handle(s) was provided by a .csv file, the program will prompt the value for   ;
;   each column found in the file for the specified handle of the preview (so you can check if the properties between the .csv and .dwg are the     ;
;   same or not).                                                                                                                                   ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.3.0   |   Correct the issues when the Handle is set with "'" in a .csv file and modified the separator                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   Add the option "Handle" to select in a dialog box the wanted handle's name (with its properties displayed)                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.1   |   Fix a problem with the default value of 'mode' if user enter the handle's name or position instead of a listed option          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   For "File" mode to get the handles's list, opens a (ListBox) to select only wanted handles if needed (all selected by default) | ;
; |            |   add the possibility to enter the name of the handle to preview it directly or its position in the list (for efficiency)        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:HANDLEPREVIEW (/ *error* infolist doc spc sep jsel vctr vsiz CMDECHO mode filename dwg hdl lst line obj e s param option old break tmp i l)
  (defun *error* (msg)
    (setvar "CMDECHO" 0)
    (command-s "_ZOOM" "_C" vctr vsiz)
    (setvar "CMDECHO" CMDECHO)
    (sssetfirst)
    (princ msg)
  )
  (defun infolist (fun foo hlst / csv)
    (setq csv (vl-remove-if-not '(lambda (x) (member (nth pos x) hlst)) lst))
    (mapcar
      '(lambda (a b) (eval foo))
      (car csv)
      (apply 'mapcar (cons fun csv))
    )
  )

  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    spc (vla-get-Block (vla-get-ActiveLayout doc))
    sep ", "
    jsel (ssadd)
  )
  (SetVarList '(("VIEWCTR" vctr nil) ("VIEWSIZE" vsiz nil) ("CMDECHO" CMDECHO nil)))
  (sssetfirst)
  (setq mode
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nEnter the handle(s) manually or"
        "\nEntrer le(s) handle(s) manuellement ou"
        nil
      )
      (list 128 (LgT "File _File" "Fichier _File" nil))
      " : "
      nil
      (LgT
        (strcat
          "\nHANDLEPREVIEW : Handle's listing method"
          "\nDefault value:     <none>"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |     File    | Opens a dialog box to search for a .csv file and read it. Only .csv |"
          "\n  |             | files with a column named Handle are allowed for this command       |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | You can also enter directly the name of the handles you're actually |"
          "\n  |    Write    | searching for as an answer. If you want to select multiple objects  |"
          "\n  |             | you can separate each handle's name with a comma (,) but no spaces  |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nHANDLEPREVIEW : Méthode pour lister les handles"
          "\nValeur par défaut: <aucune>"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Ouvre une boîte de dialogue pour rechercher un fichier .csv et le   |"
          "\n  |   Fichier   | lire. Seuls les fichiers .csv avec une colonne nommée Handle sont   |"
          "\n  |             | autorisés pour cette command                                        |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Vous pouvez également entrer directement le nom des handles que     |"
          "\n  |   Ecriture  | vous cherchez comme réponse. Si vous souhaitez sélectionner         |"
          "\n  |             | plusieurs objets, vous pouvez séparer chaque nom d'handle par une   |"
          "\n  |             | virgule (,) mais sans espaces                                       |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (cond
    ( (= "File" mode)
      (cond
        ( (not
            (setq filename
              (getfiled
                (LgT
                  "Selection of .csv import file"
                  "Sélection du fichier .csv d'import"
                  nil
                )
                (strcat (getvar "DWGPREFIX") (setq dwg (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))))
                "csv"
                128
              )
            )
          )
          (princ (LgT "\nNo .csv file selected..." "\nAucun fichier .csv sélectionné..." nil))
        )
        ( (not (wcmatch (strcase filename) (strcat "*" (strcase dwg) "*")))
          (princ
            (strcat
              (LgT
                "\nThe .csv file \""
                "\nLe fichier .csv \""
                nil
              )
              (substr filename (+ 2 (vl-string-position (ascii "\\") filename 0 T)))
              (LgT
                "\" is not from the current drawing \""
                "\" n'est pas issu du dessin courant \""
                nil
              )
              dwg
              "\"..."
            )
          )
        )
        ( (not (setq lst (read-file filename)))
          (princ
            (strcat
              (LgT
                "\nThe .csv file \""
                "\nLe fichier .csv \""
                nil
              )
              filename
              (LgT
                "\" is empty..."
                "\" est vide..."
                nil
              )
            )
          )
        )
        ( (and
            (not
              (while
                (and
                  (setq line (car lst))
                  (setq line (str2lst line ";"))
                  (null (setq pos (vl-position (strcase "Handle") (mapcar 'strcase line))))
                )
                (setq lst (cdr lst))
              )
            )
            (null pos)
          )
          (princ
            (LgT
              "\nThe .csv file doesn't have a column named \"Handle\"..."
              "\nLe fichier .csv n'a pas de colonne nommée \"Handle\"..."
              nil
            )
          )
        )
        ( (not
            (setq lst
              (mapcar
                '(lambda (x / s)
                  (setq s (str2lst x ";"))
                  (lst2str
                    (subst
                      (strcase (vl-string-trim (apply 'strcat (Wildcard-trim ".")) (nth pos s)))
                      (nth pos s)
                      s
                    )
                    sep
                  )
                 )
                (cdr lst)
              )
            )
          )
          (princ
            (LgT
              "\nThe .csv file doesn't have any value in \"Handle\" column..."
              "\nLe fichier .csv n'a pas une seule valeur dans la colonne \"Handle\"..."
              nil
            )
          )
        )
        ( (setq hdl
            (ListBox
              (LgT
                "HANDLEPREVIEW: Handle(s)'s selection"
                "HANDLEPREVIEW: Sélection des handle(s)"
                nil
              )
              (LgT
                "Select only the handle(s) you want to check on drawing :"
                "Sélectionnez seulement les handle(s) que vous souhaitez vérifier dans le dessin :"
                nil
              )
              lst
              lst
              2
              30
            )
          )
          (setq hdl (mapcar '(lambda (x) (strcase (nth pos (str2lst x sep)))) hdl))
        )
      )
    )
    ( (and mode (not (= "" mode)))
      (setq hdl
        (str2lst
          (strcase (vl-string-translate (apply 'strcat (Wildcard-trim ".")) (mAtom "," (length (Wildcard-trim ".")) 'strcat) mode))
          ","
        )
      )
    )
  )
  (if lst (setq lst (mapcar '(lambda (l) (str2lst l sep)) lst)))
  (foreach h (reverse hdl)
    (if
      (and
        (if (not (vl-catch-all-error-p (setq obj (vl-catch-all-apply 'vla-HandleToObject (list doc h))))) T (not (setq e (cons h e))))
        (if
          (and
            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-ObjectIdToObject (list doc (vla-get-OwnerId obj)))))
            (equal (vla-ObjectIdToObject doc (vla-get-OwnerId obj)) spc)
          )
          T
          (not (setq s (cons h s)))
        )
      )
      (setq
        obj (vlax-vla-object->ename obj)
        tmp (cons h tmp)
        jsel (ssadd obj jsel) 
      )
    )
  )
  (if e
    (princ
      (strcat
        (LgT
          "\nWARNING: Some specified handle(s) wasn't found in the current drawing :"
          "\nATTENTION: Certain(s) handle(s) sépcifié(s) n'ont pas été trouvé(s) sur le dessin courant :"
          nil
        )
        "\n"
        (lst2str e ", ")
      )
    )
  )
  (if s
    (princ
      (strcat
        (LgT
          "\nWARNING: Some specified handle(s) wasn't found in the current layout but exists in the current drawing"
          "\nATTENTION: Certain(s) handle(s) sépcifié(s) n'ont pas été trouvé(s) sur la présentation courante mais existe(nt) dans le dessin courant"
          nil
        )
        "\n"
        (lst2str s ", ")
      )
    )
  )
  (if (< 0 (sslength jsel))
    (progn
      (sssetfirst)
      (sssetfirst nil (ZoomObjects jsel))
      (setq
        i -1
        l (length tmp)
        param
          (list
            (cons "Previous" "Précédent")
            (cons "Next" "Suivant")
            (cons "All" "Tous")
            (cons "Handle" "Handle")
            (cons "eXit" "Quitter")
          )
        option
          (LgT
            (lst2str (list (lst2str (mapcar 'car param) " ") (lst2str (mapcar 'car param) " ")) " _")
            (lst2str (list (lst2str (mapcar 'cdr param) " ") (lst2str (mapcar 'car param) " ")) " _")
            nil
          )
        old "eXit"
      )
      (while
        (and
          (not break)
          (if lst
            (princ
              (strcat
                "\n"
                (lst2str
                  (mapcar
                    '(lambda (e v) (strcat e " = " v))
                    line
                    (infolist
                      '=
                      '(if b a "*VARIES*")
                      (if (minusp i) tmp (list (nth i tmp)))
                    )
                  )
                  ", "
                )
              )
            )
            (cond
              ( (= 1 (sslength jsel)) (princ (strcat "\nHandle = " (car tmp))))
              ( (minusp i) (princ "\nHandle = *VARIES*"))
              ( (princ (strcat "\nHandle = " (nth i tmp))))
            )
          )
          (setq mode old mode
            (getkdh
              (quote (getkword msg))
              (strcat
                "\n n°"
                (itoa (if (minusp i) l (1+ i)))
                " / "
                (itoa l)
                (LgT
                  " | Zoom on"
                  " | Zoom sur"
                  nil
                )
              )
              (list 128 option)
              " : "
              (LgT mode (cdr (assoc mode param)) nil)
              (LgT
                (strcat
                  "\nHANDLEPREVIEW : Selection preview (zoom)"
                  "\nDefault value:     <" (LgT mode (cdr (assoc mode param)) nil) ">"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |   Previous  | Zooms in on the object before the current object in selection set   |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     Next    | Zooms in on the object after the current object in selection set    |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     All     | Zooms in on all objects in the selection set                        |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |    Handle   | Opens a dialog box to select the wanted handle within the list and  |"
                  "\n  |             | zoom in on it                                                       |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     eXit    | Ends the command and keeps selected the preview's object(s)         |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |    Write    | You can also type one handle's name as answer (not case-sensitive)  |"
                  "\n  |             | or its position in the list (number) to zoom in on it directly      |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n"
                )
                (strcat
                  "\nHANDLEPREVIEW : Aperçu de la sélection (zoom)"
                  "\nValeur par défaut: <" (LgT mode (cdr (assoc mode param)) nil) ">"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |  Précédent  | Effectue un zoom sur l'objet précédant l'objet actuel contenu dans  |"
                  "\n  |             | le jeu de sélection                                                 |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |   Suivant   | Effectue un zoom sur l'objet suivant l'objet actuel contenu dans le |"
                  "\n  |             | jeu de sélection                                                    |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     Tous    | Effectue un zoom sur l'ensemble des objets contenus dans le jeu de  |"
                  "\n  |             | sélection                                                           |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |    Handle   | Ouvre une boîte de dialogue pour sélectionner le handle désiré dans |"
                  "\n  |             | la liste et effectue un zoom sur l'objet correspondant              |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |   Quitter   | Termine la commande et garde sélectionné le(s) objet(s) de l'aperçu |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |             | Vous pouvez également taper le nom du handle comme réponse (non     |"
                  "\n  |   Ecriture  | sensible à la casse) ou sa position (numéro) dans la liste pour     |"
                  "\n  |             | zoomer directement dessus                                           |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n"
                )
                nil
              )
            )
          )
          (null (sssetfirst))
        )
        (cond
          ( (= "eXit" mode)
            (setq break T)
            (setvar "CMDECHO" 0)
            (command-s "_ZOOM" "_C" vctr vsiz)
            (setvar "CMDECHO" CMDECHO)
          )
          ( (= "Next" mode) (setq old mode i (rem (1+ i) l)))
          ( (= "Previous" mode) (setq old mode i (if (minusp (1- i)) (1- l) (1- i))))
          ( (= "All" mode) (setq old mode i -1))
          ( (= "Handle" mode)
            (setq
              old mode
              i
                (ListBox
                  (LgT
                    "HANDLEPREVIEW: Handle's selection"
                    "HANDLEPREVIEW: Sélection du handle"
                    nil
                  )
                  (LgT
                    "Select the handle you want to check on drawing :"
                    "Sélectionnez le handle que vous souhaitez vérifier dans le dessin :"
                    nil
                  )
                  (if lst (mapcar '(lambda (x) (lst2str x " ; ")) (cdr lst)) tmp)
                  nil
                  0
                  nil
                )
              i
                (if lst
                  (vl-position (str2lst i " ; ") (cdr lst))
                  (vl-position (strcase i) tmp)
                )
            )
          )
          ( mode
            (cond
              ( (distof mode)
                (if (< 0 (atoi mode) (1+ l)) (setq i (1- (atoi mode))))
              )
              ( (member (strcase mode) tmp) (setq i (vl-position (strcase mode) tmp)))
            )
          )
        )
        (if (minusp i)
          (sssetfirst nil (ZoomObjects jsel))
          (sssetfirst nil (ZoomObjects (ssadd (handent (nth i tmp)))))
        )
      )
      (setvar "CMDECHO" 0)
      (command-s "_ZOOM" "_C" vctr vsiz)
      (setvar "CMDECHO" CMDECHO)
      (sssetfirst)
      (if (minusp i)
        (sssetfirst nil jsel)
        (sssetfirst nil (ssadd (handent (nth i tmp))))
      )
    )
  )
  (princ)
)