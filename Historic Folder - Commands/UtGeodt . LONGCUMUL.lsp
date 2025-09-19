
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  LONGCUMUL  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] LONGCUMUL []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.2                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Calculates the cumulated length of selected objects obtained with a filtered selection. You can read the results within a dialog box where you  ;
;   can also filter the results with some properties or export the whole results in a .csv file.                                                    ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Initialize the filters that will be applied to selection set by default                                                         ;
; Step n°2        : Retrieve the current value of "DIMZIN", "LUPREC" and "INSUNITS" and set them as 0 and 2 ("INSUNITS" is not modified)            ;
; Step n°3        : Ask the user to define the filter that will be applied to selection set                                                         ;
; Step n°3.a        : The user can select an object to get its layer property (DXF 8) and object type property (DXF 0) to add them to the filter    ;
; Step n°3.b        : The option "Layers" opens a dialog box to select within the layers's list all the wanted layers. If some layers are already   ;
;                     set for filter, the new selected layers will be added to the list (even if the previous ones are not selected)                ;
; Step n°3.c        : The option "Objects" opens a dialog box to select within the allowed object types's list all the wanted types. The selection  ;
;                     will overwrite the previous one                                                                                               ;
; Step n°3.d        : The option "All" will ask you a Yes/No question to know if you want to select only some objects with the filter or every      ;
;                     entity corresponding to this filter within the entire database (restrain by the current layout tab)                           ;
; Step n°3.d.1        : "Yes" means you want to perform the selection within the entire database, ignoring the pre-selection                        ;
; Step n°3.d.2        : "No" means you want to perform the selection manually or from the pre-selection if PICKSTYLE = 1 with filter ON             ;
; Step n°3.e        : The option "Undo" delete the last layer within the filter list to correct a mistake if needed (only one layer will be remove) ;
; Step n°3.f        : The option "eXit" put an end to the Step n°3 and allow you to go to Step n°4 (the filtered selection)                         ;
; Step n°4        : Perform a selection filtered thanks to the previous step manually (if nor pre-selection neither option "All" on "Yes") or within;
;                   pre-selection (if pre-selection and option "All" on "No") either all entities corresponding to the filter properties            ;
;                   pre-selection of not with option "All" on "Yes"                                                                                 ;
; Step n°5        : Construct a 3 levels list with the layer's name as first level's key, the object type's name as second level's key and the third;
;                   level corresponds to a list where the each element is a list with first element corresponding to the length of entity and       ;
;                   second element corresponding to the handle of entity                                                                            ;
; Step n°6        : If the list is not null, opens a dialog box to display the detailed results. The dialog box is separated in 2 parts             ;
; Step n°6.a        : The first one on the right is a listbox to prompt the results (with a scroll bar if the height of the listbox is not enough   ;
;                     display every line) and a button "Export to .csv" below it                                                                    ;
; Step n°6.b        : The second one on the left is a boxed column composed by several toggles and editbox. Each toggle corresponds to a specific   ;
;                     line in the results listbox and allow the user to prompt or not this line or property (to simplify or detail the results).    ;
;                     The editboxes allow the user to modify directly the prompted length values by multiplying them by a coefficient and/or add a  ;
;                     constant length value (the coefficient is applied to the length property and the add length)                                  ;
; Step n°6.c        : The button "Export to .csv" on the right allow the user to export the results (the whole list of objects, the coefficient and ;
;                     the add length) into a .csv file. Prompt a file's selection dialog box to create a new file or replace an existing one        ;
; Step n°6.d        : When you gathered all the wanted information, close the dialog box with the "Ok" button                                       ;
; Step n°7        : Display the results in the command line's historic, with the total length and number of objects for each layer, then the global ;
;                   results as total length, number of objects, maximum length, minimum length, average length, coefficient and add length. If a    ;
;                   .csv file (or more) has been created, the path for each file will be prompted to access quickly to each file (copy/paste the    ;
;                   path in the windows file's explorer to open them directly)                                                                      ;
; Step n°8        : Reset "DIMZIN" and "LUPREC" to their initial values                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> SwapBit                                       | v1.0.0 - 24/06/2022 (Luna)                                                    ;
;   --•  "BaFun" ---> mAtom                                         | v1.0.0 - 31/12/2021 (Luna)                                                    ;
;   --•  "BaLst" ---> loop-a-list-properties                        | v2.0.0 - 28/02/2022 (Luna)                                                    ;
;   --•  "BaStr" ---> space                                         | v1.0.0 - 22/02/2021 (Luna)                                                    ;
;   --•  "BaStr" ---> ThsdSpace                                     | v1.0.0 - 29/12/2021 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtCom" ---> SetVarList                                    | v2.0.0 - 15/06/2022 (Luna)                                                    ;
;   --•  "UtCom" ---> units                                         | v1.0.0 - 23/06/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "VlCol" ---> vla-collection->list                          | v2.0.0 - 19/08/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaEqc" ---> LgC-bit                                       | v2.0.0 - 27/06/2022 (Luna)                                                    ;
;   --•  "BaErr" ---> *error*                                       | v1.1.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtDis" ---> LgC-princ-param                               | v1.0.0 - 22/06/2022 (Luna)                                                    ;
;   --•  "DbOpc" ---> LgC-Results-DCL                               | v1.0.0 - 23/06/2022 (Luna)                                                    ;
;   --•    "BaErr" ---> *error*                                     | v1.0.0 - 23/06/2022 (Luna)                                                    ;
;   --•    "BaStr" ---> LgC-EditCheck                               | v1.0.0 - 25/06/2022 (Luna)                                                    ;
;   --•    "UtFil" ---> LgC-Export                                  | v1.0.1 - 25/06/2022 (Luna)                                                    ;
;   --•      "BaErr" ---> *error*                                   | v1.0.0 - 23/06/2022 (Luna)                                                    ;
;   --•    "DbTil" ---> LgC-Settings-set                            | v1.0.0 - 24/06/2022 (Luna)                                                    ;
;   --•    "DbLst" ---> LgC-Results                                 | v2.0.0 - 27/06/2022 (Luna)                                                    ;
;   --•      "DbLst" ---> LgC-Det_Lengths                           | v1.0.0 - 27/06/2022 (Luna)                                                    ;
;   --•      "DbLst" ---> LgC-Det_Objects                           | v1.0.0 - 27/06/2022 (Luna)                                                    ;
;   --•      "DbLst" ---> LgC-Det_Layers                            | v1.0.0 - 27/06/2022 (Luna)                                                    ;
;   --•      "DbLst" ---> LgC-Dis                                   | v1.0.0 - 24/06/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (LONGCUMUL) returns the results based on "LONGCUMUL_Settings" value for each layer and the detailed results with the total length,  ;
;   number of objects, maximum length, minimum length, average length, coefficient and add length for the whole selection set.                      ;
;     Ex. :                                                                                                                                         ;
;       Command: LONGCUMUL                                                                                                                          ;
;       Current settings: Select all = No                                                                                                           ;
;       Layer(s) list =                                                                                                                             ;
;       Object type(s) list =                                                                                                                       ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> :                                                                               ;
;       Current settings: Select all = No                                                                                                           ;
;       Layer(s) list = Layer 2                                                                                                                     ;
;       Object type(s) list = Polyline                                                                                                              ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> :                                                                               ;
;       Current settings: Select all = No                                                                                                           ;
;       Layer(s) list = Layer 2, Layer 10                                                                                                           ;
;       Object type(s) list = Polyline                                                                                                              ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> :                                                                               ;
;       Current settings: Select all = No                                                                                                           ;
;       Layer(s) list = Layer 2, Layer 10, Layer 3                                                                                                  ;
;       Object type(s) list = Polyline                                                                                                              ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> :                                                                               ;
;       Current settings: Select all = No                                                                                                           ;
;       Layer(s) list = Layer 2, Layer 10, Layer 3, Layer 8                                                                                         ;
;       Object type(s) list = Polyline                                                                                                              ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> :                                                                               ;
;       Current settings: Select all = No                                                                                                           ;
;       Layer(s) list = Layer 2, Layer 10, Layer 3, Layer 8                                                                                         ;
;       Object type(s) list = Polyline, Circle                                                                                                      ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> : a                                                                             ;
;       Do you want to select all objects within the current tab or to select them manually (+ pre-selection) [Yes/No/?] <No> ? y                   ;
;       Current settings: Select all = Yes                                                                                                          ;
;       Layer(s) list = Layer 2, Layer 10, Layer 3, Layer 8                                                                                         ;
;       Object type(s) list = Polyline, Circle                                                                                                      ;
;       Select an object or [Layers/Objects/All/Undo/eXit/?] <eXit> :                                                                               ;
;       Object types's list : Circle, Polyline                                                                                                      ;
;       ­                                                                                                                                            ;
;          []+-+-+-+-+-+-+-+-+-+[]                                                                                                                  ;
;        -> Layer "Layer 10" :                                                                                                                      ;
;           • Total length      = 285,493.70m                                                                                                       ;
;           • Number of objects = 3u                                                                                                                ;
;          []+-+-+-+-+-+-+-+-+-+[]                                                                                                                  ;
;        -> Layer "Layer 2" :                                                                                                                       ;
;           • Total length      = 252,576.68m                                                                                                       ;
;           • Number of objects = 4u                                                                                                                ;
;          []+-+-+-+-+-+-+-+-+-+[]                                                                                                                  ;
;        -> Layer "Layer 3" :                                                                                                                       ;
;           • Total length      = 221,974.45m                                                                                                       ;
;           • Number of objects = 4u                                                                                                                ;
;          []+-+-+-+-+-+-+-+-+-+[]                                                                                                                  ;
;        -> Layer "Layer 8" :                                                                                                                       ;
;           • Total length      = 109,244.47m                                                                                                       ;
;           • Number of objects = 4u                                                                                                                ;
;       ­                                                                                                                                            ;
;       TOTAL :                                                                                                                                     ;
;         - Total length         = 869,289.29m                                                                                                      ;
;         - Number of objects    = 15u                                                                                                              ;
;         - Maximum length       = 117,794.26m                                                                                                      ;
;         - Minimum length       = 18.85m                                                                                                           ;
;         - Average length       = 57,952.62m                                                                                                       ;
;         - Standar deviation    = 2,734.91m                                                                                                        ;
;         - Coefficient (x)      = 1.00                                                                                                             ;
;         - Add length (+)       = 0.00m                                                                                                            ;
;       ­                                                                                                                                            ;
;       File name n°1 : C:\Users\lphilip\OneDrive - URBASOLAR\AutoLISP\AutoCAD\LONGCUMUL - Test_LONGCUMUL Export1.csv                               ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.2   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Correct the formula of standard deviation at the end of the program                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Define 'bit', 'bitlist' and (LgC-bit) in main program, prompt the result in command line historic depending on the value of    | ;
; |   v3.0.0   |   environment variable "LONGCUMUL_Settings" for each layers, add the standard deviation in the list of prompted properties and   | ;
; |            |   add the functions (LgC-Det_Lengths), (LgC-Det_Objects) and (LgC-Det_Layers) to completely separate each level in the results   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.2   |   Replace the (getvar "CTAB") by "Model" for the selection filter to let the user use the command in the Model space of viewport | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Replace the "." with a "," for the export .csv file thanks to (vl-string-translate)                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:LONGCUMUL
  ( /
    *error* LgC-bit LgC-princ-param LgC-Results-DCL
    param type-list bit bitlist DIMZIN LUPREC u break mode msg lst typ lay jsel name tmp CSV coeff add i
  )
  (defun *error* (msg)
    (setvar "DIMZIN" DIMZIN)
    (setvar "LUPREC" LUPREC)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun LgC-bit (b a)
    (= b (logand a b))
  )
  (defun LgC-princ-param ()
    (princ
      (strcat
        (LgT
          "\nCurrent settings: Select all = "
          "\nParamètres courant: Sélectionner tout = "
          nil
        )
        (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil))
        (LgT
          "\nLayer(s) list = "
          "\nListe des calque(s) = "
          nil
        )
        (cond ((lst2str (cdr (assoc "LAYERS" param)) ", ")) ("*"))
        (LgT
          "\nObject type(s) list = "
          "\nListe des type(s) d'objet = "
          nil
        )
        (cond ((lst2str (mapcar 'car (cdr (assoc "OBJECT" param))) ", ")) ((lst2str (mapcar 'car type-list) ", ")))
      )
    )
    nil
  )
  (defun LgC-Results-DCL (/ *error* LgC-EditCheck LgC-Export LgC-Settings-set LgC-Results filename file DCL_ID)
    (defun *error* (msg)
      (if file (close file))
      (if filename (vl-file-delete filename))
      (princ msg)
    )
    (defun LgC-EditCheck (str flag)
      (cond
        ( (null (setq str (distof str)))
          (alert
            (LgT
              "Only numbers are allowed..."
              "Uniquement les nombres sont autorisés..."
              nil
            )
          )
        )
        ( (and flag (minusp str))
          (alert
            (LgT
              "Only positive numbers are allowed..."
              "Uniquement les nombres positifs sont autorisés..."
              nil
            )
          )
        )
        ( (and flag (zerop str))
          (alert
            (LgT
              "The value can't be set to zero..."
              "La valeur ne peut pas être définie à zéro..."
              nil
            )
          )
        )
        (str)
      )
    )
    (defun LgC-Export (/ *error* filename file cc ca c i)
      (defun *error* (msg)
        (if file (close file))
        (princ msg)
      )
      (if
        (and
          (setq
            filename
              (getfiled
                (LgT
                  "Selection of .csv export file"
                  "Sélection du fichier .csv d'export"
                  nil
                )
                (strcat (getvar "DWGPREFIX") (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "_LONGCUMUL Export")
                "csv"
                1
              )
          )
          (setq file (open filename "W"))
          (setq
            cc "$G$1"
            ca "$I$1"
            c "D"
            i 2
          )
          (write-line
            (strcat
              ";;;;;Coeff :"
              ";"
              (vl-string-translate "." "," (rtos coeff 2 2))
              ";"
              (LgT "Add :" "Supp. :" nil)
              ";"
              (vl-string-translate "." "," (rtos add 2 2))
            )
            file
          )
          (write-line
            (LgT
              "Handle;Layer;Object Type;Length (AutoCAD);Length (Corrected)"
              "Handle;Calque;Type d'Objet;Longueur (AutoCAD);Longueur (Corrigée)"
              nil
            )
            file
          )
          (mapcar
            '(lambda (x / lay l)
              (setq
                lay (car x)
                l (cdr x)
              )
              (mapcar
                '(lambda (x / typ l)
                  (setq
                    typ (car x)
                    l (cdr x)
                  )
                  (mapcar
                    '(lambda (x / hdl lng)
                      (setq
                        lng (car x)
                        hdl (cadr x)
                      )
                      (write-line
                        (strcat
                          hdl
                          ";"
                          lay
                          ";"
                          (nth (vl-position typ (mapcar 'cdr type-list)) (mapcar 'car type-list))
                          ";"
                          (vl-string-translate "." "," (rtos lng 2 2))
                          ";"
                          "=" cc "*(" ca "+" c (itoa (setq i (1+ i))) ")"
                        )
                        file
                      )
                     )
                    (vl-sort l '(lambda (e1 e2) (< (car e1) (car e2))))
                  )
                 )
                l
              )
             )
            lst
          )
          (not (setq file (close file)))
          (not
            (alert
              (strcat
                (LgT
                  "The file has been created successfully !"
                  "Le fichier a été créé avec succès !"
                  nil
                )
                "\n  > "
                (substr filename (+ 2 (vl-string-position (ascii "\\") filename 0 T)))
              )
            )
          )
        )
        filename
      )
    )
    (defun LgC-Settings-set (tile b)
      (if (= b (logand bit b))
        (set_tile tile "1")
        (set_tile tile "0")
      )
    )
    (defun LgC-Results (bit / LgC-Dis LgC-Det_Lengths LgC-Det_Objects LgC-Det_Layers i)
      (defun LgC-Dis (str f)
        (setq i (+ i 3))
        (if f
          (add_list
            (strcat
              (space (expt i 2))
              "[]"
              (substr (mAtom (if (= i 3) "• " "- ") (- 42 (* 2 (+ i 2))) 'strcat) 2)
              "[]"
              (space (expt i 2))
            )
          )
        )
        (add_list (strcat (space (- i 3)) str))
        (if (LgC-bit (cdr (assoc "Dis_Length" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Total length      = "
                "Longueur totale = "
                nil
              )
              (ThsdSpace (rtos (apply '+ lng) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Count" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Number of objects = "
                "Nombre d'objets = "
                nil
              )
              (ThsdSpace (itoa (length lng)) (LgT "," " " nil))
              "u"
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Max" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Maximum length    = "
                "Longueur max.   = "
                nil
              )
              (ThsdSpace (rtos (apply 'max lng) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Min" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Minimum length    = "
                "Longueur min.   = "
                nil
              )
              (ThsdSpace (rtos (apply 'min lng) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Avg" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Average length    = "
                "Longueur moy.   = "
                nil
              )
              (ThsdSpace (rtos (/ (apply '+ lng) (float (length lng))) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Dev" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Standard deviation= "
                "Ecart type      = "
                nil
              )
              (ThsdSpace
                (rtos
                  (sqrt (/ (apply '+ (mapcar '(lambda (x) (expt (- x (/ (apply '+ lng) (float (length lng)))) 2)) lng)) (float (length lng))))
                  2
                  2
                )
                (LgT "," " " nil)
              )
              (units u)
            )
          )
        )
      )
      (defun LgC-Det_Lengths (f n tmp)
        (if f
          (mapcar
            '(lambda (x / hdl lng)
              (setq
                i n
                lng (* coeff (+ add (car x)))
                hdl (cadr x)
              )
              (add_list (strcat (space i) " • \"" hdl "\" →  " (ThsdSpace (rtos lng 2 2) (LgT "," " " nil)) (units u)))
            )
            tmp
          )
        )
      )
      (defun LgC-Det_Objects (f n tmp)
        (if f
          (mapcar
            '(lambda (x / typ tmp lng)
              (setq
                i n
                typ (car x)
                tmp (cdr x)
                lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) tmp)
                typ (nth (vl-position typ (mapcar 'cdr type-list)) (mapcar 'car type-list))
              )
              (LgC-Dis (strcat (space i) "\"" typ "\" :") T)
              (LgC-Det_Lengths (LgC-bit (cdr (assoc "Det_Lengths" bitlist)) bit) i (vl-sort tmp '(lambda (e1 e2) (< (car e1) (car e2)))))
              (add_list "")
            )
            tmp
          )
        )
      )
      (defun LgC-Det_Layers (f n tmp)
        (if f
          (mapcar
            '(lambda (x / lay tmp lng)
              (setq
                i n
                lay (car x)
                tmp (cdr x)
                lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr tmp)))
              )
              (LgC-Dis (strcat (space i) (LgT "Layer \"" "Calque \"" nil) lay "\" :") T)
              (or
                (LgC-Det_Objects (LgC-bit (cdr (assoc "Det_Objects" bitlist)) bit) i (vl-sort tmp '(lambda (e1 e2) (< (car e1) (car e2)))))
                (LgC-Det_Lengths
                  (LgC-bit (cdr (assoc "Det_Lengths" bitlist)) bit)
                  i
                  (vl-sort (apply 'append (mapcar 'cdr tmp)) '(lambda (e1 e2) (< (car e1) (car e2))))
                )
              )
            )
            tmp
          )
        )
      )
      (start_list "Results")
      (cond
        ( (LgC-Det_Layers (LgC-bit (cdr (assoc "Det_Layers" bitlist)) bit) 0 lst))
        ( (LgC-Det_Objects
            (LgC-bit (cdr (assoc "Det_Objects" bitlist)) bit)
            0
            (setq
              tmp (apply 'append (mapcar 'cdr lst))
              tmp
                (vl-remove-if-not
                  '(lambda (b) (cdr b))
                  (mapcar
                    '(lambda (x) (cons (cdr x) (apply 'append (mapcar 'cdr (vl-remove-if-not '(lambda (a) (= (cdr x) (car a))) tmp)))))
                    type-list
                  )
                )
            )
          )
        )
        ( (LgC-Det_Lengths
            (LgC-bit (cdr (assoc "Det_Lengths" bitlist)) bit)
            0
            (vl-sort (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))) '(lambda (e1 e2) (< (car e1) (car e2))))
          )
        )
        ( T
          (setq
            i 0
            lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))))
          )
          (LgC-Dis "" nil)
        )
      )
      (end_list)
    )
    (setq
      filename (vl-filename-mktemp "LONGCUMUL_ResultsBox.dcl")
      file (open filename "W")
      coeff 1.0
      add 0.0
    )
    (mapcar
      '(lambda (l) (write-line l file))
      (list
        " LONGCUMUL_ResultsBox:dialog {"
        (strcat "   label = \"" (LgT "LONGCUMUL : Results" "LONGCUMUL : Résultats" nil) "\" ;")
        "   :row {"
        "     :boxed_column {"
        (strcat "       label = \"" (LgT "Settings" "Paramètres" nil) "\" ;")
        "       :toggle {"
        (strcat "         label = \"" (LgT "Details by LAYERS" "Détailler par CALQUES" nil) "\" ;")
        "         key = \"Det_Layers\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Details by OBJECT TYPES" "Détailler par TYPES D'OBJET" nil) "\" ;")
        "         key = \"Det_Objects\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Details by LENGTHS" "Détailler par LONGUEURS" nil) "\" ;")
        "         key = \"Det_Lengths\" ;"
        "       }"
        "       spacer ;"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays NUMBER of objects" "Afficher le NOMBRE d'objets" nil) "\" ;")
        "         key = \"Dis_Count\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays CUMULATED length" "Afficher la longueur CUMULÉE" nil) "\" ;")
        "         key = \"Dis_Length\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays MAXIMUM length" "Afficher la longueur MAXIMALE" nil) "\" ;")
        "         key = \"Dis_Max\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays MINIMUM length" "Afficher la longueur MINIMALE" nil) "\" ;")
        "         key = \"Dis_Min\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays AVERAGE length" "Afficher la longueur MOYENNE" nil) "\" ;")
        "         key = \"Dis_Avg\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays STANDARD DEVIATION" "Afficher l'ÉCART TYPE" nil) "\" ;")
        "         key = \"Dis_Dev\" ;"
        "       }"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "       :edit_box {"
        "         label = \"Coefficient (x) =\" ;"
        "         key = \"Coeff\" ;"
        (strcat "         value = \"" (rtos coeff 2 2) "\" ;")
        "         alignment = \"left\" ;"
        "         width = 39 ;"
        "         edit_width = 16 ;"
        "         fixed_width = true ;"
        "       }"
        "       :edit_box {"
        (strcat "         label = \"" (LgT "Add length (+)  = " "Longueur ajoutée (+) =" nil) "\" ;")
        "         key = \"Add\" ;"
        (strcat "         value = \"" (rtos add 2 2) "\" ;")
        "         alignment = \"left\" ;"
        "         width = 39 ;"
        "         edit_width = 16 ;"
        "         fixed_width = true ;"
        "       }"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "     }"
        "     :boxed_column {"
        (strcat "       label = \"" (LgT "Results" "Résultats" nil) "\" ;")
        "       :list_box {"
        "         key = \"Results\" ;"
        "         multiple_select = true ;"
        "         height = 51 ;"
        "         width = 100 ;"
        "       }"
        "       :row {"
        "         spacer ;"
        "         :button {"
        "           label = \"Export .csv\" ;"
        "           key = \"Export\" ;"
        "           width = 12 ;"
        "           alignment = \"centered\" ;"
        "           is_default = true ;"
        "         }"
        "         spacer ;"
        "       }"
        "     }"
        "   }"
        "   spacer ;"
        "   ok_only ;"
        " }"
      )
    )
    (setq file (close file))
    (setq DCL_ID (load_dialog filename))
    (if (not (new_dialog "LONGCUMUL_ResultsBox" DCL_ID))
      (exit)
    )
    (foreach x bitlist
      (LgC-Settings-set (car x) (cdr x))
    )
    (action_tile "Det_Layers" "(setq bit (SwapBit bit (cdr (assoc \"Det_Layers\" bitlist)))) (LgC-Results bit)")
    (action_tile "Det_Objects" "(setq bit (SwapBit bit (cdr (assoc \"Det_Objects\" bitlist)))) (LgC-Results bit)")
    (action_tile "Det_Lengths" "(setq bit (SwapBit bit (cdr (assoc \"Det_Lengths\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Count" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Count\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Length" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Length\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Max" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Max\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Min" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Min\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Avg" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Avg\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Dev" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Dev\" bitlist)))) (LgC-Results bit)")
    (LgC-Results bit)
    (action_tile "Coeff" "(set_tile \"Coeff\" (rtos (setq coeff (cond ((LgC-EditCheck $value T)) (coeff))) 2 2)) (LgC-Results bit)")
    (action_tile "Add" "(set_tile \"Add\" (rtos (setq add (cond ((LgC-EditCheck $value nil)) (add))) 2 2)) (LgC-Results bit)")
    (action_tile "Export" "(setq CSV (vl-remove nil (append (list (LgC-Export)) CSV)))")
    (action_tile "accept" "(setenv \"LONGCUMUL_Settings\" (itoa bit)) (done_dialog)")
    (start_dialog)
    (unload_dialog DCL_ID)
    (setq filename (not (vl-file-delete filename)))
    (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))))
  )
  
  (setq
    param
      (list
        (cons "LAYERS" '())
        (cons "OBJECT" '())
        (cons "SELALL" nil)
      )
    type-list
      (list
        (cons (LgT "Arc" "Arc" nil) "ARC")
        (cons (LgT "Circle" "Cercle" nil) "CIRCLE")
        (cons (LgT "Ellipse" "Ellipse" nil) "ELLIPSE")
        (cons (LgT "Line" "Ligne" nil) "LINE")
        (cons (LgT "Polyline" "Polyligne" nil) "LWPOLYLINE")
        (cons (LgT "3D Polyline" "Polyligne 3D" nil) "POLYLINE")
        (cons (LgT "Spline" "Spline" nil) "SPLINE")
      )
    bit (atoi (cond ((getenv "LONGCUMUL_Settings")) ((setenv "LONGCUMUL_Settings" "251"))))
    bitlist
      (list
        (cons "Det_Layers"  1)
        (cons "Det_Objects" 2)
        (cons "Det_Lengths" 4)
        (cons "Dis_Count"   8)
        (cons "Dis_Length"  16)
        (cons "Dis_Max"     32)
        (cons "Dis_Min"     64)
        (cons "Dis_Avg"     128)
        (cons "Dis_Dev"     256)
      )
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("DIMZIN" DIMZIN 0) ("LUPREC" LUPREC 2) ("INSUNITS" u nil)))
  (while
    (and
      (null break)
      (null (LgC-princ-param))
      (setq mode
        (getkdh
          (quote (entsel msg))
          (LgT
            "\nSelect an object or"
            "\nSélectionner un objet ou"
            nil
          )
          (list
            (LgT
              "Layers Objects All Undo eXit _Layers Objects All Undo eXit"
              "Calques Objets Tous annUler Quitter _Layers Objects All Undo eXit"
              nil
            )
          )
          " : "
          "eXit"
          (LgT
            (strcat
              "\nLONGCUMUL : Filter definition for selection"
              "\nDefault value:     eXit"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Select    | Select manually a linear object to add its layer property and       |"
              "\n  |             | object type property as filter. You can select one or more objects  |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Layers    | Opens a dialog box with the list of all layers name and let you     |"
              "\n  |             | select one or more layers within the list                           |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Objects   | Opens a dialog box with the list of all object types and let you    |"
              "\n  |             | select one or more object types within the list                     |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |     All     | Asks you to know if you want to select all objects with these       |"
              "\n  |             | properties (ignoring pre-selection) or if you have to select them   |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    Undo     | Deletes the last selected layer from the list. If you want to reset |"
              "\n  |             | the object type's list, use the Objects option                      |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    eXit     | Puts an end to the filter's definition and goes to the next step    |"
              "\n  |             | to select objects only in Model space                               |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            (strcat
              "\nLONGCUMUL : Définition du filtre pour la sélection"
              "\nValeur par défaut: Quitter"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Sélectionnez manuellement un objet linéaire pour ajouter ses        |"
              "\n  |  Sélection  | propriétés de calque et type d'objet au filtre. Vous pouvez         |"
              "\n  |             | sélectionner un ou plusieurs objets                                 |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Calques   | Ouvre une boîte de dialogue avec la liste de l'ensemble des calques |"
              "\n  |             | et vous permet de sélectionner un ou plusieurs calques              |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Objets    | Ouvre une boîte de dialogue avec la liste de l'ensemble des types   |"
              "\n  |             | d'objet et vous permet de sélectionner un ou plusieurs types        |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    Tous     | Demande si vous souhaitez sélectionner tous les objets avec ces     |"
              "\n  |             | propriétés (ignorant la présélection) ou si vous les sélectionnez   |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   annUler   | Supprime le dernier calque sélectionné de la liste. Si vous désirez |"
              "\n  |             | supprimer les types d'objet de la liste, utilisez l'option Objets   |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Quitter   | Met fin à la définition du filtre de sélection et passe à l'étape   |"
              "\n  |             | suivante pour effectuer la sélection dans l'espace Objet uniquement |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            nil
          )
        )
      )
    )
    (cond
      ( (= "Layers" mode)
        (setq
          lst
            (ListBox
              (LgT
                "LONGCUMUL: Layer(s) selection"
                "LONGCUMUL: Sélection des calque(s)"
                nil
              )
              (LgT
                "Please, select one or more layer(s) :"
                "Veuillez sélectionner un ou plusieurs calque(s) :"
                nil
              )
              (vl-sort (vl-remove-if '(lambda (x) (wcmatch x "*|*")) (mapcar 'car (vla-collection->list nil 'layers 1))) '<)
              (cond ((cdr (assoc "LAYERS" param))) ((getvar "CLAYER")))
              2
              nil
            )
          param
            (subst
              (cons
                "LAYERS"
                (append
                  (cdr (assoc "LAYERS" param))
                  (vl-remove-if '(lambda (x) (member x (cdr (assoc "LAYERS" param)))) lst)
                )
              )
              (assoc "LAYERS" param)
              param
            )
        )
      )
      ( (= "Objects" mode)
        (setq
          lst
            (ListBox
              (LgT
                "LONGCUMUL: Object type(s) selection"
                "LONGCUMUL: Sélection des type(s) d'objet"
                nil
              )
              (LgT
                "Please, select one or more object type(s) :"
                "Veuillez sélectionner un ou plusieurs type(s) d'objet :"
                nil
              )
              (mapcar 'car type-list)
              (mapcar 'car (cdr (assoc "OBJECT" param)))
              2
              nil
            )
          param (subst (cons "OBJECT" (if lst (mapcar '(lambda (x) (assoc x type-list)) lst) type-list)) (assoc "OBJECT" param) param)
        )
      )
      ( (= "All" mode)
        (setq mode
          (getkdh
            (quote (getkword msg))
            (LgT
              "\nDo you want to select all objects within the current tab or to select them manually (+ pre-selection)"
              "\nSouhaitez-vous sélectionner tous les objets présents sur l'onglet courant ou les sélectionner manuellement (+ présélection)"
              nil
            )
            (list
              (LgT
                "Yes No _Yes No"
                "Oui Non _Yes No"
                nil
              )
            )
            " ? "
            (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil))
            (LgT
              (strcat
                "\nLONGCUMUL : Selection mode for objects"
                "\nDefault value:     Current value (" (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil)) ")"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Entire database. If you specify this selection method and do not    |"
                "\n  |     Yes     | provide filters, it will ignore the pre-selection and select all    |"
                "\n  |             | entities in the database, including entities on layers that are     |"
                "\n  |             | off, frozen, and out the visible screen                             |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | If you didn't pre-select any entity before LONGCUMUL (PICKFIRST=1), |"
                "\n  |     No      | asks you to select objects manually (filters ON). Otherwise, uses   |"
                "\n  |             | the current selection set and apply the filters on it               |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              (strcat
                "\nLONGCUMUL : Mode de sélection des objets"
                "\nValeur par défaut: Valeur courante (" (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil)) ")"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Toute la base de données. Si vous spécifiez cette méthode de        |"
                "\n  |     Oui     | sélection sans fournir de filtre, elle ignorera la présélection et  |"
                "\n  |             | sélectionnera toutes les entités de la base de données, y compris   |"
                "\n  |             | les entités des calques inactifs, gelés, et hors écran visible      |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Si vous n'avez pas présélectionné d'entité avant LONGCUMUL          |"
                "\n  |     Non     | (PICKFIRST=1), demande de sélectionner les objets manuellement      |"
                "\n  |             | (filtres actifs). Sinon, utilise le jeu de sélection actuel et      |"
                "\n  |             | applique les filtres sur celui-ci                                   |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              nil
            )
          )
        )
        (setq param (subst (cons "SELALL" (cond ((= "Yes" mode) T) ((= "No" mode) nil))) (assoc "SELALL" param) param))
      )
      ( (= "Undo" mode)
        (setq param
          (subst (cons "LAYERS" (reverse (cdr (reverse (cdr (assoc "LAYERS" param)))))) (assoc "LAYERS" param) param)
        )
      )
      ( (and
          (listp mode)
          (setq mode (entget (car mode)))
          (setq typ (cdr (assoc 0 mode)))
          (setq typ (vl-position typ (mapcar 'cdr type-list)))
          (setq typ (nth typ type-list))
          (setq lay (cdr (assoc 8 mode)))
        )
        (if (not (member typ (cdr (assoc "OBJECT" param))))
          (setq param (subst (cons "OBJECT" (append (cdr (assoc "OBJECT" param)) (list typ))) (assoc "OBJECT" param) param))
        )
        (if (not (member lay (cdr (assoc "LAYERS" param))))
          (setq param (subst (cons "LAYERS" (append (cdr (assoc "LAYERS" param)) (list lay))) (assoc "LAYERS" param) param))
        )
      )
      ( (= "eXit" mode)
        (setq break T)
        (if (null (cdr (assoc "OBJECT" param))) (setq param (subst (cons "OBJECT" type-list) (assoc "OBJECT" param) param)))
      )
      ( T (princ (LgT "\nInvalid selection..." "\nSélection invalide..." nil)))
    )
  )
  (setq
    lst nil
    jsel
      (apply
        'ssget
        (vl-remove
          nil
          (list
            (if (cdr (assoc "SELALL" param)) "_X")
            (vl-remove
              nil
              (list
                (if (setq lst (mapcar 'cdr (cdr (assoc "OBJECT" param)))) (cons 0 (lst2str lst ",")))
                (if (setq lst (cdr (assoc "LAYERS" param))) (cons 8 (lst2str lst ",")))
                (cons 410 "Model")
              )
            )
          )
        )
      )
    lst
      (loop-a-list-properties
        jsel
        (list 8 0)
        (quote
          (list
            (list
              (vlax-curve-getDistAtParam (vlax-ename->vla-object name) (vlax-curve-getEndParam (vlax-ename->vla-object name)))
              (cdr (assoc 5 (entget name)))
            )
          )
        )
        'append
        T
      )
  )
  (if
    (setq lst
      (vl-sort
        (mapcar
          '(lambda (x / lay tmp)
            (setq
              lay (car x)
              tmp (cdr x)
            )
            (cons lay (vl-sort tmp '(lambda (e1 e2) (< (car e1) (car e2)))))
           )
          lst
        )
        '(lambda (e1 e2) (< (car e1) (car e2)))
      )
    )
    (progn
      (setq tmp (LgC-Results-DCL))
      (princ
        (strcat
          (LgT
            "\nObject types's list : "
            "\nListe des types d'objet : "
            nil
          )
          (lst2str
            (mapcar
              'car
              (vl-remove-if-not
                '(lambda (x) (member (cdr x) (mapcar 'car (apply 'append (mapcar 'cdr lst)))))
                type-list
              )
            )
            ", "
          )
          "\n­"
          "\n   []+-+-+-+-+-+-+-+-+-+[]\n -> "
          (lst2str
            (mapcar
              '(lambda (x / lay tmp lng)
                (setq
                  lay (car x)
                  tmp (cdr x)
                  lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr tmp)))
                )
                (strcat
                  (LgT
                    "Layer \""
                    "Calque \""
                    nil
                  )
                  lay
                  "\" :"
                  (if (LgC-bit (cdr (assoc "Dis_Length" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Total length      = "
                        "Longueur totale = "
                        nil
                      )
                      (ThsdSpace (rtos (apply '+ lng) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Count" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Number of objects = "
                        "Nombre d'objets = "
                        nil
                      )
                      (ThsdSpace (itoa (length lng)) (LgT "," " " nil)) "u"
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Max" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Maximum length    = "
                        "Longueur max.   = "
                        nil
                      )
                      (ThsdSpace (rtos (apply 'max lng) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Min" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Minimum length    = "
                        "Longueur min.   = "
                        nil
                      )
                      (ThsdSpace (rtos (apply 'min lng) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Avg" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Average length    = "
                        "Longueur moy.   = "
                        nil
                      )
                      (ThsdSpace (rtos (/ (apply '+ lng) (float (length lng))) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Dev" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Standard deviation= "
                        "Ecart type      = "
                        nil
                      )
                      (ThsdSpace
                        (rtos
                          (sqrt (/ (apply '+ (mapcar '(lambda (x) (expt (- x (/ (apply '+ lng) (float (length lng)))) 2)) lng)) (float (length lng))))
                          2
                          2
                        )
                        (LgT "," " " nil)
                      )
                      (units u)
                    )
                    ""
                  )
                )
               )
              lst
            )
            "\n   []+-+-+-+-+-+-+-+-+-+[]\n -> "
          )
          "\n­"
          "\nTOTAL :"
          (LgT
            "\n  - Total length         = "
            "\n  - Longueur totale      = "
            nil
          )
          (ThsdSpace (rtos (apply '+ tmp) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Number of objects    = "
            "\n  - Nombre d'objets      = "
            nil
          )
          (ThsdSpace (itoa (length tmp)) (LgT "," " " nil)) "u"
          (LgT
            "\n  - Maximum length       = "
            "\n  - Longueur max.        = "
            nil
          )
          (ThsdSpace (rtos (apply 'max tmp) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Minimum length       = "
            "\n  - Longueur min.        = "
            nil
          )
          (ThsdSpace (rtos (apply 'min tmp) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Average length       = "
            "\n  - Longueur moy.        = "
            nil
          )
          (ThsdSpace (rtos (/ (apply '+ tmp) (float (length tmp))) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Standard deviation   = "
            "\n  - Ecart type           = "
            nil
          )
          (ThsdSpace
            (rtos (sqrt (/ (apply '+ (mapcar '(lambda (x) (expt (- x (/ (apply '+ tmp) (float (length tmp)))) 2)) tmp)) (float (length tmp)))) 2 2)
            (LgT "," " " nil)
          )
          (units u)
          "\n  - Coefficient (x)      = " (ThsdSpace (rtos coeff 2 2) (LgT "," " " nil))
          (LgT
            "\n  - Add length (+)       = "
            "\n  - Longueur ajoutée (+) = "
            nil
          )
          (ThsdSpace (rtos add 2 2) (LgT "," " " nil)) (units u)
          "\n­"
        )
      )
      (setq i 0)
      (mapcar
        '(lambda (f)
          (princ
            (strcat
              (LgT
                "\nFile name n°"
                "\nNom du fichier n°"
                nil
              )
              (itoa (setq i (1+ i)))
              " : "
              f
            )
          )
        )
        CSV
      )
      (textscr)
    )
    (princ
      (LgT
        "\nNo object selected with the current filter..."
        "\nAucun objet sélectionné avec le filtre actuel..."
        nil
      )
    )
  )
  (SetVarList '(("DIMZIN" nil DIMZIN) ("LUPREC" nil LUPREC)))
  (sssetfirst nil jsel)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)