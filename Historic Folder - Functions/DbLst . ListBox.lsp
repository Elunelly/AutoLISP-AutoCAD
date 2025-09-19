
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  ListBox  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ListBox []-----------------------[]                                          ;
;--- Date of creation       > 15/04/2017                                                                                                            ;
;--- Last modification date > 11/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.0.0                                                                                                                 ;
;--- Class                  > "DbLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Generates a temporary .dcl file for a single/multiple item selection dialog box in a list passed as an argument.                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ListBox) have 5 argument(s) :                                                                                                       ;
;   --•  title                  > corresponds to the title of the dialog box                                                                        ;
;     (type title) = 'STR                       | Ex. : "Layers selection", "VP-RADPURGE: Layouts selection", "", ...                               ;
;   --•  msg                    > corresponds to the message you wish to display above the popup list                                               ;
;     (type msg) = 'STR                         | Ex. : "Multiple choice :", "Please, select one or more layer in the list below...", "", ...       ;
;   --•  lst                    > corresponds to the list you wish to display in the popup-list. Each element of the list have to be a string       ;
;     (type lst) = 'LST                         | Ex. : '(label1 label2 label3 ... labeln), (layoutlist), ...                                       ;
;   --•  value                  > is the default value to be selected when the dialog box is opened or list of default values                       ;
;     (type value) = 'STR or 'LST               | Ex. : (getvar "CLAYER"), "1030-01", ...                                                           ;
;   --•  flag                   > corresponds to the type of popup-list between a drop-down list, a single or multiple choice list                  ;
;     (type flag) = 'INT                        | Ex. : 0 = drop-down list, 1 = single choice list or 2 = multiple choice list                      ;
;   --•  h                      > corresponds to the number of lines prompted in the list for list_box tiles                                        ;
;     (type h) = 'INT                           | Ex. : nil, 8, 15, 30, 23, ...                                                                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2007 ((gile))                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> vl-list-search                                | v1.0.0 - 13/12/2021 (Luna)                                                    ;
;   --•  "BaStr" ---> LB-select                                     | v1.0.0 - 11/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsLaytb" ---> c:QuickLayoutSwitch                         | v3.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (ListBox) returns the value ('flag' = 0 or 1) or the list of values ('flag' = 2) chosen by the user.                               ;
;     Ex. : (ListBox "Layout Selection" "Select one layout :" (layoutlist) (getvar "CTAB") 1) returns the layout name selected or nil if canceled   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.0.0   |   Add the 'h' argument to adjust the height of the list (= flag equals 1 or 2) and add the number of selected line if flag = 2   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.1.0   |   Add the possibility to set several default values if 'flag' = 2 and 'value' is a list type                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Add the (vl-list-search) function as a local function, for (ListBox) use only                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Added an edit_box adding the possibility to filter the list from a search (cf. (wcmatch) wildcard characters). To display the  | ;
; |   v3.0.0   |   complete list, you must put a "*" in the search bar. The number of items displayed as well as the total number of items in     | ;
; |            |   the list are displayed below the list as information. Added a local variable 'tlst' to avoid losing the original list data     | ;
; |            |   while returning the correct results when a filter is applied. Added the (vl-list-search) function to the (ListBox) function.   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |            |   Removed the association list to work directly with a list of atoms and returns directly the value(s) selected by the user.     | ;
; |   v2.0.0   |   Also added a default value to initialize the dialog box's list (if specified). Adapted the global width of the dialog box      | ;
; |            |   according to the longest text in the list.                                                                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

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