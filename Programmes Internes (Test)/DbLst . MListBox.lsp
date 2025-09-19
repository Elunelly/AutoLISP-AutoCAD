
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  MListBox  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] MListBox []-----------------------[]                                         ;
;--- Date of creation       > 15/06/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DbLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (MListBox) have # argument(s) :                                                                                                           ;
;   --•  ...                    >                                                                                                                   ;
;     (type ...) = '...                         | Ex. :                                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (MListBox) returns [...]                                                                                                                ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun ListBox (title msg lst value h / vl-list-search LB-select tmp file DCL_ID choice tlst)
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
    (strcat
      " MListBox:dialog{"
      "   label = \"" title "\" ;"
      "   width = " (itoa (+ (apply 'max (mapcar 'strlen (mapcar 'vl-princ-to-string lst))) 5)) " ;"
      "   :edit_box {"
      "     key = \"filter\" ;"
      "   }"
      (cond
        ( (and msg (/= msg ""))
          (strcat
            "   :text {"
            "     label = \"" msg "\" ;"
            "   }"
          )
        )
        ("   spacer ;")
      )
      "   :list_box {"
      "     height = " (itoa (1+ (cond (h) (15)))) " ;"
      "     key = \"lst\" ;"
      "     multiple_select = true ;"
      "   }"
      "   :text {"
      "     key = \"select\" ;"
      "   }"
      "   :text {"
      "     key = \"count\" ;"
      "   }"
      "   spacer ;"
      "   :toggle {"
      "     label = \"Select all\" ;"
      "     key = \"All\" ;"
      "   }"
      "   :radio_column {"
      "     :radio_button {"
      "       label = \"Replace the current selection\" ;"
      "       key = \"Replace\" ;"
      "       value = 1 ;"
      "     }"
      "     :radio_button {"
      "       label = \"Include in current selection\" ;"
      "       key = \"Include\" ;"
      "     }"
      "     :radio_button {"
      "       label = \"Exclude from current selection\" ;"
      "       key = \"Exclude\" ;"
      "     }"
      "   }"
      "   spacer ;"
      "   ok_cancel ;"
      " }"
    )
    file
  )
  (close file)
  (setq DCL_ID (load_dialog tmp))
  (if (not (new_dialog "MListBox" DCL_ID))
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
      ( (listp value)
        (apply 'strcat (vl-remove nil (mapcar '(lambda (x) (if (member x lst) (strcat (itoa (vl-position x lst)) " "))) value)))
      )
      ( (member value lst) (itoa (vl-position value lst)))
      ( (itoa 0))
    )
  )
  (set_tile "select" (LB-select (get_tile "lst")))
  (action_tile "lst" "(set_tile \"select\" (LB-select $value))")
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
      (progn
        (foreach n (str2lst (get_tile \"lst\") \" \")
          (setq choice (cons (nth (atoi n) tlst) choice))
        )
        (setq choice (reverse choice))
      )
    )
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete tmp)
  choice
)