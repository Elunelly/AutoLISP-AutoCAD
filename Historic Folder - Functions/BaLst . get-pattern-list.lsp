
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  get-pattern-list  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] get-pattern-list []-----------------------[]                                     ;
;--- Date of creation       > 20/03/2020                                                                                                            ;
;--- Last modification date > 25/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   For each element of a given list, allows to separate a numerical index at the end and/or the beginning of the string which is identified as     ;
;   pattern. Each element of the initial list is therefore decomposed in the form of a list with the initial value of the element (type             ;
;   conservation) as a header, then the decomposition list corresponding to the numerical index at the beginning of the string, then to the pattern ;
;   (character string from which the numerical values at the end of the string have been removed) followed by its numerical index at the end of the ;
;   string (kept in the 'STR format like the first one).                                                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-pattern-list) have 2 argument(s) :                                                                                              ;
;   --•  lst                    > corresponds to the list of simple elements that we want to decompose                                              ;
;     (type lst) = 'LST                         | Ex. : '("Layer10" "Bloc" 8 "Layer1" "Layer3" "Bloc5" 4 88 "90" "Bloc30" "Layer7"), ...            ;
;   --•  flag                   > determines if you want to ignore the case, which can alter the way of sorting afterwards. The alphabetical        ;
;                               sorting is done according to the ascii value of the characters, so the lower case letters (from 97 to 122 in ascii  ;
;                               value) located after the upper case letters (from 65 to 90 in ascii value)                                          ;
;     (type flag) = 'INT                        | Ex. : 0 to get uppercase patterns, 1 to get lowercase patterns or 2 to keep the format of pattern ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> sort-list                                     | v1.3.1 - 06/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-pattern-list) returns a list composed of pointed pairs whose header corresponds to the initial element, and whose value       ;
;   corresponds to a list composed of the numerical index at the beginning of the string (if the element does not have an index at the beginning of ;
;   the string, the pattern is equivalent to "") the identified pattern whose case depends on the value of the flag argument (if the element does   ;
;   not have a pattern, i.e. it is only a sequence of numbers, the pattern is equivalent to "") and its numerical index located at the end of the   ;
;   string (if the element does not have an index at the end of the string, it is equivalent to ""). This list is then processed by the function    ;
;   (sort-list).                                                                                                                                    ;
;     Ex. : (get-pattern-list '("000-UBS-Modules 2" "0" "Pvcase Offset" "TEXT_101" "Text_2") 0) returns                                             ;
;               ( ("000-UBS-Modules 2" ("000" "-UBS-MODULES " "2"))                                                                                 ;
;                 ("0" ("0" "" ""))                                                                                                                 ;
;                 ("Pvcase Offset" ("" "PVCASE OFFSET" ""))                                                                                         ;
;                 ("TEXT_101" ("" "TEXT_" "101"))                                                                                                   ;
;                 ("Text_2" ("" "TEXT_" "2"))                                                                                                       ;
;               )                                                                                                                                   ;
;           (get-pattern-list '("000-UBS-Modules 2" "0" "Pvcase Offset" "TEXT_101" "Text_2") 1) returns                                             ;
;               ( ("000-UBS-Modules 2" ("000" "-ubs-modules " "2"))                                                                                 ;
;                 ("0" ("0" "" ""))                                                                                                                 ;
;                 ("Pvcase Offset" ("" "pvcase offset" ""))                                                                                         ;
;                 ("TEXT_101" ("" "text_" "101"))                                                                                                   ;
;                 ("Text_2" ("" "text_" "2"))                                                                                                       ;
;               )                                                                                                                                   ;
;           (get-pattern-list '("000-UBS-Modules 2" "0" "Pvcase Offset" "TEXT_101" "Text_2") 2) returns                                             ;
;               ( ("000-UBS-Modules 2" ("000" "-UBS-Modules " "2"))                                                                                 ;
;                 ("0" ("0" "" ""))                                                                                                                 ;
;                 ("Pvcase Offset" ("" "Pvcase Offset" ""))                                                                                         ;
;                 ("TEXT_101" ("" "TEXT_" "101"))                                                                                                   ;
;                 ("Text_2" ("" "Text_" "2"))                                                                                                       ;
;               )                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Added recognition of the numerical index at the beginning of the string and added the flag argument to take into account the   | ;
; |            |   case of the patterns obtained                                                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Removal of the function (vl-remove), correction of the method applied to avoid the bugs encountered for strings composed only  | ;
; |            |   of numerical values                                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-pattern-list (lst flag)
  (mapcar
    '(lambda (x / tag str value)
      (if (= "." (setq str (vl-string-trim "0123456789" (setq tag (vl-princ-to-string x)))))
        (setq str "")
      )
      (setq
        value
          (list
            (if (wcmatch tag "#*")
              (if (= str "")
                tag
                (substr tag 1 (vl-string-search (substr str 1 1) tag))
              )
              ""
            )
            (cond
              ((= flag 0) (strcase str))
              ((= flag 1) (strcase str t))
              (t str)
            )
            (if
              (and
                (/= str "")
                (wcmatch tag "*#")
              )
              (substr tag (1+ (strlen (vl-string-right-trim "0123456789" tag))))
              ""
            )
          )
      )
      (cons x (list value))
     )
    lst
  )
)