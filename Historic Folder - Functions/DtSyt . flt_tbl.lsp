
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  flt_tbl  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] flt_tbl []-----------------------[]                                          ;
;--- Date of creation       > 16/04/2019                                                                                                            ;
;--- Last modification date > 20/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.0.0                                                                                                                 ;
;--- Class                  > "DtSyt"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Creation of a list of values from a Symbol Table ("APPID", "BLOCK", "DIMSTYLE", "LAYER", "LTYPE", "STYLE", "UCS", "VIEW" ou "VPORT") with       ;
;   application of a search filter based on the function (wcmatch). The main goal is to get a list of values from one of the 9 Symbol Table.        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (flt_tbl) have 2 argument(s) :                                                                                                       ;
;   --•  tag                    > corresponds to the name of the Symbol Table in which the search have to be done                                   ;
;     (type tag) = 'STR                         | Ex. : "APPID", "BLOCK", "DIMSTYLE", "LAYER", "LTYPE", "STYLE", "UCS", "VIEW", "VPORT"             ;
;   --•  search                 > is a string used as a filter for values that you want to keep in the list (cf. wcmatch)                           ;
;     (type search) = 'STR                      | Ex. : "UBS*", "Cartouche*", "*1##-*", ...                                                         ;
;   --•  flag                   > determines if the search is case-sensitive or not                                                                 ;
;     (type flag) = 'SYM                        | Ex. : T for case-sensitive, nil if not                                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (flt_tbl) returns a list containing all the entries in the Symbol Table which match to the 'search' pattern (case sensitive).      ;
;     Ex. : (flt_tbl "LAYER" "Layer*") returns ("Layer1" "Layer2" "Layer3" "Layers")                                                                ;
;           (flt_tbl "LAYER" "Layer#") returns ("Layer1" "Layer2" "Layer3")                                                                         ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.0.0   |   Add 'flag' argument to specify if the search is case-sensitive or not                                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.2   |   Changing the name of variables                                                                                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.1   |   Add local variable declaration for all variables                                                                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Removal of the 'get' argument, removal of the creation of a pointed list, possibility of using the function on all Symbol      | ;
; |            |   Tables, generalisation of the use of the function to no longer be limited to filters (ssget)                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   Adding the declaration of local variables (paritally)                                                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Added Symbol Table "LTYPE", added 'get' argument, possibility to change the output format depending on the use of the          | ;
; |            |   function (flt_tbl) for a less specific use                                                                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun flt_tbl (tag search flag / lst value)
  (setq value (cdr (assoc 2 (tblnext tag T))))
  (while (/= value nil)
    (if
      (=
        T
        (wcmatch
          (if flag value (strcase value))
          (if flag search (strcase search))
        )
      )
      (setq lst (cons value lst))
    )
    (setq value (cdr (assoc 2 (tblnext tag))))
  )
  lst
)