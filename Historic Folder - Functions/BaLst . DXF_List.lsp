
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  DXF_List  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] DXF_List []-----------------------[]                                         ;
;--- Date of creation       > 27/05/2019                                                                                                            ;
;--- Last modification date > 06/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.2.2                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to modify a list in order to concatenate all the elements with a separator string, into a single string. Also allows the list to be  ;
;   sorted if required. The separator string can be placed to the left or right of each element in the list and the last occurrence (or the first,  ;
; depending on the initial position of the separator string) can be deleted or not. Removal of duplicates in the case of a specified sort.          ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (DXF_List) have 5 argument(s) :                                                                                                      ;
;   --•  lst                    > corresponds to a list of elements that we wish to concatenate                                                     ;
;     (type lst) = 'LST                         | Ex. : (layoutlist), '("Block1" "Block2" "Block3"), ...                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> remove-duplicates                             | v2.0.0 - 29/04/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> sort-list                                     | v1.3.1 - 06/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (DXF_List) returns the list (sorted or not) in case the 'Pos' argument has not been specified (different from "left" or "right").  ;
;   This list can be sorted via the (sort-list) function in ascending alphanumeric order according to the value of the 'Tri' argument (if set to    ;
;   "", no sorting is performed). Otherwise returns a string consisting of all the elements of the input list separated by the specified separator  ;
;   string. If the specified position is "left" the 'Sup' argument will remove the first occurrence of the separator string in the newly created    ;
;   string. If the specified position is "right" the 'Sup' argument will remove the last occurrence of the separator string in the string.          ;
;     Ex. : (DXF_List '("This" "is" "a" "test") " " "right" nil nil) returns "This is a test "                                                      ;
;           (DXF_List '("This" "is" "a" "test") " " "right" nil T) returns "This is a test"                                                         ;
;           (DXF_List '("This" "is" "a" "test") " " "right" T T) returns "a is test This"                                                           ;
;           (DXF_List '("This" "is" "a" "test") nil nil T T) returns '("a" "is" "test" "This")                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.2   |   Renamming (Remove-Double) as (remove-duplicates)                                                                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.1   |   Modification of the function (sort-list) v1.2.0 dated 25/06/2020, modification of the name of the arguments and removal of the | ;
; |            |   local variable 'New_List'                                                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   Modification of the value sorting function in order to limit the errors induced by the function (vl-sort), declaration of the  | ;
; |            |   'New_List' variable as a local variable and taking into account elements of all types                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Addition of the function (Remove-Double) in the case of a sort of the initial list and modification of the type of the 'Tri'   | ;
; |            |   and 'Sup' arguments                                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun DXF_List (lst str pos tri sup)
  (if tri
    (setq lst (sort-list (remove-duplicates lst) '<))
  )
  (cond
    ( (and str (= pos "left"))
      (setq lst (apply 'strcat (mapcar '(lambda (x) (strcat str (vl-princ-to-string x))) lst)))
      (if sup
        (setq lst (vl-string-left-trim str lst))
      )
    )
    ( (and str (= pos "right"))
      (setq lst (apply 'strcat (mapcar '(lambda (x) (strcat (vl-princ-to-string x) str)) lst)))
      (if sup
        (setq lst (vl-string-right-trim str lst))
      )
    )
  )
  lst
)