
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  sort-list  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] sort-list []-----------------------[]                                         ;
;--- Date of creation       > 20/03/2020                                                                                                            ;
;--- Last modification date > 06/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.3.1                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to sort a list by ignoring the case of strings and sorting the elements by their pattern and then by their numerical indexing.       ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (sort-list) have 2 argument(s) :                                                                                                     ;
;   --•  lst                    > corresponds to the list we want to sort                                                                           ;
;     (type lst) = 'LST                         | Ex. : '("Layer10" "Bloc" 8 "Layer1" "Layer3" "Bloc5" 4 88 "90" "Bloc30" "Layer7"), ...            ;
;   --•  fun                    > determines the function we want to apply to sort the list                                                         ;
;     (type fun) = 'SYM                         | Ex. : '< (= ascending order), '> (= descending order), ...                                        ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> get-pattern-list                              | v3.0.0 - 25/06/2020 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> DXF_List                                      | v1.2.2 - 06/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (sort-list) returns the list sorted according to two levels: the first level corresponds to the pattern of each element of the     ;
;   list (obtained thanks to (get-pattern-list)) ignoring the case and the second level corresponds to the numerical index for each pattern. The    ;
;   type of the elements does not affect the sorting of the elements.                                                                               ;
;     Ex. : (sort-list '("Layer10" "Bloc" 8 "Layer1" "Layer3" "Bloc5" 4 88 "90" "Bloc30" "Layer7") '<) returns                                      ;
;             (4 8 88 "90" "Bloc" "Bloc5" "Bloc30" "Layer1" "Layer3" "Layer7" "Layer10")                                                            ;
;           (sort-list '("Layer10" "Bloc" 8 "Layer1" "Layer3" "Bloc5" 4 88 "90" "Bloc30" "Layer7") '>) returns                                      ;
;             ("Layer10" "Layer7" "Layer3" "Layer1" "Bloc30" "Bloc5" "Bloc" "90" 88 8 4)                                                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.3.1   |   Correction since the release of (get-pattern-list) 3.0.0 and renames 'flag' into 'fun"                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.3.0   |   Modification of the function (get-pattern-list) to the version 3.0.0 with one more argument                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   Modified the use of the flag argument for more readability and flexibility on the sorting method to apply                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Modification of the function (get-pattern-list) associated with it, correction of the bugs induced in the case of a string     | ;
; |            |   containing only numbers and optimization of the calculation in a single loop                                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun sort-list (lst fun)
  (mapcar 'car
    (vl-sort
      (get-pattern-list lst 2)
      '(lambda (a b)
        (setq a (cadr a) b (cadr b))
        (if (= (car a) (car b))
          (if (= (cadr a) (cadr b))
            ((eval fun) (atoi (caddr a)) (atoi (caddr b)))
            ((eval fun) (cadr a) (cadr b))
          )
          ((eval fun) (atoi (car a)) (atoi (car b)))
        )
       )
    )
  )
)