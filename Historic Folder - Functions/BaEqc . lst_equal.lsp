
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                              FICHIER DE SUIVI HISTORIQUE DE LA FONCTION lst_equal                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] lst_equal []-----------------------[]                                         ;
;--- Date of creation       > 25/05/2019                                                                                                            ;
;--- Last modification date > 21/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.1                                                                                                                 ;
;--- Class                  > "BaEqc"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Checks the equality between two lists of identical length by comparing each term one by one. Similar to the (equal) function except that        ;
;   (lst_equal) also checks the type of each data.                                                                                                  ;
;     --> OBSOLETE ?                                                                                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (lst_equal) have 2 argument(s) :                                                                                                     ;
;   --•  lst1                   > corresponds to the first list evaluated                                                                           ;
;     (type lst1) = 'LST                        | Ex. : '(1 2 3.0 4 "5" "test" ...), ...                                                            ;
;   --•  lst2                   > corresponds to the second list evaluated                                                                          ;
;     (type lst2) = 'LST                        | Ex. : '(1 2 3.0 4 "5" "test" ...), ...                                                            ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (lst_equal) returns T if the lists are equal, nil then.                                                                            ;
;     Ex. : (lst_equal '(1 2 3) '(1 2 3)) returns T                                                                                                 ;
;           (lst_equal '(1 2 3) '(1 2 3.0)) returns nil, where (equal '(1 2 3) '(1 2 3.0)) returns T                                                ;
;           (lst_equal '(1 2 3) '(1 2 3 4)) returns nil, just like (equal '(1 2 3) '(1 2 3 4))                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.1   |   Modification of the naming of variables and arguments, deletion of the variable 'rslt', modification of the writing to stop    | ;
; |            |   at the slightest difference (time saving)                                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Not found in historic                                                                                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun lst_equal (lst1 lst2)

  (if (= (vl-position (last lst1) lst1) (vl-position (last lst2) lst2))
    (while
      (and
        lst1
        lst2
        (= (type (car lst1)) (type (car lst2)))
        (= (car lst1) (car lst2))
      )
      (setq lst1 (cdr lst1)
            lst2 (cdr lst2)
      )
    )
  )
  (if (and
        (null lst1)
        (null lst2)
      )
    T
    nil
  )

)