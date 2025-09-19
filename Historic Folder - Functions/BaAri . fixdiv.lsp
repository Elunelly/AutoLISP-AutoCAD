
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  fixdiv  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] fixdiv []-----------------------[]                                           ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 25/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to calculate a pseudo-egalitarian integer division for a total number of elements by specifying the maximum number to be taken into  ;
;   account for the result of the division. The aim is to determine an integer number that balances a division into equal parts (except for the     ;
;   last part which, in some cases, will be different from the others).                                                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (fixdiv) have 2 argument(s) :                                                                                                        ;
;   --•  n                      > Is the number to be divided                                                                                       ;
;     (type n) = 'INT                           | Ex. : (length lst), 102, 4521, ...                                                                ;
;   --•  m                      > Is the maximum number not to be exceeded in each division                                                         ;
;     (type m) = 'INT                           | Ex. : 8, 15, 464, ...                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (fixdiv) returns the balanced number of elements required for equal separation between each division.                              ;
;     Ex. : (fixdiv 102 18) returns 17, i.e. (/ 102 17) = 6 equal shares instead of 5 divisions of 18 plus 1 division of 12.                        ;
;           (fixdiv 66 42) returns 33, i.e. (/ 66 33) = 2 equal shares instead of 1 division of 42 plus 1 division of 24.                           ;
;           (fixdiv 53 5) returns 5, i.e. (/ 53 5) = 10 divisions of 5 plus 1 division of 3 as there's no equalisation possible.                    ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Modification of the naming of variables and arguments and restructuring of the function.                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun fixdiv (n m / d)
  (cond
    ( (not
        (member
          nil
          (mapcar
            '(lambda (x) (= x (ascii "0")))
            (vl-string->list 
              (substr
                (vl-princ-to-string (setq d (/ n (atof (rtos m 2 1)))))
                (+ 2 (vl-string-position (ascii ".") (vl-princ-to-string d)))
              )
            )
          )
        )
      )
      (setq m (fix m))
    )
    ( T
      (if
        (not
          (member
            nil
            (mapcar
              '(lambda (x) (= x (ascii "0")))
              (vl-string->list
                (substr
                  (vl-princ-to-string (setq m (/ n (atof (rtos (1+ (fix d)) 2 1)))))
                  (+ 2 (vl-string-position (ascii ".") (vl-princ-to-string m)))
                )
              )
            )
          )
        )
        (setq m (fix m))
        (setq m (1+ (fix m)))
      )
    )
  )
  m
)