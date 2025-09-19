
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  prompt-list  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] prompt-list []-----------------------[]                                        ;
;--- Date of creation       > 08/03/2021                                                                                                            ;
;--- Last modification date > 22/03/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to display a list in a more readable way in the order history by adding indents.                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (prompt-list) have 2 argument(s) :                                                                                                   ;
;   --•  lst                    > corresponds to the list we want to display                                                                        ;
;     (type lst) = 'LST                         | Ex. : (entget (car (entsel))), '((0 1) 2 (3 4) (5 6 7) 8 (9)), ...                                ;
;   --•  fun                    > corresponds to the sorting we want to apply to the list and corresponds to the function associated with (vl-sort) ;
;     (type fun) = 'SYM                         | Ex. : '<, '>, '(lambda (x) ...), ...                                                              ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> space                                         | v1.0.0 - 22/02/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "UtDis" ---> lstprompt                                     | v1.0.0 - 08/03/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (prompt-list) returns nil, however it allows to display its result as it is executed in the command history.                       ;
;     Ex. : (prompt-list (entget (entlast)) nil) returns                                                                                            ;
;               (                                                                                                                                   ;
;                  (-1 . <Entity name: 18b0eeb0d40>)                                                                                                ;
;                  (0 . "LWPOLYLINE")                                                                                                               ;
;                  (330 . <Entity name: 18b0eeb29f0>)                                                                                               ;
;                  (5 . "234")                                                                                                                      ;
;                  (100 . "AcDbEntity")                                                                                                             ;
;                  (67 . 0)                                                                                                                         ;
;                  (410 . "Model")                                                                                                                  ;
;                  (8 . "0")                                                                                                                        ;
;                  (100 . "AcDbPolyline")                                                                                                           ;
;                  (90 . 2)                                                                                                                         ;
;                  (70 . 1)                                                                                                                         ;
;                  (43 . 0.0)                                                                                                                       ;
;                  (38 . 0.0)                                                                                                                       ;
;                  (39 . 0.0)                                                                                                                       ;
;                  (                                                                                                                                ;
;                     10                                                                                                                            ;
;                     13.2926                                                                                                                       ;
;                     9.9261                                                                                                                        ;
;                  )                                                                                                                                ;
;                  (40 . 0.0)                                                                                                                       ;
;                  (41 . 0.0)                                                                                                                       ;
;                  (42 . 0.0)                                                                                                                       ;
;                  (91 . 0)                                                                                                                         ;
;                  (                                                                                                                                ;
;                     10                                                                                                                            ;
;                     16.0547                                                                                                                       ;
;                     12.8817                                                                                                                       ;
;                  )                                                                                                                                ;
;                  (40 . 0.0)                                                                                                                       ;
;                  (41 . 0.0)                                                                                                                       ;
;                  (42 . 0.0)                                                                                                                       ;
;                  (91 . 0)                                                                                                                         ;
;                  (                                                                                                                                ;
;                     210                                                                                                                           ;
;                     0.0                                                                                                                           ;
;                     0.0                                                                                                                           ;
;                     1.0                                                                                                                           ;
;                  )                                                                                                                                ;
;               )                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun prompt-list (lst fun / lstprompt i)
  (defun lstprompt (lst / l)
    (prompt (strcat "\n" (space (* (1- i) 3)) "("))
    (foreach l (if fun (vl-sort lst fun) lst)
      (if (and (listp l) (vl-list-length l))
        (progn (setq i (1+ i)) (lstprompt l) (setq i (1- i)))
        (prompt (strcat "\n" (space (* i 3)) (vl-prin1-to-string l)))
      )
    )
    (prompt (strcat "\n" (space (* (1- i) 3)) ")"))
  )

  (setq i 1)
  (lstprompt lst)
  (princ)
)