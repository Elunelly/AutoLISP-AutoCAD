
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  FormAssoc  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] FormAssoc []-----------------------[]                                         ;
;--- Date of creation       > 30/06/2022                                                                                                            ;
;--- Last modification date > 30/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Based on a list defining the format of the result, uses an association list to retrieves the value of specific pair to transform the format     ;
;   into the results.                                                                                                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (FormAssoc) have 3 argument(s) :                                                                                                     ;
;   --•  format                 > is the list we want to use as model for the results. The type of each element determines how the program will use ;
;                                 them :                                                                                                            ;
;                                   • If the element is a symbol name, retrieves its value (example a quoted variable name)                         ;
;                                   • If the element is a list, each atom within the list corresponds to a key to search into the given association ;
;                                     list. The order of the key determines the order of priority between each key to be evaluated. The first one   ;
;                                     in the list will be the first one to be searched, if no results, search the next one, etc...                  ;
;                                     If the last element of the list is a list (i.e quoted (lambda)), it will apply the function on the value, the ;
;                                     key or (cons key value) depending on 'flag' value                                                             ;
;                                   • Otherwise, returns the element directly                                                                       ;
;     (type format) = 'LST                      | Ex. : (list (list "N°_DESSIN" "INDICE" '(lambda (x) (substr x 1 4))) "-" '("PLANCHE")), ...       ;
;   --•  lst                    > is the association list to be used to search the keys and retrieve their values                                   ;
;     (type lst) = 'LST                         | Ex. : (get-att-list), (entget), ...                                                               ;
;   --•  flag                   > is a bit-coded integer to determines how the program have to work with some functionnalities. The bits can be     ;
;                                 added together in any combination to form a value between 0 and 31 :                                              ;
;                                   • 1  (bit 0) - The research for the 'key' is not case-sensitive if it's a string                                ;
;                                   • 2  (bit 1) - Ignore the value if it's an empty string                                                         ;
;                                   • 4  (bit 2) - Apply the function (lst2str) on the results to get a string instead of a list                    ;
;                                   • 8  (bit 3) - Consider only the 'value' as result for the search and to apply the (lambda) function if true    ;
;                                   • 16 (bit 4) - Consider only the 'key' as result for the search and to apply the (lambda) function if true      ;
;     (type flag) = 'INT                        | Ex. : 0, 1, 15, ...                                                                               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaEqc" ---> bit                                           | v1.0.0 - 30/06/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (FormAssoc) returns [...]                                                                                                                ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun FormAssoc (format lst flag)
  (if (bit 1 flag)
    (setq lst (mapcar '(lambda (x) (if (= 'STR (type (car x))) (cons (strcase (car x)) (cdr x)) x)) lst))
  )
  (setq lst
    (mapcar
      '(lambda (a / b key value)
        (setq b a)
        (cond
          ( (vl-symbolp a) (vl-symbol-value a))
          ( (not (listp a)) a)
          ( (while
              (and
                (car b)
                (not (or (listp (car b)) (vl-symbolp (car b))))
                (if (and (= 'STR (type (car b))) (bit 1 flag))
                  (setq key (strcase (car b)))
                  (setq key (car b))
                )
                (or
                  (null (setq value (cdr (assoc key lst))))
                  (if (bit 2 flag) (= "" value))
                )
                (setq b (cdr b))
              )
              (setq value nil)
            )
          )
          ( (or (listp (last a)) (vl-symbolp (last a)))
            (apply
              (last a)
              (list
                (cond
                  ( (bit 24 flag) (cons key value))
                  ( (bit 8 flag) value)
                  ( (bit 16 flag) key)
                )
              )
            )
          )
          ( (bit 24 flag) (cons key value))
          ( (bit 8 flag) value)
          ( (bit 16 flag) key)
        )
       )
      format
    )
  )
  (cond
    ( (member nil lst) nil)
    ( (bit 4 flag) (lst2str lst ""))
    (lst)
  )
)