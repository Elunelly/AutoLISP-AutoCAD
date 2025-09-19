
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  MFiles-field-generator  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] MFiles-field-generator []-----------------------[]                                   ;
;--- Date of creation       > 19/12/2022                                                                                                            ;
;--- Last modification date > 19/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Generates a field code based on M-Files datas for each properties (MFiles ID's are retrieved manually, high risk of errors).                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (MFiles-field-generator) have 1 argument(s) :                                                                                        ;
;   --•  lst                    > is the list of desired properties numbers (cf. 'p' list), correctly ordered (based on M-Files possibilities). You ;
;                                 can use a real value instead of an integer to specify which index of the property will be retrieved (if several   ;
;                                 value)                                                                                                            ;
;     (type lst) = 'LST                         | Ex. : '(0 9.1 8), '(0 2), '(20 8), ...                                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaApp" ---> MFiles-ID-list                                | v1.0.0 - 19/12/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> lst2str                                       | v1.0.0 - 05/03/2021 (Luna)                                                    ;
;   --•  "UtDac" ---> SliceNumber                                   | v2.0.0 - 13/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (MFiles-field-generator) returns a dotted-pair list with the key corresponding to the textual equivalence of 'lst' with M-Files    ;
;   property names and the value corresponding to the field code.                                                                                   ;
;     Ex. : (MFiles-field-generator '(0 9 8)) returns                                                                                               ;
;           (                                                                                                                                       ;
;             "Projet(s) #1 > Dessinateur(s) Projeteur (s) #1 > Triptique"                                                                          ;
;             "%<\\M-Files PGFE106511CD7B4CBAAB72BA8DAAD04C46n1_PG79D94E3637014DE59F9279E3E3419FD0n1_PGF9CADF4B98E74C1993987A4D23E3164C>%"          ;
;           )                                                                                                                                       ;
;     Ex. : (MFiles-field-generator '(0 16.1 8)) returns                                                                                            ;
;           (                                                                                                                                       ;
;             "Projet(s) #1 > Contributeur - Chargé(s) d'Etude(s) #2 > Triptique"                                                                   ;
;             "%<\\M-Files PGFE106511CD7B4CBAAB72BA8DAAD04C46n1_PG6A29938E245640AA8A84F95B20E96EC6n2_PGF9CADF4B98E74C1993987A4D23E3164C>%"          ;
;           )                                                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun MFiles-field-generator (lst / p)
  (setq p (MFiles-ID-list))
  (setq lst
    (mapcar
      '(lambda (n / i x k v s)
        (setq
          n (SliceNumber (vl-princ-to-string n))
          i (1+ (cdr n))
          n (car n)
          x (nth n p)
          k (car x)
          v (cdr x)
        )
        (if x
          (progn
            (if (vl-string-search "(s)" k)
              (setq
                k (strcat k " #" (itoa i))
                v (strcat v "n"  (itoa i))
              )
            )
            (cons k v)
          )
        )
      )
      lst
    )
  )
  (if (not (member nil lst))
    (cons
      (lst2str (mapcar 'car lst) " > ")
      (strcat "%<\\M-Files " (lst2str (mapcar 'cdr lst) "_") ">%")
    )
  )
)