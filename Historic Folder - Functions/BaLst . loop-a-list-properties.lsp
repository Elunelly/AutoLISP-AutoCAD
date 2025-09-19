
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                        --{  loop-a-list-properties  }--                                                       | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                   []-----------------------[] loop-a-list-properties []-----------------------[]                                  ;
;--- Date of creation       > 07/01/2022                                                                                                            ;
;--- Last modification date > 28/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   To facilitate the automatic creation of a list of associations of different levels via (make-a-list-properties) from a selection set.           ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (loop-a-list-properties) have 4 argument(s) :                                                                                        ;
;   --•  jsel                   > is the selection set used to create the (make-a-list-properties)                                                  ;
;     (type jsel) = 'PICKSET                    | Ex. : (ssget), ...                                                                                ;
;   --•  PropertyList           > corresponds to the list of property name that will be used to determine the number of level of association list.  ;
;                               Each property name will inquire the property value of the objects with (GetAnyProperty) function                    ;
;     (type PropertyList) = 'LST                | Ex. : '(8 'ObjectName "COLOR"), '("LineTypeScale" 0), ...                                         ;
;   --•  value                  > corresponds to the new value for the list of keys. If the 'lst' already contain a value for that list of keys, it ;
;                               will be add to the old one (depending of 'fun' value), otherwise it will be add to the list with the list of keys   ;
;     (type value) = '...                       | Ex. : 1 (to count the entity units), (quote (getpropertyvalue name "LENGTH")), ...                ;
;   --•  fun                    > corresponds to the function that will be used to connect the old value with the new one. The most common value of ;
;                               'fun' is '+, to add the new value with the old one                                                                  ;
;     (type fun) = 'SYM                         | Ex. : '+, '-, 'strcat, ...                                                                        ;
;   --•  flag                   > corresponds to the position of 'value' in relation to (cdr search), the actual value in the association list      ;
;     (type flag) = 'SYM                        | Ex. : T means 'value' is before (cdr search), nil means 'value' is after (cdr search)             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> make-a-list-properties                        | v2.0.0 - 04/02/2022 (Luna)                                                    ;
;   --•  "DtObj" ---> GetAnyProperty                                | v1.0.0 - 30/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (loop-a-list-properties) returns the association list after the loop for each object in the selection set.                         ; 
;     Ex. : (loop-a-list-properties (ssget) (list 8 0) 1 '+) returns                                                                                ;
;             ( ("Layer1" ("LWPOLYLINE" . 13) ("CIRCLE" . 4) ("INSERT" . 1))                                                                        ;
;               ("Layer3" ("CIRCLE" . 1) ("LINE" . 14) ("LWPOLYLINE" . 42))                                                                         ;
;               ("Layer2" ("INSERT" . 69))                                                                                                          ;
;               ("Layer9" ("LINE" . 3) ("ARC" . 11))                                                                                                ;
;             )                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.1   |   New version 2.0.0 of (GetAnyProperty) is released                                                                              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   New version 2.0.0 of (make-a-list-properties) is released so adding the 'flag' argument                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun loop-a-list-properties (jsel PropertyList value fun flag / i name lst)
  (if jsel
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        lst
          (make-a-list-properties
            lst
            (mapcar
              '(lambda (pp / ppType)
                (cdr
                  (GetAnyProperty
                    name
                    "*"
                    pp
                  )
                )
               )
              PropertyList
            )
            (eval value)
            fun
            flag
          )
      )
    )
  )
)