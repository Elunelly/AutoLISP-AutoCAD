
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                        --{  make-a-list-properties  }--                                                       | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                   []-----------------------[] make-a-list-properties []-----------------------[]                                  ;
;--- Date of creation       > 06/01/2022                                                                                                            ;
;--- Last modification date > 04/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   When used in a loop function and by defining the 'lst' variable with the result of this function, it creates an association list with different ;
;   levels. The main idea is to set value (like a counter, the object's length, etc.) and creates a new list of keys if not existant or applies a   ;
;   function (like '+, '-, 'strcat) between the old and the new value with the same list of keys. For that, it will considered for each key a new   ;
;   level of association list. In fine, each similar objects by the value of their properties name, will be match together.                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (make-a-list-properties) have 5 argument(s) :                                                                                        ;
;   --•  lst                    > corresponds to the existing list you want to build withing a loop. The common use will be, for example, see below ;
;                               (setq lst (make-a-list-properties lst ...))                                                                         ;
;     (type lst) = 'LST                         | Ex. : a variable name to build the list within a loop                                             ;
;   --•  k-lst                  > corresponds to the list of property value that will be used to determine the number of level of association list. ;
;                               Each property value will be used as the key for each association list.                                              ;
;     (type k-lst) = 'LST                       | Ex. : '("Layer1" "LWPOLYLINE"), '("BlockName2" "63,115,69" "23.42"), '("Layer0"), ...             ;
;   --•  value                  > corresponds to the new value for the list of keys. If the 'lst' already contain a value for that list of keys, it ;
;                               will be add to the old one (depending of 'fun' value), otherwise it will be add to the list with the list of keys   ;
;     (type value) = '...                       | Ex. : 1 (to count the entity units), (getpropertyvalue name "LENGTH"), "63,115,69", ...           ;
;   --•  fun                    > corresponds to the function that will be used to connect the old value with the new one. The most common value of ;
;                               'fun' is '+, to add the new value with the old one                                                                  ;
;     (type fun) = 'SYM                         | Ex. : '+, '-, 'strcat, ...                                                                        ;
;   --•  flag                   > corresponds to the position of 'value' in relation to (cdr search), the actual value in the association list      ;
;     (type flag) = 'SYM                        | Ex. : T means 'value' is before (cdr search), nil means 'value' is after (cdr search)             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> loop-a-list-properties                        | v2.0.0 - 28/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (make-a-list-properties) returns the new value of the list, with the new value substitute to the old one if existing, or add it to ;
;   the list.                                                                                                                                       ;
;     Ex. : (make-a-list-properties lst (list Layer ObjectName) 1 '+ nil) returns after the loop from a selection set                               ;
;             ( ("Layer1" ("LWPOLYLINE" . 13) ("CIRCLE" . 4) ("INSERT" . 1))                                                                        ;
;               ("Layer3" ("CIRCLE" . 1) ("LINE" . 14) ("LWPOLYLINE" . 42))                                                                         ;
;               ("Layer2" ("INSERT" . 69))                                                                                                          ;
;               ("Layer9" ("LINE" . 3) ("ARC" . 11))                                                                                                ;
;             )                                                                                                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding an argument to define the position of 'value' (cdr search) in the list between each other                               | ;
; |            |   (i.e (list value (cdr search)) or (list (cdr search) value)) to adapt the program to different function (like  (cons) and (-)) | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun make-a-list-properties (lst k-lst value fun flag / key search)
  (if (null (cdr k-lst))
    (if (setq search (assoc (setq key (car k-lst)) lst))
      (subst (cons key (apply fun (if flag (list value (cdr search)) (list (cdr search) value)))) search lst)
      (append lst (list (cons key value)))
    )
    (if (setq search (assoc (setq key (car k-lst)) lst))
      (subst (cons key (make-a-list-properties (cdr search) (cdr k-lst) value fun flag)) search lst)
      (append lst (list (cons key (make-a-list-properties (cdr search) (cdr k-lst) value fun flag))))
    )
  )
)