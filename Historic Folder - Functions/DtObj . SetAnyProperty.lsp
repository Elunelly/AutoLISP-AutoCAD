
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  SetAnyProperty  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] SetAnyProperty []-----------------------[]                                      ;
;--- Date of creation       > 29/12/2021                                                                                                            ;
;--- Last modification date > 05/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Modifies any property (DXF, ActiveX or Visual) of an entity by specifying the property name and its value. It's possible to assign a filter     ;
;   (wcmatch) for the type of entity to modify, depending on the property type ("*" to not apply any filter). Returns T if the modification could   ;
;   be performed, nil otherwise.                                                                                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (SetAnyProperty) have 5 argument(s) :                                                                                                ;
;   --•  name                   > corresponds to the entity name of the object                                                                      ;
;     (type name) = 'ENAME                      | Ex. : <Entity name: 1b02f97ea80>                                                                  ;
;   --•  search                 > corresponds to the pattern to match against the entity type name. The pattern can contain the wild-card           ;
;                               pattern-matching characters shown in the table Wild-card characters. You can use commas in a pattern to enter more  ;
;                               than one pattern condition. It's not case-sensitive.                                                                ;
;     (type search) = 'STR                      | Ex. : "*" (= no filter), "*LINE", "*line", "*block*", ...                                         ;
;   --•  key                    > corresponds to the property name of the object you want to get. The type of value allowed depends directly on the ;
;                               type of properties you want to get (integers for (entget) DXF properties, strings or lists for (dumpallproperties)  ;
;                               ActiveX properties, quoted symbols for (vlax-dump-object) Visual properties). If you specified a real number, the   ;
;                               decimal part corresponds to the N'th occurence of the key (first position is 0).                                    ;
;     (type key) = 'INT or 'REAL                | Ex. : 8, 62, 45, 50, 67, 10.2, ...                                                                ;
;     (type key) = 'STR or 'LST                 | Ex. : "LineTypeScale", "Color", "Thickness", "Transparency", '("Vertice" 2 "Position/X"), ...     ;
;     (type key) = 'SYM                         | Ex. : 'LineTypeScale, 'TrueColor, 'Thickness, 'EntityTransparency, ...                            ;
;   --•  value                  > corresponds to the new value attributed to the property 'key'. The type of value depends of the type accepted or  ;
;                               supported by the property. You must check each type of value and the properties name with the functions (entget),   ;
;                               (dumpallproperties) and/or (vlax-dump-object).                                                                      ;
;     (type value) = ...                        | Ex. : 42.23, "Layer1", <Entity name: 1b02f97ea80>, #<VLA-OBJECT IAcadAcCmColor 1b0b12e2460>, ... 
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> get-DXF-value                                 | v1.1.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> SliceNumber                                   | v2.0.0 - 13/01/2022 (Luna)                                                    ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlPrp" ---> getName                                       | v1.0.0 - 29/12/2021 (Luna)                                                    ;
;   --•  "VlPrp" ---> setProperty                                   | v1.0.0 - 29/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (SetAnyProperty) returns T if the modification of the property was successful, nil otherwise.                                      ;
;     Ex. : (SetAnyProperty (car (entsel "\nSélection d'une ligne :")) "LINE" 48 10.0) returns T if the object selected is a "LINE", nil otherwise  ;
;           (SetAnyProperty (car (entsel)) "*line" 'EntityTransparency 80) returns T if the modification was successful, nil otherwise              ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Suppression of the 'flag' argument and set its value based on the type value of 'key' argument, adds the (get-DXF-value),      | ;
; |            |   (SliceNumber) and (ConvName) functions in the list of dependent's functions and add the possibility to get the N'th DXF code   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun SetAnyProperty (name search key value / getName setProperty)
  (defun getName (name flag)
    (cond
      ( (= flag 0) (cdr (assoc 0 (entget name))))
      ( (= flag 1) (vla-get-ObjectName (vlax-ename->vla-object name)))
      ( (= flag 2) (vla-get-ObjectName (vlax-ename->vla-object name)))
    )
  )
  
  (defun setProperty (name flag key value)
    (cond
      ((= flag 0)
        (vl-catch-all-apply
          'entmod
          (list
            (if
              (and
                (setq key (SliceNumber (vl-princ-to-string key)))
                (get-DXF-value (entget name) (car key) (cdr key))
              )
              (subst (cons (car key) value) (cons key (get-DXF-value (entget name) (car key) (cdr key))) (entget name))
              (append (entget name) (list (cons (car key) value)))
            )
          )
        )
      )
      ((= flag 1)
        (if (listp key)
          (vl-catch-all-apply 'setpropertyvalue (append (list name) key (list value)))
          (vl-catch-all-apply 'setpropertyvalue (list name key value))
        )
      )
      ((= flag 2) (vl-catch-all-apply 'vlax-put (list (vlax-ename->vla-object name) key value)))
    )
  )
  
  (and
    name
    (setq name (ConvName name 'ENAME))
    (setq flag
      (cond
        ( (vl-symbolp key) 2)
        ( (numberp key) 0)
        ( (or (listp key) (= 'STR (type key))) 1)
      )
    )
    (wcmatch (strcase (getName name flag)) (strcase search))
    (not (vl-catch-all-error-p (setProperty name flag key value)))
  )
)