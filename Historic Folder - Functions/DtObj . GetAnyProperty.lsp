
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  GetAnyProperty  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] GetAnyProperty []-----------------------[]                                      ;
;--- Date of creation       > 30/12/2021                                                                                                            ;
;--- Last modification date > 05/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Gets any property (DXF, ActiveX or Visual) of an entity by specifying the property name. It's possible to assign a filter (wcmatch) for the     ;
;   type of entity to modify, depending on the property type ("*" to not apply any filter). Returns a pair list with the property name as key.      ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (GetAnyProperty) have 4 argument(s) :                                                                                                ;
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
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaLst" ---> get-DXF-value                                 | v1.1.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> SliceNumber                                   | v2.0.0 - 13/01/2022 (Luna)                                                    ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlPrp" ---> getName                                       | v1.0.0 - 30/12/2021 (Luna)                                                    ;
;   --•  "VlPrp" ---> getProperty                                   | v1.0.0 - 30/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaLst" ---> loop-a-list-properties                        | v2.0.0 - 28/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (GetAnyProperty) returns a pair list with the property name as key and its value, nil otherwise.                                   ;
;     Ex. : (GetAnyProperty (car (entsel "\nSélection d'une ligne :")) "LINE" 48) returns (48 . 10.0) if the property was found, nil otherwise      ;
;           (GetAnyProperty (car (entsel)) "*line" 'EntityTransparency) returns ('EntityTransparency . 80) if the property was found, or nil        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Suppression of the 'flag' argument and set its value based on the type value of 'key' argument, adds the (get-DXF-value),      | ;
; |            |   (SliceNumber) and (ConvName) functions in the list of dependent's functions and add the possibility to get the N'th DXF code   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun GetAnyProperty (name search key / getName getProperty value)
  (defun getName (name flag)
    (cond
      ( (= flag 0) (cdr (assoc 0 (entget name))))
      ( (= flag 1) (vla-get-ObjectName (vlax-ename->vla-object name)))
      ( (= flag 2) (vla-get-ObjectName (vlax-ename->vla-object name)))
    )
  )
  
  (defun getProperty (name flag key / value)
    (cond
      ( (= flag 0)
        (setq key (SliceNumber (vl-princ-to-string key)))
        (if (setq value (get-DXF-value (entget name) (car key) (cdr key)))
          (cons (car key) value)
        )
      )
      ( (= flag 1)
        (if (listp key)
          (if (null (vl-catch-all-error-p (setq value (vl-catch-all-apply 'getpropertyvalue (append (list name) key)))))
            (cons key value)
          )
          (if (null (vl-catch-all-error-p (setq value (vl-catch-all-apply 'getpropertyvalue (list name key)))))
            (cons key value)
          )
        )
      )
      ( (= flag 2)
        (if (null (vl-catch-all-error-p (setq value (vl-catch-all-apply 'vlax-get (list (vlax-ename->vla-object name) key)))))
          (cons key value)
        )
      )
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
    (setq value (getProperty name flag key))
  )
  value
)