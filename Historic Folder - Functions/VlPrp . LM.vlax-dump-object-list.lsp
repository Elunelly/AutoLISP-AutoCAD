
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                      --{  LM:vlax-dump-object->list  }--                                                      | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                 []-----------------------[] LM:vlax-dump-object->list []-----------------------[]                                 ;
;--- Date of creation       > 15/12/2016                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   This is a very simple utility to enable the user to list the Visual LISP properties of a VLA-Object. It creates a list of doted pairs with each ;
;   key and value if the property is available for the specified object. It's using the property of the function (atoms-family) and the writing     ;
;   simplification of the (vlax-get) function because AutoLISP gives a function's name for each gettable property.                                  ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:vlax-dump-object->list) have 1 argument(s) :                                                                                     ;
;   --•  obj                    > corresponds to the name of the VLA-OBJECT you want to dump the properties's list                                  ;
;     (type obj) = 'VLA-OBJECT                  | Ex. : (vlax-ename->VLA-Object (car (entsel))), ...                                                ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> GetVlaProperties                              | v1.1.0 - 15/12/2016 (Luna)                                                    ;
;   --•  "BaSym" ---> GetVlaAtoms                                   | v1.1.0 - 15/12/2016 (Luna)                                                    ;
;   --•  "VlPrp" ---> GetObjectProperties                           | v1.1.0 - 15/12/2016 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtSel" ---> Select-Filter                                 | v3.2.0 - 27/04/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:vlax-dump-object->list) returns a list of doted pair lists constructed like (PropertyName . PropertyValue) for each property   ;
;   available for the specified object.                                                                                                             ;
;     Ex. : (LM:vlax-dump-object->list (vlax-ename->VLA-Object (car (entsel)))) returns                                                             ;
;           ( ("Application" . #<VLA-OBJECT IAcadApplication 00007ff7d0824e60>)                                                                     ;
;             ("AttachmentPoint" . 1)                                                                                                               ;
;             ("BackgroundFill" . 0)                                                                                                                ;
;             ("Color" . 256)                                                                                                                       ;
;             ("Database" . #<VLA-OBJECT IAcadDatabase 0000018b3a9fc8d8>)                                                                           ;
;             ("Document" . #<VLA-OBJECT IAcadDocument 0000018b0e6582b8>)                                                                           ;
;             ("DrawingDirection" . 5)                                                                                                              ;
;             ("EntityTransparency" . "DuCalque")                                                                                                   ;
;             ("Handle" . "239")                                                                                                                    ;
;             ("HasExtensionDictionary" . 0)                                                                                                        ;
;             ("Height" . 0.2)                                                                                                                      ;
;             ("Hyperlinks" . #<VLA-OBJECT IAcadHyperlinks 0000018b18638ed8>)                                                                       ;
;             ("InsertionPoint" . (15.5003 9.71424 0.0))                                                                                            ;
;             ("Layer" . "0")                                                                                                                       ;
;             ("LineSpacingDistance" . 0.333333)                                                                                                    ;
;             ("LineSpacingFactor" . 1.0)                                                                                                           ;
;             ("LineSpacingStyle" . 1)                                                                                                              ;
;             ("LineWeight" . -1)                                                                                                                   ;
;             ("Linetype" . "ByLayer")                                                                                                              ;
;             ("LinetypeScale" . 1.0)                                                                                                               ;
;             ("Material" . "ByLayer")                                                                                                              ;
;             ("Normal" . (0.0 0.0 1.0))                                                                                                            ;
;             ("ObjectID" . 43)                                                                                                                     ;
;             ("ObjectName" . "AcDbMText")                                                                                                          ;
;             ("OwnerID" . 44)                                                                                                                      ;
;             ("PlotStyleName" . "ByLayer")                                                                                                         ;
;             ("Rotation" . 0.0)                                                                                                                    ;
;             ("StyleName" . "Standard")                                                                                                            ;
;             ("TextString" . "test")                                                                                                               ;
;             ("TrueColor" . #<VLA-OBJECT IAcadAcCmColor 0000018b18637b50>)                                                                         ;
;             ("Visible" . -1)                                                                                                                      ;
;             ("Width" . 0.830267)                                                                                                                  ;
;           )                                                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Change the name of the function to have two differents version of this function (with two differents methods)                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   The "Coordinate" property is ignored due to the 'param' argument required to get/put any value + using of (cons)               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:vlax-dump-object->list (obj / prp GetObjectProperties GetVlaProperties GetVlaAtoms)
  (defun GetObjectProperties (obj)
    (vl-remove-if-not
      '(lambda ( prp )
        (and
          (vlax-property-available-p obj prp)
          (/= prp "Coordinate")
        )
       )
      (GetVlaProperties)
    )
  )
  
  (defun GetVlaProperties ()
    (eval
      (list 'defun 'GetVlaProperties '()
        (list 'quote
          (mapcar
            (function
              (lambda ( sym )
                (substr (vl-symbol-name sym) 9)
              )
            )
            (vl-remove-if-not
              (function
                (lambda ( sym )
                  (wcmatch (strcase (vl-symbol-name sym)) "VLA`-GET`-*")
                )
              )
              (GetVlaAtoms)
            )
          )
        )
      )
    )
    (GetVlaProperties)
  )
  
  (defun GetVlaAtoms ()
    (eval
      (list 'defun 'GetVlaAtoms '( )
        (list 'quote
          (vl-sort
            (vl-remove-if-not
              (function
                (lambda ( sym )
                  (wcmatch (vl-symbol-name sym) "vla`-*")
                )
              )
              (atoms-family 0)
            )
            (function
              (lambda ( a b )
                (<
                  (vl-symbol-name a)
                  (vl-symbol-name b)
                )
              )
            )
          )
        )
      )
    )
    (GetVlaAtoms)
  )
  
  (mapcar
    '(lambda (prp)
      (cons prp (vlax-get obj prp))
     )
    (GetObjectProperties obj)
  )
)