
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  vlax-dump-object->list  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] vlax-dump-object->list []-----------------------[]                                   ;
;--- Date of creation       > 11/07/2022                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   This is a very simple utility to enable the user to list the Visual LISP properties of a VLA-Object. It creates a list of doted pairs with each ;
;   key and value if the property is available for the specified object. It's using the LOGFILE of command line's historic and can retrieves the    ;
;   properties (key and value) but also the available methods that can be applied to this object.                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (vlax-dump-object->list) have 2 argument(s) :                                                                                        ;
;   --•  obj                    > corresponds to the ENAME or the VLA-OBJECT you want to dump the properties's list (and methods if wanted)         ;
;     (type obj) = 'VLA-OBJECT or 'ENAME        | Ex. : (vlax-ename->VLA-Object (car (entsel))), (entlast), ...                                     ;
;   --•  flag                   > corresponds to the flag of the function (vlax-dump-object) to list the methods available or not (assoc -3)        ;
;     (type flag) = 'SYM                        | Ex. : T (list available methods) or nil (no method's list)                                        ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaFun" ---> FunctionLog                                   | v1.0.0 - 11/07/2022 (Luna)                                                    ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlPrp" ---> get-Coordinate                                | v1.0.0 - 11/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (vlax-dump-object->list) returns a list of doted pair lists constructed like (PropertyName . PropertyValue) for each property      ;
;   available for the specified object. If 'flag' = T, returns also an association list with the code -3 for the available methods.                 ;                                                                                                            ;
;     Ex. : (vlax-dump-object->list (vlax-ename->VLA-Object (car (entsel))) T) returns                                                              ;
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
;             ( -3                                                                                                                                  ;
;               ("ArrayPolar" . 3)                                                                                                                  ;
;               ("ArrayRectangular" . 6)                                                                                                            ;
;               ("Copy" . 0)                                                                                                                        ;
;               ("Delete" . 0)                                                                                                                      ;
;               ("FieldCode" . 0)                                                                                                                   ;
;               ("GetBoundingBox" . 2)                                                                                                              ;
;               ("GetExtensionDictionary" . 0)                                                                                                      ;
;               ("GetXData" . 3)                                                                                                                    ;
;               ("Highlight" . 1)                                                                                                                   ;
;               ("IntersectWith" . 2)                                                                                                               ;
;               ("Mirror" . 2)                                                                                                                      ;
;               ("Mirror3D" . 3)                                                                                                                    ;
;               ("Move" . 2)                                                                                                                        ;
;               ("Rotate" . 2)                                                                                                                      ;
;               ("Rotate3D" . 3)                                                                                                                    ;
;               ("ScaleEntity" . 2)                                                                                                                 ;
;               ("SetXData" . 2)                                                                                                                    ;
;               ("TransformBy" . 1)                                                                                                                 ;
;               ("Update" . 0)                                                                                                                      ;
;             )                                                                                                                                     ;
;           )                                                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun vlax-dump-object->list (name flag / *error* get-Coordinate lst sep lng str m pos sch key value prp mth)
  (defun get-Coordinate (obj i / val)
    (if (not (vl-catch-all-error-p (setq val (vl-catch-all-apply 'vla-get-Coordinate (list obj (setq i (cond (i) (0))))))))
      (cons (vlax-safearray->list (vlax-variant-value val)) (get-Coordinate obj (1+ i)))
    )
  )
  (setq
    name (ConvName name 'VLA-Object)
    lst (vl-remove "" (FunctionLog (quote (vlax-dump-object name flag))))
    sep ";   "
    lng (strlen sep)
  )
  (while (and lst (not (wcmatch (car lst) (strcat sep "*"))))
    (setq lst (cdr lst))
  )
  (while (and lst (setq str (car lst)))
    (if (and flag (not (wcmatch (substr str lng) " *")))
      (setq m T)
      (and
        (setq pos (vl-string-search (setq sch (if m " (" " = ")) str))
        (setq str (substr str (1+ lng) (- pos lng)))
        (setq key (substr str 1 (vl-string-search " (" str)))
        (if (and (null m) (= key "Coordinate"))
          (setq value (get-Coordinate name nil))
          (cond
            ( m (setq value (atoi (substr (car lst) (+ 1 pos (strlen sch)) (- (vl-string-search ")" (car lst)) (+ 2 pos))))))
            ( (vl-catch-all-error-p (setq value (vl-catch-all-apply 'vlax-get (list name (read key))))) (setq value nil))
            (value)
          )
        )
        (if m
          (setq mth (cons (cons key value) mth))
          (setq prp (cons (cons key value) prp))
        )
      )
    )
    (setq lst (cdr lst))
  )
  (cond
    ( (and prp mth) (append (reverse prp) (list (cons -3 (reverse mth)))))
    ( prp (reverse prp))
    ( mth (mapcar 'car (reverse mth)))
  )
)