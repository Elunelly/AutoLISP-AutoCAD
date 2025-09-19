
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                         --{  vla-collection->list  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] vla-collection->list []-----------------------[]                                   ;
;--- Date of creation       > 05/03/2021                                                                                                            ;
;--- Last modification date > 19/08/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlCol"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Transforms a VLA collection into a list of pointed pairs with the name and ename of each object in the collection. It is an improved version of ;
;   the (flt_tbl) function because it can access more objects unlike the Symbol Tables (more limited) but without the search tool to filter the     ;
;   search on a pattern.                                                                                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (vla-collection->list) have 3 argument(s) :                                                                                          ;
;   --•  doc                    > corresponds to the VLA-Object of the search document. If nil, matches the ActiveDocument                          ;
;     (type doc) = 'VLA-OBJECT                  | Ex. : nil, (vlax-get-acad-object), (vla-get-ActiveDocument (vlax-get-acad-object)), ...           ;
;   --•  col                    > corresponds to the name of the collection with a quote (the name of a collection is always in the plural)         ;
;     (type col) = 'SUBR                        | Ex. : 'materials, 'linetypes, 'blocks, 'layers, ...                                               ;
;   --•  flag                   > allows to define if the name of the retrieved object corresponds to the ename or the VLA-object                   ;
;     (type flag) = 'BOOLEAN                    | Ex. : 0 for entity name and 1 for VLA-Object                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "VlPrp" ---> set-layout-name                               | v1.1.0 - 24/01/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (vla-collection->list) returns the list of pointed pairs each composed of the name of the object in string format and the ename or ;
;   VLA-Object of the object to access its DXF list or its properties.                                                                              ;
;     Ex. : (vla-collection->list nil 'linetypes 0) returns                                                                                         ;
;              ( ("ByBlock" . <Entity name: 3b058940>)                                                                                              ;
;                ("ByLayer" . <Entity name: 3b058950>)                                                                                              ;
;                ("Continuous" . <Entity name: 3b058960>)                                                                                           ;
;              )                                                                                                                                    ;
;           (vla-collection->list nil 'linetypes 1) returns                                                                                         ;
;              ( ("ByBlock" . #<VLA-OBJECT IAcadLineType 0000018b15e7ada8>)                                                                         ;
;                ("ByLayer" . #<VLA-OBJECT IAcadLineType 0000018b15e79f08>)                                                                         ;
;                ("Continuous" . #<VLA-OBJECT IAcadLineType 0000018b15c51728>)                                                                      ;
;              )                                                                                                                                    ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Add an argument to specify the VLA-Object corresponding to the search document to extend the functionnalities                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Simplification of writing the collection (ActiveDocument only) and addition of the flag variable to modify the return          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun vla-collection->list (doc col flag / lst item i)
  (if
    (null
      (vl-catch-all-error-p
        (setq
          i 0
          col (vl-catch-all-apply 'vlax-get (list (cond (doc) ((vla-get-activedocument (vlax-get-acad-object)))) col))
        )
      )
    )
    (vlax-for item col
      (setq lst
        (cons
          (cons
            (if (vlax-property-available-p item 'Name)
              (vla-get-name item)
              (strcat "Unnamed_" (itoa (setq i (1+ i))))
            )
            (cond
              ( (= flag 0) (vlax-vla-object->ename item))
              (item)
            )
          )
          lst
        )
      )
    )
  )
  (reverse lst)
)