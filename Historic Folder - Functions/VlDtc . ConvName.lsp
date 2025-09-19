
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  ConvName  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] ConvName []-----------------------[]                                         ;
;--- Date of creation       > 04/01/2022                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlDtc"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to convert an entity name to VLA-Object and vice versa by choosing the output format. It helps when you don't know the type of output    ;
;   you have, but you know what type of input a function needs.                                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ConvName) have 2 argument(s) :                                                                                                      ;
;   --•  name                   > is the entity name or the VLA-Object you want to convert if needed                                                ;
;     (type name) = 'ENAME or VLA-OBJECT        | Ex. : <Entity name: 273a55ebd40>, #<VLA-OBJECT IAcadLWPolyline 0000027c1aab5868>, ...             ;
;   --•  flag                   > is the type of output you want between 'ENAME or 'VLA-OBJECT                                                      ;
;     (type flag) = 'SYM                        | Ex. : 'ENAME or 'VLA-OBJECT                                                                       ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "DtObj" ---> GetAnyProperty                                | v2.0.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "DtObj" ---> SetAnyProperty                                | v2.0.0 - 05/02/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-att-value                                 | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-AllowedValues                         | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-list                                  | v3.0.1 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-value                                 | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-VisibilityName                        | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-att-list                                  | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-att-value                                 | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-FlipState                             | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-list                                  | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-value                                 | v2.1.0 - 30/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (ConvName) returns the entity name or VLA-Object, depending of the 'flag' value.                                                   ;
;     Ex. : (ConvName (car (entsel)) 'ENAME) returns <Entity name: 273a55ebd40>                                                                     ;
;           (ConvName (car (entsel)) 'VLA-OBJECT) returns #<VLA-OBJECT IAcadLWPolyline 0000027c1aab5868>                                            ;
;           (ConvName (vlax-ename->vla-object (car (entsel))) 'ENAME) returns <Entity name: 273a55ebd40>                                            ;
;           (ConvName (vlax-ename->vla-object (car (entsel))) 'VLA-OBJECT) returns #<VLA-OBJECT IAcadLWPolyline 0000027c1aab5868>                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun ConvName (name flag / tp)
  (if (= (setq tp (type name)) flag)
    name
    ((eval (read (strcat "vlax-" (vl-symbol-name tp) "->" (vl-symbol-name flag)))) name)
  )
)