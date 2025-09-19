
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  set-dyn-VisibilityValue  }--                                                       | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] set-dyn-VisibilityValue []-----------------------[]                                  ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Sets the visibility parameter of a Dynamic Block Reference (if present) to a specific value (if allowed).                                       ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-dyn-VisibilityValue) have 2 argument(s) :                                                                                       ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have visibility parameter                             ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;   --•  val                    > correspond to the new value for the visibility parameter                                                          ;
;     (type val) = 'STR                         | Ex. : "TPC Ø160", "EXE", ...                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlObj" ---> get-dyn-AllowedValues                         | v3.0.0 - 30/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> get-dyn-VisibilityName                        | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlObj" ---> set-dyn-value                                 | v2.1.0 - 30/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-dyn-VisibilityValue) returns the visibility parameter value if successful, nil otherwise.                                     ;
;     Ex. : (set-dyn-VisibilityValue (car (entsel)) "EXE") returns "EXE" if the value is allowed, nil otherwise                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Affecting the function's name (get-dyn-value) and (get-dyn-VisibilityName) from my own library                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-dyn-VisibilityValue (blk val / vis)
  (if
    (and
      (setq vis (get-dyn-VisibilityName blk))
      (member (strcase val) (mapcar 'strcase (get-dyn-AllowedValues blk vis)))
    )
    (set-dyn-value blk vis val)
  )
)