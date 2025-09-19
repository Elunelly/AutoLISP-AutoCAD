
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                         --{  CrosshairColor->RGB  }--                                                         | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] CrosshairColor->RGB []-----------------------[]                                    ;
;--- Date of creation       > 12/08/2022                                                                                                            ;
;--- Last modification date > 12/08/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Get the color of the crosshair defined by the user in RGB color                                                                                 ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (CrosshairColor->RGB) have 0 argument(s) :                                                                                           ;
;   --•  ...                    >                                                                                                                   ;
;     (type ...) = '...                         | Ex. :                                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (CrosshairColor->RGB) returns [...]                                                                                                ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun CrosshairColor->RGB (/ cr oc )
  (setq
    cr (vla-get-ModelCrosshairColor (vla-get-Display (vla-get-Preferences (vlax-get-acad-object))))
    oc (vlax-variant-value (vlax-variant-change-type cr vlax-vbLong))
  )
  (LM:OLE->RGB oc)
)