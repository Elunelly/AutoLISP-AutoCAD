
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  LM:ACI->True  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] LM:ACI->True []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts an ACI color into a True color.                                                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:ACI->True) have 1 argument(s) :                                                                                                  ;
;   --•  c                      > an integer between 0 and 256 corresponding to the ACI color                                                       ;
;     (type c) = 'INT                           | Ex. : 0, 112, 179, 7, 255, ...                                                                    ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:RGB->True                                 | v1.4.0 - 19/06/2014 (LeeMac)                                                   ;
;   --•  "BaAri" ---> LM:ACI->RGB                                  | v1.4.0 - 19/06/2014 (LeeMac)                                                   ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:ACI->True) returns the True color code, as integer                                                                             ;
;     Ex. : (LM:ACI->True 179) returns 2500172                                                                                                      ;
;           (LM:ACI->True 0) returns 0                                                                                                              ;
;           (LM:ACI->True 255) returns 16777215                                                                                                     ;
;           (LM:ACI->True 256) returns 0                                                                                                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:ACI->True ( c / o r )
  (apply 'LM:RGB->True (LM:ACI->RGB c))
)