
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:OLE->ACI  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:OLE->ACI []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts an OLE color into a ACI color.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:OLE->ACI) have 1 argument(s) :                                                                                                   ;
;   --•  c                      > an integer between 0 and 16777215 corresponding to the OLE color                                                  ;
;     (type c) = 'INT                           | Ex. : 0, 112, 156651, 4532759, 255, ...                                                           ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:OLE->RGB                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;   --•  "BaAri" ---> LM:RGB->ACI                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:OLE->ACI) returns the ACI color code, as integer                                                                               ;
;     Ex. : (LM:OLE->ACI 4532759) returns 179                                                                                                       ;
;           (LM:OLE->ACI 0) returns 18                                                                                                              ;
;           (LM:OLE->ACI 16777215) returns 7                                                                                                        ;
;           (LM:OLE->ACI 16777216) returns 18                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:OLE->ACI ( c )
  (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)