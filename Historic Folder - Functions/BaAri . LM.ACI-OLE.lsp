
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:ACI->OLE  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:ACI->OLE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts an ACI color into a OLE color.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:ACI->OLE) have 1 argument(s) :                                                                                                   ;
;   --•  c                      > an integer between 0 and 256 corresponding to the ACI color (0 and 256 both returns 0)                            ;
;     (type c) = 'INT                           | Ex. : 0, 112, 179, 42, 255, ...                                                                   ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:RGB->OLE                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;   --•  "BaAri" ---> LM:ACI->RGB                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:ACI->OLE) returns the OLE color code, as integer                                                                               ;
;     Ex. : (LM:ACI->OLE 179) returns 4990502                                                                                                       ;
;           (LM:ACI->OLE 0) returns 0                                                                                                               ;
;           (LM:ACI->OLE 255) returns 16777215                                                                                                      ;
;           (LM:ACI->OLE 1) returns 255                                                                                                             ;
;           (LM:ACI->OLE 3) returns 65280                                                                                                           ;
;           (LM:ACI->OLE 5) returns 16711680                                                                                                        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:ACI->OLE ( c )
  (apply 'LM:RGB->OLE (LM:ACI->RGB c))
)