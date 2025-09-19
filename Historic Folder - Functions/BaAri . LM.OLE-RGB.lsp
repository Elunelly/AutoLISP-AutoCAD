
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:OLE->RGB  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:OLE->RGB []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts an OLE color into a RGB color.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:OLE->RGB) have 1 argument(s) :                                                                                                   ;
;   --•  c                      > an integer between 0 and 16777215 corresponding to the OLE color                                                  ;
;     (type c) = 'INT                           | Ex. : 0, 112, 156651, 4532759, 255, ...                                                           ;
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
;   The function (LM:OLE->RGB) returns the RGB color code, as list                                                                                  ;
;     Ex. : (LM:OLE->RGB 4532759) returns (23 42 69)                                                                                                ;
;           (LM:RGB->OLE 0) returns (0 0 0)                                                                                                         ;
;           (LM:RGB->OLE 16777215) returns (255 255 255)                                                                                            ;
;           (LM:RGB->OLE 16777216) returns (0 0 0)                                                                                                  ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:OLE->RGB ( c )
  (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)