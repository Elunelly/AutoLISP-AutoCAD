
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  LM:RGB->True  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] LM:RGB->True []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a RGB color into a True color.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:RGB->True) have 3 argument(s) :                                                                                                  ;
;   --•  R                      > an integer between 0 and 255 corresponding to the Red value of RGB color                                          ;
;     (type R) = 'INT                           | Ex. : 0, 112, 42, 69, 255, ...                                                                    ;
;   --•  G                      > an integer between 0 and 255 corresponding to the Green value of RGB color                                        ;
;     (type G) = 'INT                           | Ex. : 0, 112, 42, 69, 255, ...                                                                    ;
;   --•  B                      > an integer between 0 and 255 corresponding to the Blue value of RGB color                                         ;
;     (type B) = 'INT                           | Ex. : 0, 112, 42, 69, 255, ...                                                                    ;
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
;   The function (LM:RGB->True) returns the OLE color code, as integer                                                                              ;
;     Ex. : (LM:RGB->True 23 42 69) returns 1518149                                                                                                 ;
;           (LM:RGB->True 0 0 0) returns 0                                                                                                          ;
;           (LM:RGB->True 255 255 255) returns 16777215                                                                                             ;
;           (LM:RGB->True 255 0 0) returns 16711680                                                                                                 ;
;           (LM:RGB->True 0 255 0) returns 65280                                                                                                    ;
;           (LM:RGB->True 0 0 255) returns 255                                                                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:RGB->True ( R G B )
  (logior (lsh (fix R) 16) (lsh (fix G) 8) (fix B))
)