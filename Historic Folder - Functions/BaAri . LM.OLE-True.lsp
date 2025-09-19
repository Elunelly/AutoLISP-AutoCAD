
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  LM:OLE->True  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] LM:OLE->True []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts an OLE color into a True color.                                                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:OLE->True) have 1 argument(s) :                                                                                                  ;
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
;   The function (LM:OLE->True) returns the True color code, as integer                                                                             ;
;     Ex. : (LM:OLE->True 4532759) returns 1518149                                                                                                  ;
;           (LM:OLE->True 0) returns 0                                                                                                              ;
;           (LM:OLE->True 16777215) returns 16777215                                                                                                ;
;           (LM:OLE->True 16777216) returns 0                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:OLE->True ( c )
  (apply 'logior
    (mapcar
      '(lambda ( x ) (lsh (lsh (lsh (fix c) x) -24) (- x 8)))
      '(24 16 08)
    )
  )
)