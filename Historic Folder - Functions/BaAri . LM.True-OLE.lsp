
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  LM:True->OLE  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] LM:True->OLE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a True color into an OLE color.                                                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:True->OLE) have 1 argument(s) :                                                                                                  ;
;   --•  c                      > an integer between 0 and 16777215 corresponding to the True color                                                 ;
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
;   The function (LM:True->OLE) returns the OLE color code, as integer                                                                              ;
;     Ex. : (LM:True->OLE 4532759) returns 4532759                                                                                                  ;
;           (LM:True->OLE 0) returns 0                                                                                                              ;
;           (LM:True->OLE 16777215) returns 16777215                                                                                                ;
;           (LM:True->OLE 16777216) returns 0                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:True->OLE ( c )
  (apply 'logior
    (mapcar
      '(lambda ( x ) (lsh (lsh (lsh (fix c) x) -24) (- x 8)))
      '(08 16 24)
    )
  )
)