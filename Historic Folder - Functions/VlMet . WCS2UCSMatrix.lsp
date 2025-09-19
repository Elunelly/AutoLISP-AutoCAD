
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  WCS2UCSMatrix  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] WCS2UCSMatrix []-----------------------[]                                       ;
;--- Date of creation       > 21/01/2007                                                                                                            ;
;--- Last modification date > 21/01/2007                                                                                                            ;
;--- Author                 > Douglas C. Broad, Jr.                                                                                                 ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlMet"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Can be used with (vla-TransformBy) to transform object from WCS to the UCS.                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (WCS2UCSMatrix) have 0 argument(s) :                                                                                                 ;
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
;   The function (WCS2UCSMatrix) returns the converted matrix from WCS to UCS                                                                       ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun WCS2UCSMatrix ()
  (vlax-tmatrix
    (append
      (mapcar
        '(lambda (vector origin) (append (trans vector 0 1 t) (list origin)))
        (list '(1 0 0) '(0 1 0) '(0 0 1))
        (trans '(0 0 0) 1 0)
      )
      (list '(0 0 0 1))
    )
  )
)