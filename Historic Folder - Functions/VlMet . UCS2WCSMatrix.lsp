
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  UCS2WCSMatrix  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] UCS2WCSMatrix []-----------------------[]                                       ;
;--- Date of creation       > 21/01/2007                                                                                                            ;
;--- Last modification date > 21/01/2007                                                                                                            ;
;--- Author                 > Douglas C. Broad, Jr.                                                                                                 ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlMet"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Can be used with (vla-TransformBy) to transform object from UCS to the WCS.                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (UCS2WCSMatrix) have 0 argument(s) :                                                                                                 ;
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
;   The function (UCS2WCSMatrix) returns the converted matrix from WCS to UCS                                                                       ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun UCS2WCSMatrix ()
  (vlax-tmatrix
    (append
      (mapcar
        '(lambda (vector origin) (append (trans vector 1 0 t) (list origin)))
        (list '(1 0 0) '(0 1 0) '(0 0 1))
        (trans '(0 0 0) 0 1)
      )
      (list '(0 0 0 1))
    )
  )
)