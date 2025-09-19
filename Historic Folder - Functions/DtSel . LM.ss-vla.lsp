
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:ss->vla  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:ss->vla []-----------------------[]                                         ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a Selection Set to a list of VLA Objects.                                                                                              ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:ss->vla) have 1 argument(s) :                                                                                                    ;
;   --•  sel                    > selection set to be converted into list of VLA-Object                                                             ;
;     (type sel) = 'PICKSET                     | Ex. : (ssget), ...                                                                                ;
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
;   The function (LM:ss->vla) converts a selection set into a list of VLA-Objects.                                                                  ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:ss->vla ( sel / idx lst )
  (if (= 'pickset (type sel))
    (repeat (setq idx (sslength sel))
      (setq lst (cons (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))) lst))
    )
  )
)