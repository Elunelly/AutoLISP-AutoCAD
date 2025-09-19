
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  LM:CatchApply  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] LM:CatchApply []-----------------------[]                                       ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "BaErr"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Applies a function to a list of parameters and catches any exceptions.                                                                          ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:CatchApply) have 2 argument(s) :                                                                                                 ;
;   --•  fun                    > is the quoted function you want to use as first argument for (vl-catch-all-apply)                                 ;
;     (type fun) = 'SYM                         | Ex. : 'ssget, 'enget, 'vlax-put, 'getpropertyvalue, ...                                           ;
;   --•  prm                    > is the arguments needed for the quoted function, as second argument of (vl-catch-all-apply)                       ;
;     (type prm) = 'LIST or ANY                 | Ex. : (list (vlax-ename->VLA-Object (car (entsel))) 'Layer "0"), "_X", ...                        ;
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
;   The function (LM:CatchApply) returns the result of (vl-catch-all-apply) is no error detected, nil otherwise.                                    ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:CatchApply ( fun prm / rtn )
  (if (not (vl-catch-all-error-p (setq rtn (vl-catch-all-apply fun prm))))
    rtn
  )
)