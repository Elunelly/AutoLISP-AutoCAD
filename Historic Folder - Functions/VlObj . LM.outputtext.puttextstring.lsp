
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                     --{  LM:outputtext:puttextstring  }--                                                     | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                []-----------------------[] LM:outputtext:puttextstring []-----------------------[]                                ;
;--- Date of creation       > 16/01/2016                                                                                                            ;
;--- Last modification date > 16/01/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:outputtext:puttextstring) have 2 argument(s) :                                                                                   ;
;   --•  obj                    > is the VLA-Object of text entity (= TEXT, MTEXT, ATTRIB, ...)                                                     ;
;     (type obj) = 'VLA-OBJECT                  | Ex. : (vlax-ename->vla-object (car (entsel))), ...                                                ;
;   --•  str                    > is the new value to be set for the text entity                                                                    ;
;     (type obj) = 'VLA-OBJECT                  | Ex. : (vlax-ename->vla-object (car (entsel))), ...                                                ;
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
;   The function (LM:outputtext:puttextstring) returns T                                                                                            ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:outputtext:puttextstring ( obj str )
  (vla-put-textstring obj "") ;; to clear any existing string
  (vla-put-textstring obj str)
  T
)