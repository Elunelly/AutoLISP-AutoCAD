
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                      --{  LM:outputtext:updatefield  }--                                                      | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                 []-----------------------[] LM:outputtext:updatefield []-----------------------[]                                 ;
;--- Date of creation       > 16/01/2016                                                                                                            ;
;--- Last modification date > 16/01/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:outputtext:updatefield) have 1 argument(s) :                                                                                     ;
;   --•  ent                    > is the entity name of text entity (= TEXT, MTEXT, ATTRIB, ...)                                                    ;
;     (type ent) = 'ENAME                       | Ex. : (car (entsel)), ...                                                                         ;
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
;   The function (LM:outputtext:updatefield) returns T                                                                                              ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:outputtext:updatefield ( ent / cmd rtn )
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq rtn (vl-cmdf "_.UPDATEFIELD" ent ""))
  (setvar "CMDECHO" cmd)
  rtn
)