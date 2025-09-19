
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  SetVarList  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] SetVarList []-----------------------[]                                        ;
;--- Date of creation       > 13/06/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Gets and/or sets a list of system variables values in a specific order and can store current value into AutoLISP variables.                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (SetVarList) have 1 argument(s) :                                                                                                    ;
;   --•  lst                    > correspond to the list of system variables, in a specific format (VarName VarSym Value). Each element of the list ;
;                               is a list of 3 elements:                                                                                            ;
;                                 VarName,  is the name of the system variable you want to get and/or modify, as a string                           ;
;                                 VarSym,   is the AutoLISP variable name you want to use to store the current value of VarName (nil if you don't   ;
;                                           want to store its value)                                                                                ;
;                                 Value,    is the new value you want to set VarName (nil if you don't want to set a new value)                     ;
;     (type lst) = 'LST                         | Ex. : '(("CTAB" nil "Layout1") ("PSLTSCALE" nil 0)), (list (list "CMDECHO" 'ce 0)), ...           ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (SetVarList) returns the same list as entered with some changes for VarSym and Value. If VarSym is not a symbol (type), a variable ;
;   name to store the current value, VarSym will be replaced by the current value of the system variable. If Value is not set, it will be replaced  ;
;   by the current value of the system variable. If Value is set but the new value generates an error, it will be replaced by the error entity. You ;
;   can use the function (vl-catch-all-error-message) to know precisely why it didn't work.                                                         ;
;     Ex. : (SetVarList (list (list "CTAB" nil nil))) returns (("CTAB" "Model" "Model"))                                                            ;
;           (SetVarList '(("ctab" nil "Layout1"))) returns (("CTAB" "Model" "Layout1"))                                                             ;
;           (SetVarList '(("CTAB" ct "Layout3") ("CmdEcho" ce nil))) returns (("CTAB" CT #<%catch-all-apply-error%>) ("CMDECHO" CE 1)) with !CT     ;
;           equals "Layout1" and !CE equals 1.                                                                                                      ;
;           (SetVarList '(("CTA" nil "Model"))) returns (("CTA" nil #<%catch-all-apply-error%>))
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Checking if VarSym is a symbol or not, adding the error-handling with (setvar) and adding (strcase) for system variable name   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun SetVarList (lst)
  (mapcar
    '(lambda (x / var sym val)
      (setq
        var (car x)
        sym (cadr x)
        val (caddr x)
      )
      (if (vl-symbolp sym)
        (set sym (getvar var))
        (setq sym (getvar var))
      )
      (if val
        (setq val (vl-catch-all-apply 'setvar (list var val)))
        (setq val (getvar var))
      )
      (list (strcase var) sym val)
     )
    lst
  )
)