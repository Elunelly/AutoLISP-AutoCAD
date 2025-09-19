
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:RGB->ACI  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:RGB->ACI []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a RGB color into an ACI color.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:RGB->ACI) have 3 argument(s) :                                                                                                   ;
;   --•  R                      > an integer between 0 and 255 corresponding to the Red value of RGB color                                          ;
;     (type R) = 'INT                           | Ex. : 0, 112, 42, 69, 255, ...                                                                    ;
;   --•  G                      > an integer between 0 and 255 corresponding to the Green value of RGB color                                        ;
;     (type G) = 'INT                           | Ex. : 0, 112, 42, 69, 255, ...                                                                    ;
;   --•  B                      > an integer between 0 and 255 corresponding to the Blue value of RGB color                                         ;
;     (type B) = 'INT                           | Ex. : 0, 112, 42, 69, 255, ...                                                                    ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaApp" ---> LM:acapp                                      | v1.0.0 - 12/11/2010 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:RGB->ACI) returns the ACI color code, as integer                                                                               ;
;     Ex. : (LM:RGB->ACI 23 42 69) returns 179                                                                                                      ;
;           (LM:RGB->ACI 0 0 0) returns 18                                                                                                          ;
;           (LM:RGB->ACI 255 255 255) returns 7                                                                                                     ;
;           (LM:RGB->ACI 255 0 0) returns 1                                                                                                         ;
;           (LM:RGB->ACI 0 255 0) returns 3                                                                                                         ;
;           (LM:RGB->ACI 0 0 255) returns 5                                                                                                         ;
;           (LM:RGB->ACI 51 51 51) returns 250                                                                                                      ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:RGB->ACI ( R G B / c o )
  (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
    (progn
      (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o R G B) (vla-get-colorindex o))))
      (vlax-release-object o)
      (if (vl-catch-all-error-p c)
        (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
        c
      )
    )
  )
)