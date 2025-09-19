
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:ACI->RGB  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:ACI->RGB []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts an ACI color into a RGB color.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:ACI->RGB) have 1 argument(s) :                                                                                                   ;
;   --•  c                      > an integer between 0 and 256 corresponding to the ACI color (0 and 256 both returns '(0 0 0) )                    ;
;     (type c) = 'INT                           | Ex. : 0, 112, 179, 42, 255, ...                                                                   ;
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
;   The function (LM:ACI->RGB) returns the RGB color code, as list                                                                                  ;
;     Ex. : (LM:ACI->RGB 179) returns (38 38 76)                                                                                                    ;
;           (LM:RGB->ACI 0) returns (0 0 0)                                                                                                         ;
;           (LM:RGB->ACI 255) returns (255 255 255)                                                                                                 ;
;           (LM:RGB->ACI 1) returns (255 0 0)                                                                                                       ;
;           (LM:RGB->ACI 3) returns (0 255 0)                                                                                                       ;
;           (LM:RGB->ACI 5) returns (0 0 255)                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.4.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:ACI->RGB ( c / o r )
  (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda ( )
            (vla-put-colorindex o c)
            (list (vla-get-red o) (vla-get-green o) (vla-get-blue o))
           )
        )
      )
      (vlax-release-object o)
      (if (vl-catch-all-error-p r)
        (prompt (strcat "\nError: " (vl-catch-all-error-message r)))
        r
      )
    )
  )
)