
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  delenv  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] delenv []-----------------------[]                                           ;
;--- Date of creation       > 13/09/2012                                                                                                            ;
;--- Last modification date > 13/09/2012                                                                                                            ;
;--- Author                 > Patrick_35                                                                                                            ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSyt"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Delete an environment variable from the registry. Be careful with this function !                                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (delenv) have 1 argument(s) :                                                                                                        ;
;   --•  env                    > name of the environment variable                                                                                  ;
;     (type env) = 'STR                         | Ex. : "LONGCUMUL_Settings", ...                                                                   ;
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
;   The function (delenv) returns T if successful, nil otherwise.                                                                                   ;
;     Ex. : (delenv "QLS_Settings") returns T if the environment variable is found, nil otherwise                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun delenv (env / key)
  (setq key (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\FixedProfile\\General"))
  (if (vl-registry-read key env)
    (vl-registry-delete key env)
  )
)