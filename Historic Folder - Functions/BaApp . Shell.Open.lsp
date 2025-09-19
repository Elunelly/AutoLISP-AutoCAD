
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  Shell:Open  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] Shell:Open []-----------------------[]                                         ;
;--- Date of creation       > 12/12/2006                                                                                                            ;
;--- Last modification date > 12/12/2006                                                                                                            ;
;--- Author                 > Patrick_35                                                                                                            ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Use the Shell.Application to open a specified file with the default application depending on the extension.                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Shell:Open) have 1 argument(s) :                                                                                                    ;
;   --•  filepath               > is the full path of file to be search in Excel application                                                        ;
;     (type filepath) = 'STR                    | Ex. : "C:\\Users\\lphilip\\Documents\\Drawing1.dwg", ...                                          ;
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
;   The function (Shell:Open) returns a numeric value if the object is successfully released after the file is opened, otherwise nil.               ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Shell:Open (filepath / shell)
  (if filepath
    (progn
      (setq shell (vlax-create-object "Shell.Application"))
      (vlax-invoke shell 'Open filename)
      (vlax-release-object shell)
    )
  )
)