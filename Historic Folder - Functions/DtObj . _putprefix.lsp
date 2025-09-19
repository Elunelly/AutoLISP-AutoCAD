
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  _putprefix  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] _putprefix []-----------------------[]                                        ;
;--- Date of creation       > 08/08/2018                                                                                                            ;
;--- Last modification date > 14/01/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to add a prefix for all objects that have the "TextPrefix" property in their VLA-OBJECT data. Can be applied for example to          ;
;   dimension objects.                                                                                                                              ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (_putprefix) have 2 argument(s) :                                                                                                    ;
;   --•  name                   > is the entity name to modify                                                                                      ;
;     (type name) = 'ENAME                      | Ex. : <Entity name: 1e596b02d50>, (car (entsel)), ...                                             ;
;   --•  txt                    > corresponds to the string we want to add as a prefix                                                              ;
;     (type txt) = 'STR                         | Ex. : "Nombre = ", "JD - ", ...                                                                   ;
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
;   The function (_putprefix) returns T if sucessful, nil otherwise.                                                                                ;
;     Ex. : (_putprefix (car (entsel)) "JD - ") returns T if successful, nil otherwise like the object selected doesn't have a 'TextPrefix property ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Updated argument and variable names and added Boolean return in case of success or failure                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun _putprefix (name txt)
  (if
    (and
      (= 'ename (type name))
      (vlax-property-available-p (setq name (vlax-ename->vla-object name)) 'textprefix)
      (vlax-write-enabled-p name)
    )
    (progn
      (vla-put-textprefix name txt)
      T
    )
  )
)