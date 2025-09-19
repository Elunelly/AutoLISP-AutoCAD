
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  make-layer-temp  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] make-layer-temp []-----------------------[]                                      ;
;--- Date of creation       > 29/04/2024                                                                                                            ;
;--- Last modification date > 29/04/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlCol"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (make-layer-temp) have 0 argument(s) :                                                                                               ;
;   --•  ...                    >                                                                                                                   ;
;     (type ...) = '...                         | Ex. :                                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlPrp" ---> props                                         | v1.0.0 - 29/04/2024 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (make-layer-temp) returns T if successful, nil otherwise.                                                                          ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun make-layer-temp ( plst / props layers layer)
  (defun props (property-name default / value)
    (if (setq value (cdr (assoc property-name plst)))
      (cond ((vlax-put layer property-name value)) (T))
      (cond ((vlax-put layer property-name default)) (T))
    )
  )
  (defun delete-temp (/ *error* cme nmt lst)
    (defun *error* (msg)
      (setvar "CMDECHO" cme)
      (setvar "NOMUTT" nmt)
      (princ)
    )
    (setq
      cme (getvar "CMDECHO")
      nmt (getvar "NOMUTT")
    )
    (if (= "$temp$" (getvar "CLAYER"))
      (setvar "CLAYER" "0")
    )
    (setvar "CMDECHO" 0)
    (setvar "NOMUTT" 1)
    (command "_-LAYDEL" "_Name" "$temp$" "" "_Yes")
    (setvar "CMDECHO" cme)
    (setvar "NOMUTT" nmt)
    (and
      (setq lst (vlax-ldata-get "URBASOLAR" "$temp$"))
      (mapcar '(lambda (x) (vl-catch-all-apply 'vla-delete (list (ConvName x 'VLA-OBJECT)))) lst)
      (vlax-ldata-delete "URBASOLAR" "$temp$")
    )
  )

  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (cond
    ( (setq layer (LM:CatchApply 'vla-item (list layers "$temp$"))))
    ( (setq layer (vla-add layers "$temp$")))
  )
  (and
    (props "TrueColor" (vla-get-TrueColor (vla-item layers "0")))
    (props "LineType" "Continuous")
    (cond ((vla-put-Freeze layer :vlax-false)) (T))
    (cond ((vla-put-LayerOn layer :vlax-true)) (T))
    (cond ((vla-put-Lock layer :vlax-false)) (T))
    (setvar "CLAYER" "$temp$")
  )
)