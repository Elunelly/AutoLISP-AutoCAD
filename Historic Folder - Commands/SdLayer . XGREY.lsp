
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                                --{  XGREY  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] XGREY []-----------------------[]                                           ;
;--- Date of creation       > 01/03/2024                                                                                                            ;
;--- Last modification date > 01/03/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "SdLayer"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Put the selected XREF object in grayscale. It modifies the layers properties to convert them into grayscale colors. Make sure to use            ;
;   VISRETAIN = 1 and XREFOVERRIDE = 1.                                                                                                             ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (...) returns [...]                                                                                                                 ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:XGREY (/ RGB->GREY )
  (defun RGB->GREY (RGB / c)
    (if
      (and
        (listp RGB)
        (= 3 (length RGB))
        (not (member nil (mapcar '(lambda (x) (<= 0 x 255)) RGB)))
        (setq c (LM:round (/ (apply '+ RGB) 3.)))
      )
      (list c c c)
    )
  )
  (setq
    jsel (ssget '((0 . "VIEWPORT")))
    layers (flt_tbl "LAYER" "*|*" nil)
    layers
      (mapcar
        '(lambda (l)
          ()
         )
        layers
      )
  )
)