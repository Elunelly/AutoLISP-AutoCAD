
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  set-att-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] set-att-list []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Sets attributes with tags found in the association list to their associated values.                                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-att-list) have 2 argument(s) :                                                                                                  ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have attributes                                       ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;   --•  lst                    > is the association list of ((<tag> . <value>) ...)                                                                ;
;     (type lst) = 'LST                         | Ex. : '(("Number" . "15") ("TITRE_1" "EXAMPLE OF NUMBERING")), ...                                ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlDtc" ---> ConvName                                      | v1.0.0 - 04/01/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-att-list) returns the list of successful attribute (tag .value), nil otherwise.                                               ;
;     Ex. : (set-att-list (car (entsel)) '(("C:LINE_TYPE" . "Continuous"))) returns (("C:LINE_TYPE" . "Continuous")) if successful, nil otherwise   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-att-list (blk lst / itm tmp)
  (foreach att (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
    (if (setq itm (assoc (vla-get-tagstring att) lst))
      (progn (vla-put-textstring att (cdr itm)) (setq tmp (cons itm tmp)))
    )
  )
  tmp
)