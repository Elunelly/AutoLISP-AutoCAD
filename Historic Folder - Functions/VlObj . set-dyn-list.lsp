
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  set-dyn-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] set-dyn-list []-----------------------[]                                        ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Sets dynamic properties with tags found in the association list to their associated values.                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-dyn-list) have 2 argument(s) :                                                                                                  ;
;   --•  blk                    > is the VLA-OBJECT of the block reference that is suppose to have dynamic properties                               ;
;     (type blk) = 'ENAME or 'VLA-OBJECT        | Ex. : #<VLA-OBJECT IAcadBlockReference 0000027c1aab5868>, (car (entsel)), ...                     ;
;   --•  lst                    > is the association list of ((<tag> . <value>) ...)                                                                ;
;     (type lst) = 'LST                         | Ex. : '(("Distance1" . 15) ("Visibility" "EXE")), ...                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlObj" ---> set-dyn-value                                 | v2.1.0 - 30/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-dyn-list) returns the list of successful dynamic properties (tag . value), nil otherwise.                                     ;
;     Ex. : (set-dyn-list (car (entsel)) '(("Distance1" . 23.86))) returns (("Distance1" . 23.86)) if successful, nil otherwise                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Re-design of the function and using the (set-dyn-value) function                                                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Adding the (ConvName) function to be able to handle entity name and VLA-Object at the same time                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun set-dyn-list (blk lst)
  (vl-remove
    nil
    (mapcar
      '(lambda (x)
        (if (set-dyn-value blk (car x) (cdr x))
          x
        )
      )
      lst
    )
  )
)