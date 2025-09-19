
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  LM:getOwner  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] LM:getOwner []-----------------------[]                                        ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "VlDrw"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   A wrapper for the ObjectIDtoObject method & OwnerID property to enable compatibility with 32-bit & 64-bit systems                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:getOwner) have 1 argument(s) :                                                                                                   ;
;   --•  obj                    > object to get its owner (model space, paper space, drawing, etc...)                                               ;
;     (type obj) = 'VLA-OBJECT                  | Ex. : (vlax-ename->VLA-Object (car (entsel))), ...                                                ;
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
;   The function (LM:getOwner) returns the owner's VLA-Object if successful, nil otherwise                                                          ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:getOwner ( obj )
  (eval
    (list 'defun 'LM:getowner '( obj )
      (if (vlax-method-applicable-p obj 'ownerID32)
        (list 'vla-objectIDtoobject32 (LM:acdoc) '(vla-get-ownerID32 obj))
        (list 'vla-objectIDtoobject   (LM:acdoc) '(vla-get-ownerID   obj))
      )
    )
  )
  (LM:getowner obj)
)