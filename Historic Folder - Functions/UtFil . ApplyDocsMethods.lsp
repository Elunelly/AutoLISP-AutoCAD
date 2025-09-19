
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                          --{  ApplyDocsMethods  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] ApplyDocsMethods []-----------------------[]                                      ;
;--- Date of creation       > 16/01/2008                                                                                                            ;
;--- Last modification date > 05/07/2022                                                                                                            ;
;--- Author                 > Glenn_White/Luna                                                                                                      ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtFil"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Close a list of documents without saving or not, depending on the specified arguments. You can close all opened documents except the active one ;
;   or close some specified documents only or all without any exceptions. You can decide to save them before closing or not.                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ApplyDocsMethods) have 2 argument(s) :                                                                                              ;
;   --•  docs                   > a list of drawings's name that you want to close                                                                  ;
;     (type docs) = 'LST or SYM                 | Ex. : '("Drawing1.dwg" "Drawing3.dwg"), nil (= All opened drawings), T (= All but active), ...    ;
;   --•  lst                    > an association list of methods you want to apply on documents with the key corresponding to the method and the    ;
;                                 value corresponding to the list of arguments needed for the method. The order of listed methods is important      ;
;     (type lst) = 'LST                         | Ex. : (list (cons 'Close :vlax-False)), (list (cons 'Save nil)), ...                              ;
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
;   The function (ApplyDocsMethods) returns a doted list with the key corresponding to the number of drawings found in the AutoCAD application and  ;
;   the value corresponding to the list of each drawing where a method worked correctly (first element is the drawing name and the second one is    ;
;   list of working method)                                                                                                                         ;
;     Ex. : (ApplyDocsMethods T '((Close . :vlax-false))) returns                                                                                   ;
;       (3 (("Drawing1.dwg" ((Close . :vlax-false))) ("Drawing2.dwg" ((Close . :vlax-false))) ("Drawing3.dwg" ((Close . :vlax-false)))))            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesign the whole program to apply any method on specific documents and not only to close all drawing but Active without save | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun ApplyDocsMethods (docs lst / col i name tmp)
  (setq
    col (vla-get-Documents (vlax-get-acad-object))
    i (vla-get-Count col)
    docs (if (listp docs) (mapcar 'strcase docs) docs)
  )
  (vlax-for Item col
    (and
      (setq name (vla-get-Name Item))
      (or
        (null docs)
        (and (listp docs) (member (strcase name) docs))
        (and (vl-symbolp docs) (= :vlax-False (vla-get-Active Item)))
      )
      (setq lst
        (vl-remove-if-not
          '(lambda (m)
            (and
              (vlax-method-applicable-p Item (car m))
              (not
                (vl-catch-all-error-p
                  (vl-catch-all-apply
                    'vlax-invoke-method
                    (append
                      (list Item (car m))
                      (vl-remove nil (list (cdr m)))
                    )
                  )
                )
              )
            )
          )
          lst
        )
      )
      (setq tmp (cons (cons name lst) tmp))
    )
  )
  (cons i tmp)
)