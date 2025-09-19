
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                          --{  LM:SortentsTable  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                     []-----------------------[] LM:SortentsTable []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "VlDic"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the Sortents Table object (VLA-Object) or add one if not existent.                                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:SortentsTable) have 1 argument(s) :                                                                                              ;
;   --•  obj                    > Block container object                                                                                            ;
;     (type obj) = 'VLA-OBJECT                  | Ex. : (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))), (LM:GetOwner obj), ...;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaErr" ---> LM:CatchApply                                 | v1.2.0 - 09/02/2015 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:SortentsTable) retrieves the Sortents Tables object (Controls object sorting in support of draw order for several operations). ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.2.0   |   LeeMac's modifications                                                                                                         | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:SortentsTable ( obj / dic )
  (cond
    ( (LM:CatchApply 'vla-item (list (setq dic (vla-getextensiondictionary obj)) "acad_sortents")))
    ( (LM:CatchApply 'vla-addobject  (list dic "acad_sortents" "AcDbSortentsTable")))
  )
)