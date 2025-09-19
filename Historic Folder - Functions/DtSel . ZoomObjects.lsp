
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  ZoomObjects  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] ZoomObjects []-----------------------[]                                        ;
;--- Date of creation       > 06/07/2022                                                                                                            ;
;--- Last modification date > 06/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Zooms in on all objects in the selection set                                                                                                    ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ZoomObjects) have 1 argument(s) :                                                                                                   ;
;   --•  jsel                   > corresponds to the selection set you want to zoom in on                                                           ;
;     (type jsel) = 'PICKSET                    | Ex. : (ssget), (ssget "_I"), ...                                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "DtSel" ---> LM:ssBoundingBox                              | v1.2.0 - 04/01/2017 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (ZoomObjects) returns the selection set                                                                                            ;
;     Ex. : (ZoomObjects (ssget)) returns <Selection set: 47> if selection set is not null, nil otherwise                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun ZoomObjects (jsel / ptlst)
  (and
    jsel
    (setq ptlst (LM:ssBoundingBox jsel))
    (vla-ZoomWindow
      (vlax-get-acad-object)
      (vlax-3D-point (car ptlst))
      (vlax-3D-point (last ptlst))
    )
  )
  jsel
)