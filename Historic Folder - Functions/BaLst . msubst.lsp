
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  msubst  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] msubst []-----------------------[]                                          ;
;--- Date of creation       > 06/05/2021                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allow the use of the (subst) function in multiple ways.                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (msubst) have 3 argument(s) :                                                                                                        ;
;   --•  lst                    > is the list of pointed pairs of new values to implement                                                           ;
;     (type lst) = 'LST                         | Ex. : '((10 0.0 0.0) (10 20.0 20.0) (10 30.0 10.0) (10 40.22 50.1), ...                           ;
;   --•  pos-lst                > corresponds to the list of positions for each previously specified pointed pair. This corresponds to the number   ;
;                               of times the key appears in the list. If nil, then the default position is 0                                        ;
;     (type pos-lst) = 'LST                     | Ex. : '(0 1 3 2), nil, '(5), ...                                                                  ;
;   --•  entlist                > corresponds to the list of pointed pairs (mainly DXF list)                                                        ;
;     (type entlist) = 'LST                     | Ex. : (entget (car (entsel))), '((-1 . <Entity name: 3b240de0>) ... (210 0.0 0.0 1.0)), ...       ;
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
;   The function (msubst) returns the list after the multiple modifications.                                                                        ;
;     Ex. : (msubst '((8 . "Layer1") (70 . 1) (10 20.0 -12.32)) '(0 0 1) (entget (car (entsel)))) returns                                           ;
;           ( (-1 . <Entity name: 18b0eeb0d40>)                                                                                                     ;
;             (0 . "LWPOLYLINE")                                                                                                                    ;
;             (330 . <Entity name: 18b0eeb29f0>)                                                                                                    ;
;             (5 . "234")                                                                                                                           ;
;             (100 . "AcDbEntity")                                                                                                                  ;
;             (67 . 0)                                                                                                                              ;
;             (410 . "Model")                                                                                                                       ;
;             (8 . "Layer1")                                                                                                                        ;
;             (100 . "AcDbPolyline")                                                                                                                ;
;             (90 . 2)                                                                                                                              ;
;             (70 . 1)                                                                                                                              ;
;             (43 . 0.0)                                                                                                                            ;
;             (38 . 0.0)                                                                                                                            ;
;             (39 . 0.0)                                                                                                                            ;
;             (10 13.2926 9.9261)                                                                                                                   ;
;             (40 . 0.0)                                                                                                                            ;
;             (41 . 0.0)                                                                                                                            ;
;             (42 . 0.0)                                                                                                                            ;
;             (91 . 0)                                                                                                                              ;
;             (10 20.0 -12.32)                                                                                                                      ;
;             (40 . 0.0)                                                                                                                            ;
;             (41 . 0.0)                                                                                                                            ;
;             (42 . 0.0)                                                                                                                            ;
;             (91 . 0)                                                                                                                              ;
;             (210 0.0 0.0 1.0)                                                                                                                     ;
;           )                                                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Possibility of extended use beyond DXF lists only. By separating the new values from the position, the keys can be of another  | ;
; |            |   type than 'REAL or 'INT                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun msubst (lst pos-lst entlist / new-item key pos tmp old-item)
  (foreach new-item lst
    (setq key (car new-item))
    (if pos-lst
      (setq
        pos (car pos-lst)
        pos-lst (cdr pos-lst)
      )
      (setq pos 0) 
    )
    (if
      (and
        (setq tmp (vl-remove-if-not '(lambda (x) (= (car x) key)) entlist))
        (setq old-item (nth pos tmp))
      )
      (setq entlist (subst new-item old-item entlist))
    )
  )
  entlist
)