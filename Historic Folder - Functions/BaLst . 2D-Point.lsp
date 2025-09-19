
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  2D-Point  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] 2D-Point []-----------------------[]                                          ;
;--- Date of creation       > 12/08/2022                                                                                                            ;
;--- Last modification date > 12/08/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Convert a list representing a 3D or 2D point into a 2D point. To not consider the coordinate Z in some calculations.                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (2D-Point) have 1 argument(s) :                                                                                                      ;
;   --•  pt                     > is the list representing a point. It's a 2 or 3 atom's length list                                                ;
;     (type pt) = 'LST                          | Ex. : (getpoint), (0.0 0.0 0.0), (12.15 -943.4568), ...                                           ;
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
;   The function (2D-Point) returns the point with only the X and Y coordinates                                                                     ;
;     Ex. : (2D-Point '(0.0 0.0 0.0)) returns (0.0 0.0)                                                                                             ;
;           (2D-Point '(12.15 -943.4568)) returns (12.15 -943.4568)                                                                                 ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun 2D-Point (pt / lg)
  (cond
    ( (or (not (listp pt)) (not (setq lg (vl-list-length pt)))) nil)
    ( (= 3 lg) (reverse (cdr (reverse pt))))
    ( (= 2 lg) pt)
  )
)