
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  pt-member  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] pt-member []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 13/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaEqc"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Check if a specified point is member of a list of point or not, depending on 'fuzz' value.                                                      ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (pt-member) have 3 argument(s) :                                                                                                     ;
;   --•  pt                     > is the specified point you want to check                                                                          ;
;     (type pt) = 'LST                          | Ex. : (getpoint), '(0.0 0.0 0.0), '(0.0 0.0), '(1.13e12 0.98e12 0.0), ...                         ;
;   --•  pt-list                > corresponds to the list of points                                                                                 ;
;     (type pt-list) = 'LST                     | Ex. : (get-pt-list (car (entsel))), ...                                                           ;
;   --•  fuzz                   > is the precision for the (equal) function                                                                         ;
;     (type fuzz) = 'INT or 'REAL               | Ex. : 1, 1000, 0.001, ...                                                                         ;
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
;   The function (pt-member) returns T if the 'pt' is a 'pt-list' member, nil otherwise.                                                            ;
;     Ex. : (pt-member (getpoint "\nSelect point:") (get-pt-list (car (entsel))) 0.1) returns T if :True, nil otherwise                             ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Re-design of the function                                                                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun pt-member (pt pt-list fuzz)
  (setq pt (mapcar 'rtos pt))
  (car
    (member
      T
      (mapcar
        '(lambda (p)
          (apply
            'and
            (mapcar
              '(lambda (a b) (equal a b fuzz))
              pt
              (mapcar 'rtos p)
            )
          )
         )
        pt-list
      )
    )
  )
)