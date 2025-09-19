
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  sort-cons  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] sort-cons []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 25/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to sort a list of pointed pairs in ascending order according to the key of each pointed pair without deleting duplicates, thus           ;
;   preserving the length of the initial list as well as the duplicate values.                                                                      ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (sort-cons) have 1 argument(s) :                                                                                                     ;
;   --•  lst                    > corresponds to the list of pointed pairs that we want to sort                                                     ;
;     (type lst) = 'LST                         | Ex. : '(("A" . 1) ("B" . 6) ("A" . 2) ("C" . 1) ("B" . 6)), ...                                   ;
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
;   The function (sort-cons) returns the list sorted in ascending order of the keys and values for each key, provided that the list is only         ;
;   composed of pointed pairs, otherwise returns the starting list.                                                                                 ;
;     Ex. : (sort-cons '(("A" . 1) ("B" . 6) ("A" . 2) ("B" . 6))) returns (("A" . 1) ("A" . 2) ("B" . 6) ("B" . 6))                                ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Modification of the naming of variables and arguments                                                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun sort-cons (lst / tmp-list n)
  (if
    (and
      (not (member nil (mapcar 'vl-consp lst)))
      (null (vl-remove nil (mapcar 'vl-list-length lst)))
    )
    (progn
      (while lst
        (setq
          tmp-list (cons (cons (car lst) (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst))) tmp-list)
          lst (vl-remove (car lst) lst)
        )
      )
      (setq
        tmp-list
          (vl-sort
            tmp-list
            '(lambda (e1 e2)
              (if (= (vl-princ-to-string (caar e1)) (vl-princ-to-string (caar e2)))
                (< (vl-princ-to-string (cdar e1)) (vl-princ-to-string (cdar e2)))
                (< (vl-princ-to-string (caar e1)) (vl-princ-to-string (caar e2)))
              )
             )
          )
      )
      (foreach n tmp-list
        (repeat (cdr n)
          (setq lst (cons (car n) lst))
        )
      )
      (setq lst (reverse lst))
    )
    lst
  )
)