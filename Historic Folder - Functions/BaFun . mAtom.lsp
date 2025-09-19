
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  mAtom  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] mAtom []-----------------------[]                                           ;
;--- Date of creation       > 31/12/2021                                                                                                            ;
;--- Last modification date > 31/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaFun"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows a function 'f' to be applied repeatedly ('n' times) to an atom of any type. This makes it possible, for example, to multiply strings and ;
;   concatenate them, to calculate powers 'n' of a number, etc. Its field of application is open because of the 'f' argument, which makes it        ;
;   possible to apply any desired function, however complex, to any type of element.                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (mAtom) have 3 argument(s) :                                                                                                         ;
;   --•  a                      > corresponds to the atom or list you want to apply 'n' times the 'f' function                                      ;
;     (type a) = ...                            | Ex. : " ", 2, 42.23, -9, '(1 2), ...                                                              ;
;   --•  n                      > corresponds to number of occurence of 'a' will be affected by the function 'f'                                    ;
;     (type n) = 'INT                           | Ex. : 0, 1, 10, 15, 23, ...                                                                       ;
;   --•  f                      > corresponds to the quote function you want to apply. Some functions like (cons) won't work.                       ;
;     (type f) = 'SYM                           | Ex. : '+, '*, strcat, 'list, ...                                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (mAtom) returns the result of 'f' applied 'n' times on 'a', or nil if the function 'f' is not able to work properly in this case.  ;
;     Ex. : (mAtom " " 5 'strcat) returns "     ", like (space 5)                                                                                   ;
;           (mAtom 2 8 '*) returns 256, like (expt 2 8)                                                                                             ;
;           (mAtom '(0 . "INSERT") 2 'list) returns '((0 . "INSERT") (0 . "INSERT"))                                                                ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun mAtom (a n f / l)
  (if (> n 0)
    (progn
      (repeat n
        (setq l (cons a l))
      )
      (setq a (vl-catch-all-apply f (reverse l)))
    )
    (setq a nil)
  )
  (if (and a (not (vl-catch-all-error-p a)))
    a
  )
)