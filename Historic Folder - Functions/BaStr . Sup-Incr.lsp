
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  Sup-Incr  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] Sup-Incr []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Increments a string according to a specified step and the number of characters in the string. This works equally well with strings representing ;
;   integers, real numbers, letters, or slightly more complex strings. In the case of complex strings, only the last value representing a number or ;
;   a set of letters will be incremented.                                                                                                           ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Sup-Incr) have 2 argument(s) :                                                                                                      ;
;   --•  value                  > is the string you want to increment. It can represent a number (in string format) or a string                     ;
;     (type value) = 'STR                       | Ex. : "23.5", "-15.0", "42", "AA", "BJHKZ", "001", "99", ...                                      ;
;   --•  inc                    > is the increment value                                                                                            ;
;     (type inc) = 'REAL or 'INT                | Ex. : 1, 0.25, 10, -2, -3.5, ...                                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaStr" ---> Sup-Incr-Letter                               | v3.9.0 - 06/12/2015 (Leemac)                                                  ;
;   --•    "BaAri" ---> _incrementalpha                             | v3.9.0 - 06/12/2015 (Leemac)                                                  ;
;   --•  "BaStr" ---> Sup-Incr-Number                               | v3.9.0 - 06/12/2015 (Leemac)                                                  ;
;   --•    "BaStr" ---> _decimalplaces                              | v3.9.0 - 06/12/2015 (Leemac)                                                  ;
;   --•    "UtDac" ---> _rtos                                       | v3.9.0 - 06/12/2015 (Leemac)                                                  ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Sup-Incr) returns the incremented value of the string.                                                                            ;
;     Ex. : (Sup-Incr "1" 5) returns "6"                                                                                                            ;
;           (Sup-Incr "09" 1) returns "10"                                                                                                          ;
;           (Sup-Incr "A" 2) returns "C"                                                                                                            ;
;           (Sup-Incr "Z" 1) returns "AA"                                                                                                           ;
;           (Sup-Incr "A8" 2) returns "B0"                                                                                                          ;
;           (Sup-Incr "12" 0.13) returns "12.13"                                                                                                    ;
;           (Sup-Incr "12" -6) returns "06"                                                                                                         ;
;           (Sup-Incr "D" -2) returns "B"                                                                                                           ;
;           (Sup-Incr "AA" -2.3) returns "Y"                                                                                                        ;
;           (Sup-Incr "T6-I01" 3) returns "T6-I04"                                                                                                  ;
;           (Sup-Incr "T6-I99" 3) returns "T6-J02"                                                                                                  ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Changing the name of variables and arguments                                                                                   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Sup-Incr (value inc / Sup-Incr-Letter Sup-Incr-Number)
  (defun Sup-Incr-Letter (str inc / _incrementalpha a)
    (defun _incrementalpha (a b / c d e)
      (cond
        ( (cond
            ( (< 47 (setq c (car a)) 58)
              (setq
                d 48
                e 10
              )
            )
            ( (< 64 c 91)
              (setq
                d 65
                e 26
              )
            )
            ( (< 96 c 123)
              (setq
                d 97
                e 26
              )
            )
          )
          (setq
            c (+ (- c d) b)
            b (/ c e)
          )
          (if (not (minusp c))
            (cons
              (+ d (rem c e))
              (if (zerop b)
                (cdr a)
                (if (cdr a)
                  (_incrementalpha (cdr  a) b)
                  (_incrementalpha (list d) (if (= 10 e) b (1- b)))
                )
              )
            )
            (cons
              (+ d e (rem c e))
              (if (and (cdr a) (= (length (cdr a)) 1) (= (cadr a) d))
                '(0)
                (if (zerop b)
                  (_incrementalpha (cdr a) (1- b))
                  (_incrementalpha (list d) (if (= 10 e) b (1- b)))
                )
              )
            )
          )
        )
        ( (cons
            c
            (if (cdr a)
              (_incrementalpha (cdr a) b)
              (_incrementalpha (list 65) (1- b))
            )
          )
        )
      )
    )

    (vl-list->string
      (reverse
        (vl-remove
          0
          (if (setq a (reverse (vl-string->list str)))
            (_incrementalpha a inc)
            (_incrementalpha '(65) (1- inc))
          )
        )
      )
    )
  )

  (defun Sup-Incr-Number (str inc / _rtos _decimalplaces num incd maxd slen strd)
    (defun _rtos (r p / d v)
      (setq d (getvar "DIMZIN"))
      (setvar "DIMZIN" 0)
      (setq v (rtos r 2 p))
      (setvar "DIMZIN" d)
      v
    )

    (defun _decimalplaces (str / pos)
      (if (setq pos (vl-string-position 46 str))
        (- (strlen str) pos 1)
        0
      )
    )
      
    (setq num (+ (distof str) (distof inc)))
    (if (minusp (distof str))
      (setq str (substr str 2))
    )
    (if (minusp (distof inc))
      (setq inc (substr inc 2))
    )
    (setq
      incd (_decimalplaces inc)
      strd (_decimalplaces str)
      maxd (max incd strd)
      slen (strlen str)
    )
    (cond
      ( (and (< 0 strd) (< 0 incd))
        (setq slen (+ (- slen strd) maxd))
      )
      ( (and (= 0 strd) (< 0 incd))
        (setq slen (+ incd slen 1))
      )
    )
    (setq str (_rtos num maxd))
    (if (minusp num)
      (setq str (substr str 2))
    )
    (while (< (strlen str) slen)
      (setq str (strcat "0" str))
    )
    (if (minusp num)
      (strcat "-" str)
      str
    )
  )

  (if (distof value 2)
    (Sup-Incr-Number value (vl-princ-to-string inc))
    (Sup-Incr-Letter value (fix inc))
  )
)