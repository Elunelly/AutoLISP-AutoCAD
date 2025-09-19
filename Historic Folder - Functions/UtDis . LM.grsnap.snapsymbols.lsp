
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                        --{  LM:grsnap:snapsymbols  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                   []-----------------------[] LM:grsnap:snapsymbols []-----------------------[]                                   ;
;--- Date of creation       > 21/08/2014                                                                                                            ;
;--- Last modification date > 21/08/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   This function forms the core element of the GrSnap utility; the function defines & returns a function requiring two arguments: a point from     ;
;   which to calculate an available snap point & display the appropriate snap symbol, and an Object Snap bit code corresponding to one or more snap ;
;   modes that are to be considered when calculating the snap point.                                                                                ;
;   You may be puzzled as to why this function has been designed to return another function to be used to perform the snap calculations, rather     ;
;   than performing the calculations itself. The answer is efficiency.                                                                              ;
;   There are many operations pertaining to the construction of the Object Snap symbol vectors which only need to be performed once when the        ;
;   calling program is run. Given that the acquisition of a snap point is likely to be performed continuously within a grread loop, it would be     ;
;   incredibly inefficient & unnecessary if such operations were to be performed repeatedly.                                                        ;
;   Global variables could obviously be used to overcome this inefficiency, but the function then 'leaks' data and it becomes the responsibility of ;
;   the calling program to clean up the variables when finished.                                                                                    ;
;   Instead, by defining & returning an optimised function, the calling program can assign the function to a local symbol (outside of any loop      ;
;   construct), and then evaluate the local function within the grread loop.                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LM:grsnap:snapsymbols) have 1 argument(s) :                                                                                         ;
;   --•  ...                    >                                                                                                                   ;
;     (type ...) = '...                         | Ex. :                                                                                             ;
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
;   The function (LM:grsnap:snapsymbols) returns [fun], a function requiring two arguments:                                                         ;
;     p - [lst] UCS Point to be snapped                                                                                                             ;
;     o - [int] Object Snap bit code                                                                                                                ;
;   The returned function returns either the snapped point (displaying an appropriate snap symbol) or the supplied point if the snap failed for the ;
;   given Object Snap bit code.                                                                                                                     ;
;     Ex. :                                                                                                                                         ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
  (setq
    -p (- p)
    q (1+  p)
    -q (- q)
    r (+ 2 p)
    -r (- r)
    i (/ pi 6.0)
    a 0.0
  )
  (repeat 12
    (setq
      l (cons (list (* r (cos a)) (* r (sin a))) l)
      a (- a i)
    )
  )
  (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
  (list
    (list 1
      (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
    )
    (list 2
      (list -r -q) (list 0  r) (list 0  r) (list r -q)
      (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
    )
    (cons 4 c)
    (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
    (list 16
      (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
      (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
      (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
    )
    (list 32
      (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
      (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
    )
    (list 64
      '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
      '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
      '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
      '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
    )
    (list 128
      (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
      (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
      (list -p q) (list -p -p) (list -p -p) (list q -p)
      (list -q q) (list -q -q) (list -q -q) (list q -q)
    )
    (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
    (list 512
      (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
      (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
    )
    (list 2048
      (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
      (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
      (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
      (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
      (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
    )
    (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
  )
)