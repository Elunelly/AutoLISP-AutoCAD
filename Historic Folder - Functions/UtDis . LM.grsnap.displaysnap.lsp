
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                        --{  LM:grsnap:displaysnap  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                   []-----------------------[] LM:grsnap:displaysnap []-----------------------[]                                   ;
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
; The function (LM:grsnap:displaysnap) have 3 argument(s) :                                                                                         ;
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
;   The function (LM:grsnap:displaysnap) returns [fun], a function requiring two arguments:                                                         ;
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

(defun LM:grsnap:displaysnap ( pnt lst col / scl )
  (setq
    scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
    pnt (trans pnt 1 2)
  )
  (grvecs
    (cons col lst)
    (list
      (list scl 0.0 0.0 (car  pnt))
      (list 0.0 scl 0.0 (cadr pnt))
      (list 0.0 0.0 scl 0.0)
      '(0.0 0.0 0.0 1.0)
    )
  )
)