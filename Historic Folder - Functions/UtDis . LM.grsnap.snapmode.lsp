
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                         --{  LM:grsnap:snapmode  }--                                                          | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                    []-----------------------[] LM:grsnap:snapmode []-----------------------[]                                     ;
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
; The function (LM:grsnap:snapmode) have 1 argument(s) :                                                                                            ;
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
;   The function (LM:grsnap:snapmode) returns [fun], a function requiring two arguments:                                                            ;
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

(defun LM:grsnap:snapmode ( str )
  (vl-some
    (function
      (lambda ( x )
        (if (wcmatch (car x) (strcat (strcase str t) "*"))
          (progn
            (princ (cadr x)) (caddr x)
          )
        )
      )
    )
    '(
      ("endpoint"      " of " 00001)
      ("midpoint"      " of " 00002)
      ("center"        " of " 00004)
      ("node"          " of " 00008)
      ("quadrant"      " of " 00016)
      ("intersection"  " of " 00032)
      ("insert"        " of " 00064)
      ("perpendicular" " to " 00128)
      ("tangent"       " to " 00256)
      ("nearest"       " to " 00512)
      ("appint"        " of " 02048)
      ("parallel"      " to " 08192)
      ("none"          ""     16384)
    )
  )
)