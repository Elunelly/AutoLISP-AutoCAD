
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  LM:grsnap:snapfunction  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] LM:grsnap:snapfunction []-----------------------[]                                   ;
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
; The function (LM:grsnap:snapfunction) have 0 argument(s) :                                                                                        ;
;   --•  ...                    >                                                                                                                   ;
;     (type ...) = '...                         | Ex. :                                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:OLE->ACI                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;   --•  "UtDis" ---> LM:grsnap:displaysnap                         | v1.0.0 - 21/08/2014 (LeeMac)                                                  ;
;   --•  "UtDis" ---> LM:grsnap:snapsymbols                         | v1.0.0 - 21/08/2014 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LM:grsnap:snapfunction) returns [fun], a function requiring two arguments:                                                        ;
;     p - [lst] UCS Point to be snapped                                                                                                             ;
;     o - [int] Object Snap bit code                                                                                                                ;
;   The returned function returns either the snapped point (displaying an appropriate snap symbol) or the supplied point if the snap failed for the ;
;   given Object Snap bit code.                                                                                                                     ;
;     Ex. :                                                                                                                                         ;
;       (setq                                                                                                                                       ;
;         osf (LM:grsnap:snapfunction) ;; Define optimised Object Snap function                                                                     ;
;         osm (getvar 'osmode)         ;; Retrieve active Object Snap modes                                                                         ;
;       )                                                                                                                                           ;
;       (while (= 5 (car (setq grr (grread t 13 0)))) ;; While the cursor is moved                                                                  ;
;         (redraw)             ;; Refresh the display                                                                                               ;
;         (osf (cadr grr) osm) ;; Calculate & display any available snap points                                                                     ;
;       )                                                                                                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LM:grsnap:snapfunction ( )
  (eval
    (list
      'lambda
      '( p o / q )
      (list
        'if
        '(zerop (logand 16384 o))
        (list
          'if
          '(setq q
            (cdar
              (vl-sort
                (vl-remove-if
                  'null
                  (mapcar
                    (function
                      (lambda ( a / b )
                        (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                          (list (distance p b) b (car a))
                        )
                      )
                    )
                    '(
                      (0001 . "_end")
                      (0002 . "_mid")
                      (0004 . "_cen")
                      (0008 . "_nod")
                      (0016 . "_qua")
                      (0032 . "_int")
                      (0064 . "_ins")
                      (0128 . "_per")
                      (0256 . "_tan")
                      (0512 . "_nea")
                      (2048 . "_app")
                      (8192 . "_par")
                    )
                  )
                )
                '(lambda ( a b ) (< (car a) (car b)))
              )
            )
          )
          (list
            'LM:grsnap:displaysnap
            '(car q)
            (list
              'cdr
              (list
                'assoc
                '(cadr q)
                (list
                  'quote
                  (LM:grsnap:snapsymbols (atoi (cond ((getenv "AutoSnapSize")) ("5"))))
                )
              )
            )
            (LM:OLE->ACI
              (if (= 1 (getvar 'cvport))
                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
              )
            )
          )
        )
      )
      '(cond ((car q)) (p))
    )
  )
)