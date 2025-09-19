
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  VP-scale  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] VP-scale []-----------------------[]                                         ;
;--- Date of creation       > 09/12/2021                                                                                                            ;
;--- Last modification date > 29/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows to calculate the scale of a viewport object and to return the result as "1/# ###".                                                       ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (VP-scale) have 1 argument(s) :                                                                                                      ;
;   --•  name                   > corresponds to the entity name of the viewport                                                                    ;
;     (type name) = 'ENAME                      | Ex. : <Nom d'entité: 1b09574baf0>, (car (entsel)), ...                                            ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> ThsdSpace                                     | v1.0.0 - 29/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "UtDac" ---> cvunits                                       | v1.0.0 - 09/12/2021 (Luna)                                                    ;
;   --•    "UtDac" ---> f                                           | v1.0.0 - 09/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (VP-scale) returns the viewport's scale as a string like "1/# ###".                                                                ;
;     Ex. : (VP-scale (car (entsel))) returns "1/500", "1/1 000", "1/1 250", "1/50", ...                                                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Creation of the function (ThsdSpace) and finish the editing of (VP-scale)                                                      | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun VP-scale (name / cvunits Heo Hep u xp)
  (defun cvunits (value from-unit to-unit / f)
    (defun f (u)
      (if (= (type u) 'INT)
        (cond
          ((= u 1) "inch")
          ((= u 2) "foot")
          ((= u 3) "mile")
          ((= u 4) "millimeter")
          ((= u 5) "centimeter")
          ((= u 6) "meter")
          ((= u 7) "kilometer")
          ((= u 8) "microinch")
          ((= u 9) "millipouce")
          ((= u 10) "yard")
          ((= u 11) "Angstrom")
          ((= u 12) "nanometer")
          ((= u 13) "micron")
          ((= u 14) "decimeter")
          ((= u 15) "dekameter")
          ((= u 16) "hectometer")
          ((= u 17) "gigameter")
          ((= u 18) "astronomical_unit")
          ((= u 19) "light_year")
          ((= u 20) "parsec")
          ((= u 21) "survey_foot")
        )
        u
      )
    )
    
    (cvunit value (f from-unit) (f to-unit))
  )

  (if (= "VIEWPORT" (cdr (assoc 0 (entget name))))
    (setq
      u (getvar "INSUNITS")
      Heo (float (cvunits (cdr (assoc 45 (entget name))) u 4))
      Hep (float (cvunits (cdr (assoc 41 (entget name))) u 6))
      xp (/ Hep Heo)
      xp (strcat "1/" (ThsdSpace (rtos (/ 1 xp) 2 0) " "))
    )
  )
)