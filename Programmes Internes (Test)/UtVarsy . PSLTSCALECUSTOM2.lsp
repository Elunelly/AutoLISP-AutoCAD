;                                     []-----------------------[] PSLTSCALECUSTOM2 []-----------------------[]                                      ;
;--- Date of creation       > 08/02/2024                                                                                                            ;
;--- Last modification date > 08/02/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtVarsy"                                                                                                             ;

(defun c:PSLTSCALECUSTOM2 (/ lay val)
  (setq lay (getvar "CTAB"))
  (initget "0 1")
  (setq val (cond ((setq val (getkword "\nSpécifiez la nouvelle valeur de PSLTSCALE [0/1] <0> : ")) (atoi val)) (0)))
  (mapcar '(lambda (l) (setvar "CTAB" l) (setvar "PSLTSCALE" val)) (layoutlist))
  (setvar "CTAB" lay)
  (princ)
)