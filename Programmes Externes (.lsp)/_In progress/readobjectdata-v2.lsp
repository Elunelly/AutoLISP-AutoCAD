  (defun c:rod (/ )(c:readobjectdata))

  (vl-load-com)

  (defun c:readobjectdata (/ count *error* ename textename fieldnames fieldstring input strcatlst tablelist usercmdecho)

    (defun *error* ( msg )
      (princ (strcat "\n<" msg ">\n"))
      (progn
        (and TextENAME (entdel TextENAME))
        ;(vl-cmdf "_.usc" "_previous")
        (setvar "cmdecho" usercmdecho)
      );progn
      (princ)
    );defun

    (setq usercmdecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    ;(vl-cmdf "_.ucs" "_world")
    (while (and (setq Input (grread T 4 2)) (= (car Input) 5))
      (if TextENAME
        (progn (entdel TextENAME) (setq TextENAME nil))
      );if
      (if
        (and (setq ename (car (nentselp (cadr Input))))
          (not (eq TextENAME ename))
        );and

        (if (setq tablelist (ade_odgettables ename))
          (progn
            (setq count 0 fieldnames nil)
            (repeat (length tablelist)
              (foreach x (cdr (assoc "Columns" (ade_odtabledefn (nth count tablelist))))
                (if (not (member (cdr (assoc "ColName" x)) fieldnames))
                  (setq fieldnames (cons (cons (cons (1+ count) (nth count tablelist)) (cdr (assoc "ColName" x))) fieldnames))
                )
              )
              (setq count (1+ count))
            )
            (if fieldnames 
              (progn
                (setq fieldnames (reverse fieldnames))
                (setq strcatlst
                  (apply 'strcat
                    (mapcar
                      (function
                        (lambda (x)
                          (strcat "{\\C" (itoa (caar x)) ";" (cdar x) "} - " (cdr x) " : "
                            (if (/= (type (setq fieldstring (ade_odgetfield ename (cdar x) (cdr x) 0))) 'STR)
                              (if (eq (type fieldstring) 'INT)
                                (itoa fieldstring)
                                (rtos fieldstring 2 2)
                              )
                              fieldstring
                            )
                          "\n")
                        )
                      );fin function
                      fieldnames
                    );fin mapcar
                  );fin apply
                );fin setq
                (setq TextENAME
                  (entmakex
                    (list
                      (cons 0 "MTEXT")
                      (cons 100 "AcDbEntity")
                      (cons 100 "AcDbMText")
                      (cons 1
                        (strcat "{\\fArial;" strcatlst "}" );strcat
                      );cons 1
                      (cons 10
                        (polar (cadr Input) 0 (/ (getvar "VIEWSIZE") 50.0))
                      )
                      (cons 40 (/ (getvar "VIEWSIZE") 75.0));
                      (cons 50 (- 0 (getvar "VIEWTWIST")))
                      (cons 62 250)
                      (cons 71 1)
                      (cons 72 5)
                      (cons 90 1)
                      (cons 63 255)
                      (cons 45 1.2)
                    );list
                  );entmakex
                );setq
              )
            )
          )
        )

      );if
    );fin while
    (and TextENAME (entdel TextENAME))
    ;(vl-cmdf "_.ucs" "_previous")
    (setvar "cmdecho" usercmdecho)
    (princ)
  )