;                                        []-----------------------[] ROOFSGCOUNT []-----------------------[]                                        ;
;--- Date of creation       > 08/02/2024                                                                                                            ;
;--- Last modification date > 08/02/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtVarsy"                                                                                                             ;

(defun c:ROOFSGCOUNT (/ jsel i name pts n lst)
  (and
    (setq jsel (ssget '((0 . "POLYLIGN") (8 . "UBS-140-Cablage*"))))
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        pts (get-pt-list name)
        n (/ (- (length pts) 2) 2.)
      )
      (if (not (= n (fix n)))
        (princ (strcat "\n/!\\ La polyligne \"" (cdr (assoc 5 (entget name))) "\" possède un nombre impair de sommets (" (itoa (length pts)) ")"))
        (setq lst (cons n lst))
      )
    )
    (princ
      (strcat
        "\nSur les " (itoa (sslength jsel)) " polylignes prisent en compte, " (itoa (length lst)) " polylignes ont un nombre pair de sommets."
        "\nUn total de " (mapcar '+ lst) " changement de rangée sont comptés avec max = " (apply 'max lst) ", min = " (apply 'min lst)
      )
    )
  )
  (princ)
)