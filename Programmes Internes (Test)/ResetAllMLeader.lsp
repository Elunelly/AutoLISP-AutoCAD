(defun c:RAML (/ ResetMLeader jsel i name n)
  (defun ResetMLeader (name / entlist i l pt-list)
    (setq entlist (entget name))
    (while (setq entlist (member '(304 . "LEADER_LINE{") entlist))
      (setq entlist (cddr entlist))
      (repeat (setq i (1+ (vl-position '(305 . "}") entlist)))
        (setq l (nth (setq i (1- i)) entlist))
        (if (= (car l) 10)
          (setq pt-list (cons l pt-list))
        )
      )
    )
    (if pt-list
      (progn
        (setq entlist (entget name))
        (mapcar '(lambda (pt) (setq entlist (vl-remove pt entlist))) pt-list)
        (entmod entlist)
      )
    )
  )
  (if (setq jsel (ssget "_X" '((0 . "MULTILEADER") (410 . "Model"))))
    (progn
      (repeat (setq n 0 i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if (ResetMLeader name)
          (setq n (1+ n))
        )
      )
      (princ
        (strcat
          "\nA total of "
          (itoa n)
          " / "
          (itoa (sslength jsel))
          " MLeader were modified and had intermediate vertices."
        )
      )
    )
  )
  (princ)
)