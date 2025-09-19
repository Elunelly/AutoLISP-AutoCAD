;;;=================================================================
;;;
;;; COUP.LSP V1.00
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:coup(/ cmd h s)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion des erreurs
  ;;;
  ;;;---------------------------------------------------------------

  (defun *errcoup* (msg)
    (if (/= msg "Function cancelled")
      (if (= msg "quit / exit abort")
        (princ)
        (princ (strcat "\nErreur : " msg))
      )
      (princ)
    )
    (setq *error* s)
    (setvar "pickadd" h)
    (setvar "cmdecho" cmd)
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Couper une ligne
  ;;;
  ;;;---------------------------------------------------------------

  (defun couper_ligne(/ a b c d e f g n o p1 p2 q)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Trier les points d'intersections
  ;;;
  ;;;---------------------------------------------------------------

    (defun trie_liste(o / b c d f i k n l)
      (setq b e)
      (setq l 0)
      (while (/= (car b) nil)
        (setq i (car b))
        (cond
          ((or (= o "DG") (= o "GD"))
            (foreach n b
              (if (< (car n) (car i))
                (setq i n)
              )
            )
          )
        )
        (cond
          ((or (= o "HB") (= o "BH"))
            (foreach n b
              (if (< (cadr n) (cadr i))
                (setq i n)
              )
            )
          )
        )
        (cond
          ((or (= o "BGH") (= o "HDB"))
            (foreach n b
              (if (< (car n) (car i))
                (setq i n)
              )
            )
            (foreach n b
              (if (and (= (car n) (car i)) (< (cadr n) (cadr i)))
                (setq i n)
              )
            )
          )
          ((or (= o "HGB") (= o "BDH"))
            (foreach n b
              (if (< (car n) (car i))
                (setq i n)
              )
            )
            (foreach n b
              (if (and (= (car n) (car i)) (> (cadr n) (cadr i)))
                (setq i n)
              )
            )
          )
        )
        (setq d (append (list i) d))
        (setq c nil)
        (foreach n b
          (if (/= n i)
            (setq c (append (list n) c))
          )
        )
        (setq b c)
        (setq l (1+ l))
      )
      (if (or (= o "GD") (= o "BH") (= o "BGH") (= o "BGD") (= o "HGB") (= o "BDG"))
        (setq e (reverse d))
        (setq e d)
      )
    )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine de Couper
  ;;;
  ;;;---------------------------------------------------------------

    (princ "\nSélectionnez la ligne à couper.")
    (setq e nil)
    (setvar "pickadd" 0)
    (setq a (ssget))
    (if a
      (progn
        (setq n 0)
        (while (ssname a n)
          (if (= (cdr (assoc 0 (entget (ssname a n)))) "LINE")
            (setq b n)
          )
          (setq n (1+ n))
        )
        (if b
          (progn
            (princ "\nSélectionnez les intersections.")
            (setvar "pickadd" 1)
            (setq c (ssget))
            (if c
              (progn
                (setq n 0)
                (setq d nil)
                (while (ssname c n)
                  (if (= (cdr (assoc 0 (entget (ssname c n)))) "LINE")
                    (setq d n)
                  )
                  (setq n (1+ n))
                )
                (if d
                  (progn
                    (setq n 0)
                    (setq i 0)
                    (setq b (entget (ssname a b)))
                    (while (ssname c n)
                      (setq d (entget (ssname c n)))
                      (if (= (cdr (assoc 0 d)) "LINE")
                        (progn
                          (setq f (inters (list (cadr (assoc 10 b)) (caddr (assoc 10 b)))
                                          (list (cadr (assoc 11 b)) (caddr (assoc 11 b)))
                                          (list (cadr (assoc 10 d)) (caddr (assoc 10 d)))
                                          (list (cadr (assoc 11 d)) (caddr (assoc 11 d)))))
                          (if f
                            (progn
                              (setq i (1+ i))
                              (setq e (append (list f) e))
                            )
                          )
                        )
                      )
                      (setq n (1+ n))
                    )
                    (if e
                      (progn
                        (cond
                          ((= (cadr (assoc 10 b)) (cadr (assoc 11 b)))
                            (if (> (caddr (assoc 10 b)) (caddr (assoc 11 b)))
                              (setq o "HB")
                              (setq o "BH")
                            )
                          )
                          ((= (caddr (assoc 10 b)) (caddr (assoc 11 b)))
                            (if (> (cadr (assoc 10 b)) (cadr (assoc 11 b)))
                              (setq o "DG")
                              (setq o "GD")
                            )
                          )
                          ((> (cadr (assoc 10 b)) (cadr (assoc 11 b)))
                            (if (> (caddr (assoc 10 b)) (caddr (assoc 11 b)))
                              (setq o "HDB")
                              (setq o "BGH")
                            )
                          )
                          ((< (cadr (assoc 10 b)) (cadr (assoc 11 b)))
                            (if (> (caddr (assoc 10 b)) (caddr (assoc 11 b)))
                              (setq o "HGB")
                              (setq o "BDH")
                            )
                          )
                        )
                        (trie_liste o)
                        (if (or (= o "BDH") (= o "BGH"))
                          (progn
                            (setq e (reverse e))
                            (setq k (angle (list (cadr (assoc 11 b)) (caddr (assoc 11 b)))
                                           (list (cadr (assoc 10 b)) (caddr (assoc 10 b)))))
                          )
                          (setq k (angle (list (cadr (assoc 10 b)) (caddr (assoc 10 b)))
                                         (list (cadr (assoc 11 b)) (caddr (assoc 11 b)))))
                        )
                        (setq g (getreal (strcat "\nValeur du décalage <" (rtos g_lign) "> : ")))
                        (if g
                          (setq g_lign g)
                        )
                        (setq n 0)
                        (while (nth n e)
                          (setq p1 (polar (nth n e) k g_lign))
                          (setq p2 (polar (nth n e) k (- g_lign)))
                          (command "_.break" (cdr (assoc -1 b)) "_none" p1 "_none" p2)
                          (setq q (entget (ssname (ssget "_l") 0)))
                          (if (/= (cdr (assoc -1 b)) (cdr (assoc -1 q)))
                            (setq b q)
                            (setq b (entget (cdr (assoc -1 b))))
                          )
                          (setq n (1+ n))
                        )
                      )
                      (princ "\nPas d'intersection.")
                    )
                  )
                  (princ "\nAucune ligne de sélectionnée.")
                )
              )
              (princ "\nAucune Sélection.")
            )
          )
          (princ "\nAucune ligne de sélectionnée.")
        )
      )
      (princ "\nAucune Sélection.")
    )
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine de lancement
  ;;;
  ;;;---------------------------------------------------------------

  (setq s *error*)
  (setq *error* *errcoup*)
  (setq cmd (getvar "cmdecho"))
  (setq h (getvar "pickadd"))
  (setvar "cmdecho" 0)
  (command "_.undo" "_group")
  (if (not g_lign)
    (setq g_lign 1.0)
  )
  (couper_ligne)
  (command "_.undo" "_end")
  (setq *error* s)
  (setvar "pickadd" h)
  (setvar "cmdecho" cmd)
  (princ)
)

(setq nom_lisp "COUP")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)
