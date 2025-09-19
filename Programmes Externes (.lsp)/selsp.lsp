;;;---------------------------------------------------------------------------;
;;;
;;;    SELSP.LSP
;;;
;;;    (C) Copyright 1998 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;    July 1996
;;; 
;;;---------------------------------------------------------------------------;
;;;
;;;    DESCRIPTION
;;;
;;;    List out the vertices of a polyline.
;;;
;;;---------------------------------------------------------------------------;

;;;************************************************************************
;;; Function: C:SELSP
;;;
;;; Function lists the points referenced by a pline and 
;;; optionally number the vertices with text objects.
;;;
;;;
(defun C:SELSP (/ en ed done pt ptlen	ptct ct	size markem error typ)

  (setq error *error*)
  ;;
  ;; Define error handler
  ;;
  (defun *error* (msg)
    (alert msg)
    (setq *error* error)
    (exit)
  )

  ;;
  ;; Input a pline to process
  ;;
  (setq en (car (entsel "\nSelect a pline: ")))
  (if (null en)
    (prompt "\nNo object selected.")
    (progn
      (setq typ (cdr (assoc 0 (entget en))))
      (if (and (/= typ "POLYLINE") (/= typ "LWPOLYLINE"))
	(progn
	  (prompt "\nERROR: Object selected is not a pline.")
	  (setq en nil)
	)
      )
    )
  )
  (if en
    (progn
      (initget "Yes No")
					;(setq markem (getkword "\nMark vertices? (Yes/No) <No>: "))
      (setq markem "No")

      (if (not markem)
	(setq markem "No")
      )
					;(if (= markem "Yes")
					;  (setq size (getdist "\nText size: "))
					;)



      (if (= typ "POLYLINE")
	(progn
	  (setq ptct 0)
	  (setq done nil)



	  (entmake '((0 . "POLYLINE")    ; Object type
          (62 . 5)             ; Color
          ))
	  (while (not done)
	    (setq ed (entget en))
	    (if	(= (cdr (assoc 0 ed)) "SEQEND")
	      (setq done T)
	    )
	    (if	(= (cdr (assoc 0 ed)) "VERTEX")
	      (progn
		(setq ptct (+ ptct 1))
		(setq pt (cdr (assoc 10 ed)))
		(setq lstpt (append pt))

					;(princ "\n")
					;(princ pt)

					;(if (= markem "Yes")
					;  (entmake (list (cons 0 "TEXT")
					;                 (cons 1 (itoa ptct))
					;                 (cons 10 pt)
					;                 (cons 40 size)
					;           )
					;  )
					;)
                                        (entmake '((0 . "VERTEX")      ; Object type
					          (cons 10 pt)    ; Start point
                                        ) )


	       )
	    )
	    (if	(not done)
	      (progn
		(setq en (entnext en))
		(setq ed (entget en))
	      )
	    )
	  )				;while
	 
          (entmake '((0 . "SEQEND")))    ; Sequence end 


			 )
      )
      (if (= typ "LWPOLYLINE")
	(progn
	  (setq ct 0)
	  (setq ptct 0)
	  (setq ed (entget en))
	  (setq ptlen (cdr (assoc 90 ed)))
	  (setq len (length ed))
	  (setq lstpt nil)
	  (command "_.PSELECT" "SP")
	  (while (<= ct len)
	    (setq elem (nth ct ed))
	    (setq ct (+ ct 1))
	    (if	(= 10 (car elem))
	      (progn
		(setq ptct (+ ptct 1))
		(setq pt (cdr elem))
                (setq lstpt (append pt))
		;(command (list (nth 0 lstpt) "," (nth 1 lstpt))
                (command lstpt)
		;(princ "\n")
					;(princ pt)

					;(if (= markem "Yes")
					;  (entmake (list (cons 0 "TEXT")
					;                 (cons 1 (itoa ptct))
					;                 (cons 10 pt)
					;                 (cons 40 size)
					;           )
					;  )
					;)
	      )
	    )
	  )				;while elements in lwpolyline
          (command "")

	  
	)
      )					;if lwpolyline
    )
  )					;if

  (setq *error* error)

  (prompt "\n\nProcessing completed.")
  (command "" "")
  (princ)

)					;C:SELSP

(prompt "\nType: SELSP to list the points of a pline.")
(princ)

