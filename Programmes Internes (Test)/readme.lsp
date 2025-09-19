(defun readme (/ filename file line lst)

	(if (and (setq filename (findfile "readme.lsp"))
		 (setq file (open filename "R"))
	    )
		(while (setq line (read-line file))
			(setq lst (cons line lst))
		)
	)
	lst

)