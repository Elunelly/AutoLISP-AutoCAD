(defun c:KillPVcase ()

	(cond
		(pck
			(setq pck nil)
			(prompt "\nD�sormais, les calques nomm�s \"PVcase*\" ne seront pas purg�s de mani�re automatique\nStatut = Inactif")
		)
		((null pck)
			(setq pck t)
			(prompt "\nD�sormais, les calques nomm�s \"PVcase*\" seront purg�s de mani�re automatique (Double Clic pour activer la purge)\nStatut = Activ�")
		)
	)
	(princ)

)

(vl-load-com)

(vlr-mouse-reactor "PVcase" '((:vlr-beginDoubleClick . PvcaseLayerKill)))

(defun PVcaseLayerKill (reactor_object lst / i lst layer acet)

	(if (and pck
		 (setq i 0
		       lst (flt_tbl "LAYER" "PVcase*")
		 )
		 (setq acet (acet-ui-progress "" (length lst)))
	    )
		(progn
			(foreach layer lst
				(vla-delete (vlax-ename->vla-object (tblobjname "LAYER" layer)))
				(acet-ui-progress (setq i (1+ i)))
			)
			(setq acet (acet-ui-progress))
		)
	)
	(princ)

)