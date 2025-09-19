(defun c:KillPVcase ()

	(cond
		(pck
			(setq pck nil)
			(prompt "\nDésormais, les calques nommés \"PVcase*\" ne seront pas purgés de manière automatique\nStatut = Inactif")
		)
		((null pck)
			(setq pck t)
			(prompt "\nDésormais, les calques nommés \"PVcase*\" seront purgés de manière automatique (Double Clic pour activer la purge)\nStatut = Activé")
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