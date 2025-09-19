;                          +---------------------------------------------------------------------------------------------+                          ;
;                          |                                                                                             |                          ;
;                          |              FONCTION EXCELREADER ET INCREMENTATION ALPHABETIQUE + NUMERIQUE                |                          ;
;                          |                                                                                             |                          ;
;                          +---------------------------------------------------------------------------------------------+                          ;



;; ================================================= Importation des fonctions d'autres fichiers ================================================= ;;



;--- Fonction d'ouverture d'un fichier Excel nommé ExFile et de la feuille MyTabl
(defun MSX_Open (Exfile Mytabl)

	(setq MyFile (findfile Exfile))
	(if (/= MyFile nil)
		(progn
			(setq ExcelApp (vlax-get-or-create-object "Excel.Application"))
			(vla-put-visible ExcelApp :vlax-false)
			(vlax-put-property ExcelApp 'DisplayAlerts :vlax-false)
			(setq Active_WkB (vl-catch-all-apply 'vla-open (list (vlax-get-property ExcelApp "WorkBooks") MyFile)))
		)
	)
	(if (/= ExcelApp nil)
		(progn
			(setq Active_Sht (vl-catch-all-apply 'vlax-get-property (list (vlax-get-property Active_WkB "Sheets") "Item" MyTabl)))
			(if (not (vl-catch-all-error-p Active_Sht))
				(vlax-invoke-method Active_Sht "Activate")
				(setq Active_Sht nil)
			)
		)
		(setq Active_Sht nil)
	)

)

;--- Fonction de lecture de la valeur d'une cellule pointée d'un fichier Excel
;--- Fonction en lien avec la fonction (MSX_Open)
(defun GetCell (Cell_name)

	(if (/= ExcelApp nil)
		(progn
			(setq Active_Rng (vlax-get-property (vlax-get-property Active_Sht 'Cells) "Range" Cell_name))
			(setq Active_Cell (vlax-variant-value (vlax-get-property Active_Rng 'Value2)))
		)
		(setq Active_Cell nil)
	)
	Active_Cell

)

;--- Fonction de fermeture du fichier Excel et de l'application Excel + supression des variables de la mémoire
;--- Fonction EN COURS DE DEVELOPPEMENT issue de Marc'Antonio
(defun MSX_Quit ()

	(if (not (vlax-object-released-p ExcelApp))					; Vérifie si l'application Excel n'est pas déchargée.
		(progn												; 
			(if Active_Rng
				(progn
					(vlax-object-released-p Active_Rng)
					(vlax-release-object Active_Rng)
				)
			)
			(if Active_Sht
				(progn
					(vlax-object-released-p Active_Sht)
					(vlax-release-object Active_Sht)
				)
			)
			(if Active_WkB
				(progn
					(vlax-object-released-p Active_WkB)
					(vlax-invoke-method Active_WkB 'Close)
					(vlax-release-object Active_WkB)
				)
			)
			(vlax-invoke-method ExcelApp 'QUIT)
			(vlax-release-object ExcelApp)
		)
	)
	(setq ExFile nil MyTabl nil MyFile nil ExcelApp nil Active_WkB nil Active_Sht nil Active_Rng nil Active_Cell nil)

)


;--- ExFile correspondant au nom d'un fichier présent dans les chemins de support (ex : "UBS-Projet LISP.lsp")
;--- MyTabl correspondant au nom d'une feuille Excel (ex : "Feuil1")
;--- Type_lst correspondant au type de recherche à effectuer sur la feuille ("Tab_H", "Tab_V", "Cell_V" ou "Cell_H")
;--- Value correspondant à une chaîne de caractères ou un nombre que l'on cherche dans une cellule Excel (ex : "Toiture" ou "UBS*" ou "*100*" ...)
;--- Ask_Cell_R correspondant à l'indice de ligne pour la première cellule recherchée (en haut à gauche) (ex : 1, 56, ...)
;--- Ask_Cell_C correspondant à l'indice de colonne pour la première cellule recherchée (en haut à gauche) (ex : "B", "AH", ...)
;--- Rtn_Cell_R correspondant à l'indice de ligne pour la première donnée à récupérer (en haut à gauche) (ex : 12, 56, ...)
;--- Rtn_Cell_C correspondant à l'indice de colonne pour la première donnée à récupérer (en haut à gauche) (ex : "B", "AH", ...)
;--- La recherche requiert d'avoir des lignes et/ou des colonnes continues (pas de cellules vides au milieu de la liste à établir)
;--- La méthode "Tab_H" correspond à récupérer une cellule équivalente à Value dans une colonne spécifiée et retourne la liste des cellules dans la ligne correspondante à partir d'une colonne spécifiée.
;--- La méthode "Tab_V" correspond à récupérer une cellule équivalente à Value dans une ligne spécifiée et retourne la liste des cellules dans la colonne correspondante à partir d'une ligne spécifiée.
;--- La méthode "Cell_H" correspond à récupérer une liste de cellules équivalentes à Value dans une colonne spécifiée et retourne le résultat équivalent de la cellule à la même ligne pour une colonne spécifiée.
;--- La méthode "Cell_V" correspond à récupérer une liste de cellules équivalentes à Value dans une ligne spécifiée et retourne le résultat équivalent de la cellule à la même colonne pour une ligne spécifiée.

(defun ExcelReader_List (ExFile MyTabl Type_lst Value Ask_Cell_C Ask_Cell_R Rtn_Cell_C Rtn_Cell_R / i_C i_R Max_C Max_R Ask_Cell)

	(setq Excel_List nil)
	(MSX_Open Exfile MyTabl)
	(cond
		((= Type_lst "Tab_H")
			(progn
				(setq i_R Ask_Cell_R)
				(while (GetCell (strcat Ask_Cell_C (rtos i_R 2 0)))
					(setq i_R (1+ i_R))
				)
				(setq Max_R i_R)
				(setq i_R Ask_Cell_R)
				(while (< i_R Max_R)
					(if (wcmatch (vl-princ-to-string (GetCell (strcat Ask_Cell_C (rtos i_R 2 0)))) (vl-princ-to-string Value))
						(progn
							(setq Ask_Cell_R i_R)
							(setq Ask_Cell (strcat Ask_Cell_C (rtos Ask_Cell_R 2 0)))
							(setq i_R Max_R)
						)
						(setq i_R (1+ i_R))
					)
				)
				(if (null Ask_Cell) (prompt (strcat "\nValeur recherchée non répertoriée dans le document Excel \"" ExFile "\"")))
				(setq i_C Rtn_Cell_C)
				(while (GetCell (strcat i_C (rtos Ask_Cell_R 2 0)))
					(setq i_C (1+ i_C))
				)
				(setq Max_C i_C)
				(setq i_C Rtn_Cell_C)
				(while (/= i_C Max_C)
					(setq Excel_List (cons (vl-princ-to-string (GetCell (strcat i_C (rtos Ask_Cell_R 2 0)))) Excel_List))
					(setq i_C (1+ i_C))
				)
			)
		)
		((= Type_lst "Tab_V")
			(progn
				(setq i_C Ask_Cell_C)
				(while (GetCell (strcat i_C (rtos Ask_Cell_R 2 0)))
					(setq i_C (1+ i_C))
				)
				(setq Max_C i_C)
				(setq i_C Ask_Cell_C)
				(while (/= i_C Max_C)
					(if (wcmatch (vl-princ-to-string (GetCell (strcat i_C (rtos Ask_Cell_R 2 0)))) (vl-princ-to-string Value))
						(progn
							(setq Ask_Cell_C i_C)
							(setq Ask_Cell (strcat Ask_Cell_C (rtos Ask_Cell_R 2 0)))
							(setq i_C Max_C)
						)
						(setq i_C (1+ i_C))
					)
				)
				(if (null Ask_Cell) (prompt (strcat "\nValeur recherchée non répertoriée dans le document Excel \"" ExFile "\"")))
				(setq i_R Rtn_Cell_R)
				(while (GetCell (strcat Ask_Cell_C (rtos i_R 2 0)))
					(setq i_R (1+ i_R))
				)
				(setq Max_R i_R)
				(setq i_R Rtn_Cell_R)
				(while (< i_R Max_R)
					(setq Excel_List (cons (vl-princ-to-string (GetCell (strcat Ask_Cell_C (rtos i_R 2 0)))) Excel_List))
					(setq i_R (1+ i_R))
				)
			)
		)
		((= Type_lst "Cell_V")
			(progn
				(setq i_C Ask_Cell_C)
				(while (GetCell (strcat i_C (rtos Ask_Cell_R 2 0)))
					(setq i_C (1+ i_C))
				)
				(setq Max_C i_C)
				(setq i_C Ask_Cell_C)
				(while (/= i_C Max_C)
					(if (wcmatch (vl-princ-to-string (GetCell (strcat i_C (rtos Ask_Cell_R 2 0)))) (vl-princ-to-string Value))
						(setq Excel_List (cons (vl-princ-to-string (GetCell (strcat i_C (rtos Rtn_Cell_R 2 0)))) Excel_List))
					)
					(setq i_C (1+ i_C))
				)
			)
		)
		((= Type_lst "Cell_H")
			(progn
				(setq i_R Ask_Cell_R)
				(while (GetCell (strcat Ask_Cell_C (rtos i_R 2 0)))
					(setq i_R (1+ i_R))
				)
				(setq Max_R i_R)
				(setq i_R Ask_Cell_R)
				(while (< i_R Max_R)
					(if (wcmatch (vl-princ-to-string (GetCell (strcat Ask_Cell_C (rtos i_R 2 0)))) (vl-princ-to-string Value))
						(setq Excel_List (cons (vl-princ-to-string (GetCell (strcat Rtn_Cell_C (rtos i_R 2 0)))) Excel_List))
					)
					(setq i_R (1+ i_R))
				)
			)
		)
		((/= Type_lst "Tab_H" "Tab_V" "Cell_H" "Cell_V")
			(progn
				(setq Excel_List "Empty")
				(prompt (strcat "\nLa méthode \"" Type_lst "\" n'a pas été programmée pour le moment.\nVeuillez vous référer aux méthodes actuellement programmées."))
			)
		)
	)
	(MSX_Quit)
	(if (null Excel_List) (prompt "\nValeur recherchée non répertoriée dans le document Excel"))
	(setq Excel_List (reverse Excel_List))
	Excel_List

) ; Retourne une liste sous forme de string pouvant ensuite être exploitée par la suite soit sous format DXF, soit sous format d'atomes
;--- Fonction purement fonctionnelle