;; IMPOROP (gile)

;; Importe dans le dessin courant les propriétés personnalisées d'un fichier (dwg ou dwt)


(defun c:improp (/ target filename doc odbx source)

  (or *acdoc*

      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))

  )

  (setq target (vla-get-SummaryInfo *acdoc*))

  (if

    (setq filename (getfiled "Choisir le fichier source"

                             (getvar 'dwgprefix)

                             "dwg;dwt"

                             0

                   )

    )

     (progn

       (if

         (not

           (and

             (setq

               doc (GetItem

                     (vla-get-Documents (vlax-get-acad-object))

                     (strcat (vl-filename-base filename) ".dwg")

                   )

             )

             (= filename (vla-get-FullName doc))

           )

         )

          (setq doc  (OpenDrawingDBX filename)

                odbx T

          )

       )

       (setq source (vla-get-SummaryInfo doc)

             n      -1

       )

       (foreach p

                '(Author Comments HyperlinkBase KeyWords Subject Title)

         (if (/= "" (setq prop (vlax-get source p)))

           (vlax-put target p prop)

         )

       )

       (repeat (vla-NumCustomInfo source)

         (vla-GetCustomByIndex source (setq n (1+ n)) 'key 'val)

         (vla-AddCustomInfo target key val)

       )

       (and odbx (vlax-release-object doc))

     )

  )

  (princ)

)


;;; Accéder à un dessin fermé


(defun OpenDrawingDBX (filename / objdbx release)

  (setq objdbx

         (vlax-create-object

           (if (< (setq release (atoi (getvar "ACADVER"))) 16)

             "ObjectDBX.AxDbDocument"

             (strcat "ObjectDBX.AxDbDocument." (itoa release))

           )

         )

  )

  (vla-open objdbx filename)

  objdbx

)


;;; GetItem (gile)

;;; Retourne le vla-object de l'item s'il est présent dans la collection

;;;

;;; Arguments

;;; col : la collection (vla-object)

;;; name : le nom de l'objet (string) ou son indice (entier)

;;;

;;; Retour : le vla-object ou nil


(defun GetItem (col name / obj)

  (vl-catch-all-apply

    (function (lambda () (setq obj (vla-item col name))))

  )

  obj

)
