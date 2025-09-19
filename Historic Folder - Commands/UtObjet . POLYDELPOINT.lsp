
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                            --{  POLYDELPOINT  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] POLYDELPOINT []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 15/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.1.1                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allows the user to delete every vertex from selected LWPOLYLINE and POLYLINE within a specified radius from the cursor. If the remaining amount ;
;   of vertices is inferior to 2, deletes the polyline from the drawing. Can be useful to delete common vertices between several polylines without  ;
;   having to use the context menu for single polyline/vertex.                                                                                      ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:OLE->RGB                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;   --•  "BaLst" ---> 2D-Point                                      | v1.0.0 - 12/08/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> divlist                                       | v2.0.0 - 16/06/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> loop-a-list-properties                        | v2.0.0 - 28/02/2022 (Luna)                                                    ;
;   --•  "UtDis" ---> grcircle                                      | v2.0.0 - 26/08/2022 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 22/08/2022 (Luna)                                                    ;
;   --•  "UtDis" ---> Pdp-princ                                     | v1.0.0 - 03/10/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> OverkillPoly                                  | v1.0.0 - 30/09/2022 (Luna)                                                    ;
;   --•  "VlObj" ---> Poly-DeletePoints                             | v1.0.0 - 25/08/2022 (Luna)                                                    ;
;   --•    "DtObj" ---> LWPOLY_RemoveNthPoints                      | v1.0.0 - 25/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (POLYDELPOINT) returns the number of vertices deleted for each polyline or the name of polylines deleted if less than 2 vertices    ;
;   remain.                                                                                                                                         ;
;     Ex. :                                                                                                                                         ;
;       command: POLYDELPOINT                                                                                                                       ;
;       Specify the tolerance value for the distance between coordinates and vertex [?] <30.0> :                                                    ;
;       Please, select a point :                                                                                                                    ;
;        2 / 6 vertice(s) have been erased from "289"                                                                                               ;
;        2 / 7 vertice(s) have been erased from "28A"                                                                                               ;
;        1 / 5 vertice(s) have been erased from "28B"                                                                                               ;
;       Please, select a point :                                                                                                                    ;
;        /!\ The polyline "289" has been erased !                                                                                                   ;
;        1 / 5 vertice(s) have been erased from "28A"                                                                                               ;
;        1 / 4 vertice(s) have been erased from "28B"                                                                                               ;
;       Please, select a point :                                                                                                                    ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.1.1   |   Exclude locked layers from selection (with "_:L" option) to avoid errors                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.1.0   |   Create a temporary layer in order to make sure the temp-circles are visible                                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.0.1   |   Add a loop for the creation of the selection set to avoid errors. Add a warning on the help text for "clean" option            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v4.0.0   |   Add the option "Clean" to delete all points between 2 specified points on a single polyline                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Convert (grcircle) and (2D-Point) as dependent functions to be used in another programs if needed and fix the issues for       | ;
; |            |   LWPOLYLINE with their bulges and segment's width                                                                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:POLYDELPOINT ( / *error* Poly-DeletePoints OverkillPoly Pdp-princ doc dist jsel pt i ent hdl obj n lst)
  (defun *error* (msg)
    (setvar "CLAYER" layer)
    (command-s "_-LAYDEL" "_Name" tmp "" "_Yes")
    (vla-EndUndoMark doc)
    (princ msg)
  )
  (defun Poly-DeletePoints (obj fun / LWPOLY_RemoveNthPoints obn d pts n i ind rmn sup r)
    (defun LWPOLY_RemoveNthPoints (lst n / pos pre pts tmp i rtn)
      (setq
        pos (vl-position (assoc 10 lst) lst)
        pre (sublist lst 1 pos)
        pts (sublist (reverse (cdr (reverse lst))) (1+ pos) nil)
        tmp (divlist pts 5)
        i -1
        n (cond ((listp n) n) ((list n)))
      )
      (foreach pt tmp
        (cond
          ( (member (setq i (1+ i)) n))
          ( (member (1+ i) n) (setq rtn (cons (subst '(42 . 0.0) (assoc 42 pt) pt) rtn)))
          ( (setq rtn (cons pt rtn)))
        )
      )
      (if (< 1 (length rtn))
        (entmod (append (subst (cons 90 (1- (length rtn))) (assoc 90 pre) pre) (apply 'append (reverse rtn)) (list (last lst))))
      )
    )
    
    (setq obn (vla-get-ObjectName obj))
    (cond
      ( (= "AcDb3dPolyline" obn) (setq d 3))
      ( (= "AcDbPolyline" obn) (setq d 2))
    )
    (if d
      (progn
        (setq pts (vlax-get obj 'Coordinates))
        (setq pts (divlist pts d))
        (setq n (length pts) i -1)
        (setq ind (mapcar 'eval (mAtom '(setq i (1+ i)) (length pts) 'list)))
        (setq rmn (vl-remove-if fun pts))
        (setq pts (mapcar 'cons ind pts))
        (setq sup (vl-remove-if '(lambda (x) (member (cdr x) rmn)) pts))
        (cond
          ( (= n (length rmn)))
          ( (and
              (< 1 (length rmn))
              (cond
                ( (= "AcDb3dPolyline" obn)
                  (vlax-put obj 'Coordinates (apply 'append rmn))
                )
                ( (= "AcDbPolyline" obn)
                  (LWPOLY_RemoveNthPoints (entget (vlax-VLA-Object->ename obj)) (mapcar 'car sup))
                )
              )
            )
            sup
          )
          ( (null (vla-delete obj)) obj)
        )
      )
      ( (vla-get-ObjectName obj))
    )
  )
  (defun OverkillPoly (ent pt1 pt2 / obj odr pr1 pr2 pts)
    (setq
      obj (vlax-ename->vla-object ent)
      pt1 (vlax-curve-getClosestPointTo ent pt1)
      pt2 (vlax-curve-getClosestPointTo ent pt2)
      odr (vl-sort (list pt1 pt2) '(lambda (a b) (< (vlax-curve-getDistAtPoint ent a) (vlax-curve-getDistAtPoint ent b))))
      pt1 (car odr)
      pt2 (cadr odr)
      pr1 (vlax-curve-getParamAtPoint ent pt1)
      pr2 (vlax-curve-getParamAtPoint ent pt2)
      pts (Poly-DeletePoints obj '(lambda (p) (<= pr1 (vlax-curve-getParamAtPoint ent p) pr2)))
    )
  )
  (defun Pdp-princ ()
    (cond
      ( (listp lst)
        (princ
          (strcat
            "\n " (itoa (length lst)) " / " (itoa n)
            (LgT
              " vertice(s) have been erased from \""
              " sommet(s) ont été supprimé(s) de \""
              nil
            )
            hdl "\" :"
          )
        )
        (print lst)
      )
      ( (= 'VLA-OBJECT (type lst))
        (ssdel ent jsel)
        (princ
          (LgT
            (strcat "\n /!\\ The polyline \"" hdl "\" has been erased !")
            (strcat "\n /!\\ La polyligne \"" hdl "\" a été supprimée !")
            nil
          )
        )
      )
    )
  )

  (setq tmp "$temp$")
  (layer-get-or-create tmp (CrosshairColor->RGB) "Continuous" 0 0)
  (setq layer (getvar "CLAYER"))
  (setvar "CLAYER" tmp)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dist
    (getkdh
      (quote (getdist msg))
      (LgT
        "\nSpecify the tolerance value for the distance between coordinates and vertex or"
        "\nSpécifiez la tolérance pour la distance entre les coordonnées et les sommets ou"
        nil
      )
      (list 4 (LgT "Clean _Clean" "Epurer _Clean" nil))
      " : "
      (vlax-ldata-get "URBASOLAR" "POLYDELPOINT_Distance" 1.0)
      (LgT
        (strcat
          "\nPOLYDELPOINT : Tolerance value"
          "\nThe tolerance value correspond to the maximum distance between the selected point and a vertex from selected polylines (2D/3D)."
          " Your cursor will be similar to an eraser, every vertex (from selected polylines only) within the circle"
          " (circle radius = tolerance value) will be deleted."
          "\nThe polyline will be erased if only one or less vertex are remaining in its properties."
          "\nOption : Clean"
          "\nYou'll be able to select only one polyline to remove all the vertex between 2 specified points."
          "\n WARNING! If you select an existing vertex from the selected polyline, it'll also be deleted."
        )
        (strcat
          "\nPOLYDELPOINT : Valeur de tolérance"
          "\nLa tolérance correspond à la distance maximale entre le point sélectionné et un sommet des polylignes sélectionnées (2D/3D)."
          " Votre curseur sera similaire à une gomme, chaque sommet (des polylignes sélectionnées uniquement) situé dans le cercle"
          " (rayon du cercle = tolérance) sera supprimé."
          "\nLa polyligne sera supprimée s'il ne reste qu'un ou moins de ses sommets dans ses propriétés."
          "\nOption : Epurer"
          "\nVous pourrez sélectionner une seule polyligne pour supprimer tous les sommets compris entre 2 points spécifiés."
          "\n ATTENTION! Si vous sélectionnez un sommet existant de la polyligne choisie, ce dernier sera également supprimé."
        )
        nil
      )
    )
  )
  (vlax-ldata-put "URBASOLAR" "POLYDELPOINT_Distance" dist)
  (while
    (not
      (setq jsel
        (cond
          ( (= "Clean" dist) (ssget "_+.:E:S:L" '((0 . "LWPOLYLINE,POLYLINE"))))
          ( (ssget "_:L" '((0 . "LWPOLYLINE,POLYLINE"))))
        )
      )
    )
    (princ (LgT "\nPlease select a polyline(s) : " "\nVeuillez sélectionner une/des polyligne(s) : " nil))
  )
  (sssetfirst nil jsel)
  (cond
    ( (= "Clean" dist)
      (setq
        ent (ssname jsel 0)
        hdl (cdr (assoc 5 (entget ent)))
        n (length (get-pt-list ent))
        lst (list (getpoint "\nPoint 1 : ") (getpoint "\nPoint 2 : "))
        lst (OverkillPoly ent (trans (car lst) 1 0) (trans (cadr lst) 1 0))
      )
      (Pdp-princ)
    )
    ( T
      (while (setq pt (grcircle (LgT "\nPlease, select a point : " "\nVeuillez choisir un point : " nil) dist))
        (vla-StartUndoMark doc)
        (setq pt (trans pt 1 0))
        (repeat (setq i (sslength jsel))
          (setq
            ent (ssname jsel (setq i (1- i)))
            hdl (cdr (assoc 5 (entget ent)))
            obj (vlax-ename->VLA-Object ent)
            n (length (get-pt-list ent))
            lst (Poly-DeletePoints obj '(lambda (p) (not (< dist (distance (2D-Point p) (2D-Point pt))))))
          )
          (Pdp-princ)
        )
        (vla-EndUndoMark doc)
      )
    )
  )
  (setvar "CLAYER" layer)
  (command "_-LAYDEL" "_Name" tmp "" "_Yes")
  (princ)
)