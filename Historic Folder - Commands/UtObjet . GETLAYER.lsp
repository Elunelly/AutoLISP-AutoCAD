
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                               --{  GETLAYER  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] GETLAYER []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 08/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Displays the layer's name of a nested object to easily know its layer without having to freeze all Xref's layers for this. If the object if in  ;
;   a block reference or an Xref, it will display the list of all nested block's name and layer until the selected object is stored.                ;                                                                                                                                                ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : While...                                                                                                                        ;
; Step n°1.a        : Ask the user to select a subentity with (getkdh)                                                                              ;
; Step n°1.a.1        : If the user enter "?", displays the help message on command line and ask the question again                                 ;
; Step n°1.b        : If a entity is selected, retrieves its name and layer                                                                         ;
; Step n°1.c        : If the layer is from an Xref, displays the name of Xref and layer in command line (below the question). If not, displays the  ;
;                     name of layer only in command line (next to the question)                                                                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> space                                         | v1.0.0 - 22/02/2021 (Luna)                                                    ;
;   --•  "UtUse" ---> getkdh                                        | v2.0.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtWin" ---> LgT                                           | v1.1.0 - 12/05/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (GETLAYER) returns the name of Xref and layer (if subentity is a nested object from Xref) or just the layer's name for each object  ;
;   selected.                                                                                                                                       ;
;     Ex. :                                                                                                                                         ;
;       Command: GETLAYER                                                                                                                           ;
;       Select a subentity (to know its layer) [?] :                                                                                                ;
;        Block  : Test Deep                                                                                                                         ;
;        Layer  : 0                                                                                                                                 ;
;           Block  : A$C664d27c4                                                                                                                    ;
;           Layer  : 0                                                                                                                              ;
;              Block  : A$Cf643ef82                                                                                                                 ;
;              Layer  : 0                                                                                                                           ;
;                 Object : Attribute                                                                                                                ;
;                 Layer  : 0                                                                                                                        ;
;       Select a subentity (to know its layer) [?] : UBS-000-Limite cadastrale                                                                      ;
;       Select a subentity (to know its layer) [?] :                                                                                                ;
;        Block  : XRef_Leve PA (UBS)                                                                                                                ;
;        Layer  : UBS-900-XREF TOPO                                                                                                                 ;
;           Object : Face                                                                                                                           ;
;           Layer  : GC MNT                                                                                                                         ;
;       Select a subentity (to know its layer) [?] :                                                                                                ;
;        Block  : XRef_Leve PA (UBS)                                                                                                                ;
;        Layer  : UBS-900-XREF TOPO                                                                                                                 ;
;           Xref   : XRef_Leve PA (UBS)                                                                                                             ;
;           Block  : TCPOINT                                                                                                                        ;
;           Layer  : GC Points                                                                                                                      ;
;              Object : Point                                                                                                                       ;
;              Layer  : 0                                                                                                                           ;
;       Select a subentity (to know its layer) [?] :                                                                                                ;
;       Command:                                                                                                                                    ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Use the structure of (nentsel) to dig up into the nested block reference and retrieves every layer of nested objects           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Redesigning the whole program with the english/french version                                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:GETLAYER ( / nlst name ent i layer pos )
  (while
    (and
      (setq nlst
        (getkdh
          (quote (nentsel msg))
          (LgT
            "\nSelect a subentity (to know its layer)"
            "\nSélectionnez un sous-objet (pour connaître son calque)"
            nil
          )
          nil
          " : "
          nil
          (LgT
            (strcat
              "\nGETLAYER : Selection of subentities"
              "\n  When the selected object is not complex (that is, not a 3D polyline, block or Xref), GETLAYER will return the name of the "
              "layer containing this object."
              "\n  If the selected object is complex (3D polyline, block or Xref), GETLAYER will select the deepest non-complex subentity (if the "
              "selected sub-object is in a nested block, a block within a block, for example) and returns the name of the layer containing this "
              "subentity."
            )
            (strcat
              "\nGETLAYER : Sélection des sous-objets"
              "\n  Lorsque l'objet sélectionné n'est pas complexe (ce n'est ni une polyligne 3D, ni un bloc ou une Xref), GETLAYER renverra le nom du "
              "calque contenant cet objet."
              "\n  Si l'objet sélectionné est complexe (polyligne 3D, bloc ou Xref), GETLAYER sélectionnera l'entité non complexe la plus profonde "
              "(si le sous-objet sélectionné est un bloc imbriqué, un bloc dans un bloc, par exemple) et retourne le nom du calque contenant ce "
              "sous-objet."
            )
            nil
          )
        )
      )
      (setq name (car nlst))
      (if (= 4 (length nlst))
        (setq nlst
          (mapcar
            '(lambda (ent)
              (cons
                ent
                (list
                  (cdr (assoc 8 (entget ent)))
                  (cdr (assoc 2 (entget ent)))
                )
              )
             )
            (last nlst)
          )
        )
        (not (setq nlst nil))
      )
      (if (= "ATTRIB" (cdr (assoc 0 (entget name))))
        (setq nlst
          (append
            (list
              (cons
                (setq ent (cdr (assoc 330 (entget name))))
                (list
                  (cdr (assoc 8 (entget ent)))
                  (cdr (assoc 2 (entget ent)))
                )
              )
            )
            nlst
          )
        )
        T
      )
      (setq nlst (cons (cdr (assoc 8 (entget name))) nlst))
    )
    (setq i -1)
    (if (cdr nlst)
      (princ
        (strcat
          (apply
            'strcat
            (mapcar
              '(lambda (x / e p l b)
                (setq
                  e (car x)
                  p (cdr x)
                  l (car p)
                  b (cadr p)
                )
                (strcat
                  "\n" (space (1+ (* 3 (setq i (1+ i)))))
                  (if (setq p (vl-string-search "|" l))
                    (strcat "Xref   : " (substr l 1 p))
                    ""
                  )
                  "\n" (space (1+ (* 3 i)))
                  (LgT "Block  : " "Bloc   : " nil)
                  (if p (substr b (+ 2 p)) b)
                  "\n" (space (1+ (* 3 i)))
                  (LgT "Layer  : " "Calque : " nil)
                  (if p (substr l (+ 2 p)) l)
                )
               )
              (reverse (cdr nlst))
            )
          )
          "\n" (space (1+ (* 3 (setq i (1+ i)))))
          (LgT "Object : " "Objet  : " nil)
          (substr (vla-get-ObjectName (vlax-ename->vla-object name)) 5)
          "\n" (space (1+ (* 3 i)))
          (LgT "Layer  : " "Calque : " nil)
          (if (setq pos (vl-string-position (ascii "|") (setq layer (car nlst)) 0 T))
            (substr layer (+ 2 pos))
            layer
          )
        )
      )
      (princ (car nlst))
    )
  )
  (princ)
)