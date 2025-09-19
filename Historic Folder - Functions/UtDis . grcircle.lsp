
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  grcircle  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] grcircle []-----------------------[]                                          ;
;--- Date of creation       > 12/08/2022                                                                                                            ;
;--- Last modification date > 16/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.1.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Display a circle + hatch below the cursor for more visibility for area of selection.                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (grcircle) have 2 argument(s) :                                                                                                      ;
;   --•  msg                    > the message to display while moving the cursor until the user select a point                                      ;
;     (type msg) = 'STR                         | Ex. : "\nPlease, select a point : ", "", nil, ...                                                 ;
;   --•  r                      > correspond to the radius of the circle (if radius < 0, use (getpoint) instead)                                    ;
;     (type r) = 'REAL                          | Ex. : 0.0, 1, 5.156, -9, ...                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaAri" ---> LM:OLE->RGB                                   | v1.4.0 - 19/06/2014 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.3.0 - 16/09/2022 (Luna)                                                    ;
;   --•  "BaAri" ---> CrosshairColor->RGB                           | v1.0.0 - 12/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (grcircle) returns the selected 3D point or nil (if keyboard inputs instead).                                                      ;
;     Ex. : (grcircle "\nSelect a point :" 1.0) returns (19.2571 8.49208 0.0), nil otherwise                                                        ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.1.0   |   Disable "OSMODE" if r > 0 to avoid the delay of calculation for each osnap possibilities while (grread) is running             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Use the (c:test2) from GrSnapDemo.lsp to add the LeeMac's functions (LM:grsnap:snapfunction), (LM:grsnap:parsepoint) and       | ;
; |            |   (LM:grsnap:snapmode) to handle the osnap functionnalities and the keyboard entries during (grread)                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Add an hatch inside the circle and check if radius > 0 to create the objects and use (grread) or use (getpoint) instead        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun grcircle (msg r / *error* CrosshairColor->RGB osf osm omd str ce ms pt ci ho co cr gr gd tmp)
  (defun *error* (msg)
    (if ci (vla-delete ci))
    (if ho (vla-delete ho))
    (redraw)
    (if omd (setvar "OSMODE" omd))
    (if ce (setvar "CMDECHO" ce))
    (princ msg)
  )
  (defun CrosshairColor->RGB (/ cr oc )
    (setq
      cr (vla-get-ModelCrosshairColor (vla-get-Display (vla-get-Preferences (vlax-get-acad-object))))
      oc (vlax-variant-value (vlax-variant-change-type cr vlax-vbLong))
    )
    (LM:OLE->RGB oc)
  )

  (if (< 0 r)
    (progn
      (setq
        osf (LM:grsnap:snapfunction)
        omd (getvar "OSMODE")
        str ""
        msg (cond (msg) (""))
      )
      (if (zerop (logand 16384 omd))
        (setvar "OSMODE" (+ omd 16384))
      )
      (setq
        osm (getvar "OSMODE")
        ce (getvar "CMDECHO")
        ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
        pt (vlax-3D-point '(0.0 0.0 0.0))
        ci (vla-addCircle ms pt r)
        ho (vla-AddHatch ms acHatchPatternTypePredefined "SOLID" :vlax-false acHatchObject)
        co (vla-get-TrueColor ci)
        cr (CrosshairColor->RGB)
      )
      (setvar "CMDECHO" 0)
      (vlax-invoke ho 'AppendOuterLoop (list ci))
      (vla-Evaluate ho)
      (apply 'vla-setRGB (cons co cr))
      (vla-put-TrueColor ci co)
      (vla-put-TrueColor ho co)
      (vlax-put ci 'EntityTransparency "0")
      (vlax-put ho 'EntityTransparency "50")
      (vlax-put ci 'LineType "Continuous")
      (setvar "CMDECHO" ce)
      (princ msg)
      (while
        (progn
          (setq
            gr (grread T 15 0)
            gd (cadr gr)
            gr (car gr)
          )
          (cond
            ( (or (= 5 gr) (= 3 gr))
              (redraw)
              (setq pt (vlax-3D-Point (trans (setq gd (osf gd osm)) 1 0)))
              (vla-move ho (vla-get-Center ci) pt)
              (vla-move ci (vla-get-Center ci) pt)
              gd
              (= 5 gr)
            )
            ( (= 2 gr)
              (cond
                ( (= 6 gd)
                  (if (zerop (logand 16384 (setq osm (setvar "OSMODE" (boole 6 16384 (getvar "OSMODE"))))))
                    (princ (LgT "\n<Osnap on>" "\n<Accrobj actif>" nil))
                    (princ (LgT "\n<Osnap off>" "\n<Accrobj inactif>" nil))
                  )
                  (princ msg)
                )
                ( (= 8 gd)
                  (if (< 0 (strlen str))
                    (progn
                      (princ "\010\040\010")
                      (setq str (substr str 1 (1- (strlen str))))
                    )
                  )
                  T
                )
                ( (< 32 gd 127)
                  (setq str (strcat str (princ (chr gd))))
                )
                ( (member gd '(13 32))
                  (cond
                    ( (= "" str) nil)
                    ( (setq gd (LM:grsnap:parsepoint pt str))
                      (setq osm 16384)
                      nil
                    )
                    ( (setq tmp (LM:grsnap:snapmode str))
                      (setq
                        osm tmp
                        str ""
                      )
                    )
                    ( (setq str "")
                      (princ (LgT "\n2D / 3D Point Required." "\nPoint 2D / 3D requis." nil))
                      (princ msg)
                    )
                  )
                )
              )
            )
          )
        )
      )
      (setq ci (vla-delete ci))
      (setq ho (vla-delete ho))
      (redraw)
      (setvar "OSMODE" omd)
      (if (listp gd) gd)
    )
    (apply 'getpoint (list msg))
  )
)