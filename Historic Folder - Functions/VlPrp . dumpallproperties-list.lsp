
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  dumpallproperties->list  }--                                                       | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] dumpallproperties->list []-----------------------[]                                  ;
;--- Date of creation       > 10/12/2021                                                                                                            ;
;--- Last modification date > 10/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;                                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (dumpallproperties->list) have 1 argument(s) :                                                                                       ;
;   --•  name                   > is the entity name of the object we want to get all their properties name and value                               ;
;     (type name) = 'ENAME                      | Ex. : (car (entsel)), ...                                                                         ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> Wildcard-trim                                 | v1.0.0 - 10/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 10/12/2021 (Luna)                                                    ;
;   --•  "BaStr" ---> get-key&value                                 | v1.0.0 - 10/12/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (dumpallproperties->list) returns a list of doted pair lists wich each doted pair corresponds to (PropertyName . PropertyValue).   ;
;     Ex. : (dumpallproperties->list (car (entsel))) returns (see below) for a 2 points "LWPOLYLINE"                                                ;
;           ( ("Annotative" . "")                                                                                                                   ;
;             ("AnnotativeScale" . "")                                                                                                              ;
;             ("Area" . 0.0)                                                                                                                        ;
;             ("BlockId" . <Nom d'entité: 18b0eeb29f0>)                                                                                             ;
;             ("CastShadows" . 0)                                                                                                                   ;
;             ("ClassName" . "")                                                                                                                    ;
;             ("Closed" . 0)                                                                                                                        ;
;             ("CollisionType" . 1)                                                                                                                 ;
;             ("Color" . "256")                                                                                                                     ;
;             ("ConstantWidth" . 0.0)                                                                                                               ;
;             ("Elevation" . 0.0)                                                                                                                   ;
;             ("EndParam" . 1.0)                                                                                                                    ;
;             ("EndPoint/X" . "")                                                                                                                   ;
;             ("EndPoint/Y" . "")                                                                                                                   ;
;             ("EndPoint/Z" . "")                                                                                                                   ;
;             ("ExtensionDictionary" . "0")                                                                                                         ;
;             ("Handle" . "234")                                                                                                                    ;
;             ("HasBulges" . 0)                                                                                                                     ;
;             ("HasFields" . 0)                                                                                                                     ;
;             ("HasSaveVersionOverride" . 0)                                                                                                        ;
;             ("HasWidth" . 0)                                                                                                                      ;
;             ("Hyperlinks" . "")                                                                                                                   ;
;             ("IsA" . "AcDbPolyline")                                                                                                              ;
;             ("IsAProxy" . 0)                                                                                                                      ;
;             ("IsCancelling" . 0)                                                                                                                  ;
;             ("IsEraseStatusToggled" . 0)                                                                                                          ;
;             ("IsErased" . 0)                                                                                                                      ;
;             ("IsModified" . 0)                                                                                                                    ;
;             ("IsModifiedGraphics" . 0)                                                                                                            ;
;             ("IsModifiedXData" . 0)                                                                                                               ;
;             ("IsNewObject" . 0)                                                                                                                   ;
;             ("IsNotifyEnabled" . 0)                                                                                                               ;
;             ("IsNotifying" . 0)                                                                                                                   ;
;             ("IsObjectIdsInFlux" . 0)                                                                                                             ;
;             ("IsOnlyLines" . 1)                                                                                                                   ;
;             ("IsPeriodic" . 0)                                                                                                                    ;
;             ("IsPersistent" . 1)                                                                                                                  ;
;             ("IsPlanar" . 1)                                                                                                                      ;
;             ("IsReadEnabled" . 1)                                                                                                                 ;
;             ("IsReallyClosing" . 1)                                                                                                               ;
;             ("IsTransactionResident" . 0)                                                                                                         ;
;             ("IsUndoing" . 0)                                                                                                                     ;
;             ("IsWriteEnabled" . 0)                                                                                                                ;
;             ("LayerId" . <Nom d'entité: 18b0eeb2900>)                                                                                             ;
;             ("Length" . 4.04537)                                                                                                                  ;
;             ("LineWeight" . -1)                                                                                                                   ;
;             ("LinetypeId" . <Nom d'entité: 18b0eeb2950>)                                                                                          ;
;             ("LinetypeScale" . 1.0)                                                                                                               ;
;             ("LocalizedName" . "Polyligne")                                                                                                       ;
;             ("MaterialId" . <Nom d'entité: 18b0eeb2de0>)                                                                                          ;
;             ("MergeStyle" . 1)                                                                                                                    ;
;             ("Normal/X" . 0.0)                                                                                                                    ;
;             ("Normal/Y" . 0.0)                                                                                                                    ;
;             ("Normal/Z" . 1.0)                                                                                                                    ;
;             ("ObjectId" . <Nom d'entité: 18b0eeb0d40>)                                                                                            ;
;             ("OwnerId" . <Nom d'entité: 18b0eeb29f0>)                                                                                             ;
;             ("Plinegen" . 0)                                                                                                                      ;
;             ("PlotStyleName" . "ParCouleur")                                                                                                      ;
;             ("ReceiveShadows" . 0)                                                                                                                ;
;             ("ShadowDisplay" . "")                                                                                                                ;
;             ("StartParam" . 0.0)                                                                                                                  ;
;             ("StartPoint/X" . "")                                                                                                                 ;
;             ("StartPoint/Y" . "")                                                                                                                 ;
;             ("StartPoint/Z" . "")                                                                                                                 ;
;             ("Thickness" . 0.0)                                                                                                                   ;
;             ("Transparency" . -1)                                                                                                                 ;
;             ("Vertices"                                                                                                                           ;
;                 0                                                                                                                                 ;
;                 (                                                                                                                                 ;
;                     ("Bulge" . 0.0)                                                                                                               ;
;                     ("ClassName" . "")                                                                                                            ;
;                     ("EndWidth" . 0.0)                                                                                                            ;
;                     ("Id" . 0)                                                                                                                    ;
;                     ("IsA" . "AcRxBoxedValueOnStack")                                                                                             ;
;                     ("LocalizedName" . "")                                                                                                        ;
;                     ("Position/X" . 13.2926)                                                                                                      ;
;                     ("Position/Y" . 9.9261)                                                                                                       ;
;                     ("StartWidth" . 0.0)                                                                                                          ;
;                 )                                                                                                                                 ;
;                 1                                                                                                                                 ;
;                 (                                                                                                                                 ;
;                     ("Bulge" . 0.0)                                                                                                               ;
;                     ("ClassName" . "")                                                                                                            ;
;                     ("EndWidth" . 0.0)                                                                                                            ;
;                     ("Id" . 0)                                                                                                                    ;
;                     ("IsA" . "AcRxBoxedValueOnStack")                                                                                             ;
;                     ("LocalizedName" . "")                                                                                                        ;
;                     ("Position/X" . 16.0547)                                                                                                      ;
;                     ("Position/Y" . 12.8817)                                                                                                      ;
;                     ("StartWidth" . 0.0)                                                                                                          ;
;                 )                                                                                                                                 ;
;             )                                                                                                                                     ;
;             ("Visible" . 0)                                                                                                                       ;
;           )                                                                                                                                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun dumpallproperties->list (name / *error* get-key&value filename lmf cme file c n k-p line lst sub tmp i dap)
  (defun *error* (msg)
    (if file
      (setq file (close file))
    )
    (if lmf
      (setvar "LOGFILEMODE" lmf)
      (setvar "LOGFILEMODE" 0)
    )
    (if cme
      (setvar "CMDECHO" cme)
      (setvar "CMDECHO" 1)
    )
    (princ msg)
  )

  (defun get-key&value (l s p name a / key value)
    (cons
      (setq key (substr l s (- (vl-string-search p l) s)))
      (if
        (not
          (or
            (vl-catch-all-error-p (setq value (vl-catch-all-apply 'getpropertyvalue (append (list name) a (list key)))))
            (null value)
          )
        )
        value
        (if
          (or
            (not (wcmatch l "* = *"))
            (= "Failed to get value" (setq value (substr l (+ 1 (strlen " = ") (vl-string-search " = " l)))))
          )
          ""
          value
        )
      )
    )
  )

  (setq
    filename (getvar "LOGFILENAME")
    lmf (getvar "LOGFILEMODE")
    cme (getvar "CMDECHO")
  )
  (vl-file-delete filename)
  (setvar "CMDECHO" 0)
  (setvar "LOGFILEMODE" 1)
  (dumpallproperties name)
  (setvar "LOGFILEMODE" 0)
  (setvar "CMDECHO" 1)
  (setq
    filename (getvar "LOGFILENAME")
    file (open filename "R")
    c 1
    n 5
    k-p "(type:"
  )
  (while (setq line (read-line file))
    (setq lst (cons line lst))
  )
  (close file)
  (setq lst (vl-remove-if '(lambda (l) (= l "")) (reverse lst)))
  (while 
    (and
      lst
      (not (wcmatch (car lst) "*Begin dumping object*"))
    )
    (setq lst (cdr lst))
  )
  (setq lst (reverse (cdr lst)))
  (while
    (and
      lst
      (not (wcmatch (car lst) "End object dump"))
    )
    (setq lst (cdr lst))
  )
  (setq lst (reverse (cdr lst)))
  (while (and lst (setq l (car lst)))
    (setq
      dap
        (cons
          (cond
            ( (and
                (not (wcmatch l "* = *"))
                (wcmatch (cadr lst) "Item #*:")
              )
              (cons
                (setq sub nil l (substr l c (- (vl-string-search k-p l) c)))
                (while (wcmatch (cadr lst) "Item #*:")
                  (setq sub
                    (append sub
                      (list
                        (setq tmp nil i (atoi (vl-string-trim (apply 'strcat (wildcard-trim "~#")) (car (setq lst (cdr lst))))))
                        (while (wcmatch (cadr lst) "   ?*")
                          (setq tmp (append tmp (list (get-key&value (car (setq lst (cdr lst))) n k-p name (list l i)))))
                        )
                      )
                    )
                  )
                )
              )
            )
            ((get-key&value l c k-p name nil))
          )
          dap
        )
      lst (cdr lst)
    )
  )
  (reverse dap)
)