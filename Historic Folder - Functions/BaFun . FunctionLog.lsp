
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                             --{  FunctionLog  }--                                                             | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                        []-----------------------[] FunctionLog []-----------------------[]                                        ;
;--- Date of creation       > 11/07/2022                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaFun"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the historic lines displayed by a function into the LogFile an turned it into a list. Each string corresponds to a line.              ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (FunctionLog) have 1 argument(s) :                                                                                                   ;
;   --•  fun                    > is a quoted function expression (use the function (quote) and not the ')                                          ;
;     (type fun) = 'LST                         | Ex. : (quote (dumpallproperties (entlast))), (quote (vlax-dump-object name T)), ...               ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtFil" ---> read-file                                     | v1.0.1 - 06/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 11/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (FunctionLog) returns a list of strings for each lines in the LOGFILE.                                                             ;
;     Ex. : (FunctionLog (quote (vlax-dump-object vnam T))) returns                                                                                 ;
;       (                                                                                                                                           ;
;          "[ AutoCAD - Mon Jul 11 15:47:47 2022  ]----------------------------------------"                                                        ;
;          "; IAcadLWPolyline: Interface AutoCAD Lightweight Polyline"                                                                              ;
;          "; Property values:"                                                                                                                     ;
;          ";   Application (RO) = #<VLA-OBJECT IAcadApplication 00007ff6cd6b4e60>"                                                                 ;
;          ";   Area (RO) = 0.0"                                                                                                                    ;
;          ";   Closed = 0"                                                                                                                         ;
;          ";   ConstantWidth = 0.0"                                                                                                                ;
;          ";   Coordinate = ...Indexed contents not shown..."                                                                                      ;
;          ";   Coordinates = (172.824 128.558 230.955 173.089)"                                                                                    ;
;          ";   Document (RO) = #<VLA-OBJECT IAcadDocument 000001d0163c9408>"                                                                       ;
;          ";   Elevation = 0.0"                                                                                                                    ;
;          ";   EntityTransparency = \"ByLayer\""                                                                                                   ;
;          ";   Handle (RO) = \"18B\""                                                                                                              ;
;          ";   HasExtensionDictionary (RO) = 0"                                                                                                    ;
;          ";   Hyperlinks (RO) = #<VLA-OBJECT IAcadHyperlinks 000001d0297536e8>"                                                                   ;
;          ";   Layer = \"0\""                                                                                                                      ;
;          ";   Length (RO) = 73.2272"                                                                                                              ;
;          ";   Linetype = \"ByLayer\""                                                                                                             ;
;          ";   LinetypeGeneration = 0"                                                                                                             ;
;          ";   LinetypeScale = 1.0"                                                                                                                ;
;          ";   Lineweight = -1"                                                                                                                    ;
;          ";   Material = \"ByLayer\""                                                                                                             ;
;          ";   Normal = (0.0 0.0 1.0)"                                                                                                             ;
;          ";   ObjectID (RO) = 42"                                                                                                                 ;
;          ";   ObjectName (RO) = \"AcDbPolyline\""                                                                                                 ;
;          ";   OwnerID (RO) = 43"                                                                                                                  ;
;          ";   PlotStyleName = \"ByLayer\""                                                                                                        ;
;          ";   Thickness = 0.0"                                                                                                                    ;
;          ";   TrueColor = #<VLA-OBJECT IAcadAcCmColor 000001d029752c60>"                                                                          ;
;          ";   Visible = -1"                                                                                                                       ;
;          "; Methods supported:"                                                                                                                   ;
;          ";   AddVertex (2)"                                                                                                                      ;
;          ";   ArrayPolar (3)"                                                                                                                     ;
;          ";   ArrayRectangular (6)"                                                                                                               ;
;          ";   Copy ()"                                                                                                                            ;
;          ";   Delete ()"                                                                                                                          ;
;          ";   Explode ()"                                                                                                                         ;
;          ";   GetBoundingBox (2)"                                                                                                                 ;
;          ";   GetBulge (1)"                                                                                                                       ;
;          ";   GetExtensionDictionary ()"                                                                                                          ;
;          ";   GetWidth (3)"                                                                                                                       ;
;          ";   GetXData (3)"                                                                                                                       ;
;          ";   Highlight (1)"                                                                                                                      ;
;          ";   IntersectWith (2)"                                                                                                                  ;
;          ";   Mirror (2)"                                                                                                                         ;
;          ";   Mirror3D (3)"                                                                                                                       ;
;          ";   Move (2)"                                                                                                                           ;
;          ";   Offset (1)"                                                                                                                         ;
;          ";   Rotate (2)"                                                                                                                         ;
;          ";   Rotate3D (3)"                                                                                                                       ;
;          ";   ScaleEntity (2)"                                                                                                                    ;
;          ";   SetBulge (2)"                                                                                                                       ;
;          ";   SetWidth (3)"                                                                                                                       ;
;          ";   SetXData (2)"                                                                                                                       ;
;          ";   TransformBy (1)"                                                                                                                    ;
;          ";   Update ()"                                                                                                                          ;
;       )                                                                                                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun FunctionLog (fun / *error* filename lmf cme nom lst)
  (defun *error* (msg)
    (setvar "LOGFILEMODE" lmf)
    (setvar "CMDECHO" cme)
    (setvar "NOMUTT" nom)
    (princ msg)
    nil
  )

  (and
    (setq filename (getvar "LOGFILENAME"))
    (setq lmf (getvar "LOGFILEMODE"))
    (setq cme (getvar "CMDECHO"))
    (setq nom (getvar "NOMUTT"))
    (setvar "LOGFILEMODE" 0)
    (cond ((vl-file-delete filename)) (T))
    (setvar "CMDECHO" 0)
    (setvar "NOMUTT" 1)
    (setvar "LOGFILEMODE" 1)
    (cond ((eval fun)) (T))
    (princ)
    (setvar "LOGFILEMODE" 0)
    (setvar "NOMUTT" nom)
    (setvar "CMDECHO" cme)
    (setq filename (getvar "LOGFILENAME"))
    (setvar "LOGFILEMODE" lmf)
    (setq lst (read-file filename))
  )
  lst
)