
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  read-file  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] read-file []-----------------------[]                                         ;
;--- Date of creation       > 27/12/2021                                                                                                            ;
;--- Last modification date > 06/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFil"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Reads the file passed as an argument and returns a list of strings for each line of the document.                                               ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (read-file) have 1 argument(s) :                                                                                                     ;
;   --•  filename               > is the name of the file you want to read. The file name can be either the full name or the relative name, if the  ;
;                               file is present in a search path supported by AutoCAD (see Options).                                                ;
;     (type filename) = 'STR                    | Ex. : "acaddoc.lsp", "C:\\Users\\lphilip\\Documents\\387 Disneyland_20211203 ASC_v5.pdf", ...     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaErr" ---> *error*                                       | v1.0.1 - 06/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (read-file) returns the list with each string representing a line of the file, nil if the file cannot be found.                    ;
;     Ex. : (read-file (getfiled "Select a file..." "" "" 0)) returns                                                                               ;
;     ("Dictionaries in drawing \"Dessin2\" :" "ACAD_CIP_PREVIOUS_PRODUCT_INFO" "ACAD_COLOR" "ACAD_DETAILVIEWSTYLE" "   Metric50" "ACAD_GROUP"      ;
;      "ACAD_LAYOUT" "   Model" "   Pr•sentation1" "   Pr•sentation2" "ACAD_MATERIAL" "   ByBlock" "   ByLayer" "   Global" "ACAD_MLEADERSTYLE"     ;
;      "   Annotatif" "   Standard" "ACAD_MLINESTYLE" "   Standard" "ACAD_PLOTSETTINGS" "ACAD_PLOTSTYLENAME" "   Normal" "ACAD_SCALELIST" "   A0"   ;
;      "   A1" "   A2" "   A3" "   A4" "   A5" "   A6" "   A7" "   A8" "   A9" "   B0" "   B1" "   B2" "   B3" "   B4" "   B5" "   B6"              ;
;      "ACAD_SECTIONVIEWSTYLE" "   Metric50" "ACAD_TABLESTYLE" "   Standard" "ACAD_VISUALSTYLE" "   2dWireframe" "   Basic" "   Brighten"           ;
;      "   ColorChange" "   Conceptual" "   Dim" "   EdgeColorOff" "   Facepattern" "   Flat" "   FlatWithEdges" "   Gouraud" "   GouraudWithEdges" ;
;      "   Hidden" "   JitterOff" "   Linepattern" "   OverhangOff" "   Realistic" "   Shaded" "   Shaded with edges" "   Shades of Gray"           ;
;      "   Sketchy" "   Thicken" "   Wireframe" "   X-Ray" "AcDbVariableDictionary" "   CANNOSCALE" "   CENTEREXE" "   CENTERLTYPEFILE"             ;
;      "   CMLEADERSTYLE" "   CTABLESTYLE" "   CVIEWDETAILSTYLE" "   CVIEWSECTIONSTYLE" "   DIMASSOC" "   HIDETEXT" "   LAYEREVAL" "   LAYERNOTIFY" ;
;      "PVcase" "   Settings_BlocksDataTemplatesContainer" "   Settings_CivilAnalysis" "   Settings_Electrical" "   Settings_Grading"               ;
;      "   Settings_GradingInstant" "   Settings_kWPCalculation" "   Settings_Other" "   Settings_ParksData" "   Settings_ParksDataEastWest"        ;
;      "   Settings_ParksDataFixed" "   Settings_ParksDataTracker" "   Settings_ShadingData" "   Settings_StringingColors"                          ;
;      "   Settings_StringingData" "   Settings_TerrainMeshData" "   Settings_TopographyData" "   Settings_UnitsData" "   Settings_Vegetation")     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Check if filename is not null and return nil if an error occured                                                               | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun read-file (filename / *error* file lst line)
  (defun *error* (msg)
    (if file (close file))
    (princ msg)
    nil
  )
  (if
    (and
      filename
      (setq filename (findfile filename))
      (setq file (open filename "R"))
    )
    (progn
      (while (setq line (read-line file))
        (setq lst (cons line lst))
      )
      (setq file (close file))
    )
  )
  (reverse lst)
)