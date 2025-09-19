
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                       --{  Excel:IsOpenedWorkBook  }--                                                        | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                  []-----------------------[] Excel:IsOpenedWorkBook []-----------------------[]                                   ;
;--- Date of creation       > 26/06/2017                                                                                                            ;
;--- Last modification date > 19/09/2022                                                                                                            ;
;--- Author                 > Ranjit Singh/Luna                                                                                                     ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Detect if a file is already opened in Excel application or not.                                                                                 ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Excel:IsOpenedWorkBook) have 1 argument(s) :                                                                                        ;
;   --•  filepath               > is the full path of file to be search in Excel application                                                        ;
;     (type filepath) = 'STR                    | Ex. : "C:\\Users\\lphilip\\Documents\\Drawing1.xls", ...                                          ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Excel:IsOpenedWorkBook) returns a dotted pair list with the head corresponding to 'filepath (strcase) and the tail corresponding  ;
;   to the VLA-Object of the related workbook, nil otherwise.                                                                                       ;
;     Ex. : (Excel:IsOpenedWorkBook (getfiled "Searching file" "" "csv" 0)) returns                                                                 ;
;           ("C:\\Users\\lphilip\\Documents\\Workbook1.csv" . #<VLA-OBJECT _Workbook 000002b6218f5098>) if opened, nil otherwise.                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Modify the return value to be a dotted pair list with the full path string and the VLA-Object of workbook if found             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Excel:IsOpenedWorkBook (filepath / XLSobj wb lst)
  (setq XLSobj (vlax-get-or-create-object "Excel.application"))
  (vlax-for wb (vlax-get XLSobj 'WorkBooks)
    (setq lst (cons (cons (strcase (vlax-get wb 'FullName)) wb) lst))
  )
  (vlax-release-object XLSobj)
  (assoc (strcase filepath) lst)
)