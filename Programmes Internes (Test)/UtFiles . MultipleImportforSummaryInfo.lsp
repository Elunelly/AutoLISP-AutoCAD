
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                 FICHIER DE SUIVI HISTORIQUE DE LA COMMANDE MISI                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] MISI []-----------------------[]                                           ;
;--- Date of creation       > 17/03/2022                                                                                                            ;
;--- Last modification date > 18/03/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   The purpose of the MISI (= Multiple Import for SummaryInfo) command is to export all the data stored in the drawing properties (Author,         ;
;   Comments, HyperlinkBase, Keywords, Subject and Title) to an external .txt file. The program works on both open and unopened files. Once the     ;
;   .txt file is created, you can import the data recorded in the file to other .dwg files, open or not. It is also possible to use the command     ;
;   only to create the .txt file first, modify the information contained in the file (respecting its initial structure), then use the existing file ;
;   as new data to import.                                                                                                                          ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Retrieves the "LOCALE" system variable value to determine the language used for the program                                     ;
; Step n°2        : Asks the user for the drawing name to export its SummaryInfo datas                                                              ;
; Step n°2.a        : If "Current" (or ENTER as default value), the ActiveDocument will be used as source drawing                                   ;
; Step n°2.b        : If "Browse", opens a file explorer window and lets you select an existing .dwg or .dwt file                                   ;
; Step n°2.a/b.1      : Asks the user for the .txt file location and name that will be used for the export (if you specify an existing file, it     ;
;                       will be overwritten)                                                                                                        ;
; Step n°2.a/b.1.a      : If "Source" (or ENTER as default value), it will be stored in the source drawing folder with the name of the drawing plus ;
;                         "_ExportSummaryInfo.txt"                                                                                                  ;
; Step n°2.a/b.1.b      : If "Browse", opens a file explorer window and lets you select the location and name for a new file, or an existing one    ;
; Step n°2.a/b.1.c      : If "Temporary", it will create a file in "%TEMP%" folder and it will be deleted at the end of the program                 ;
; Step n°2.c        : If "Skip", it will not export the data of an existing .dwg file but only import the data of an existing .txt file             ;
; Step n°2.c.1        : Opens a file explorer window and lets you select an existing .txt file                                                      ;
; Step n°3        : While you specify an existing .dwg file in the file explorer window, it will import the settings stored in the .txt file into   ;
;                   the drawing                                                                                                                     ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "BaStr" ---> GetFolder                                     | v2.0.0 - 18/03/2022 (Luna)                                                    ;
;   --•  "UtFil" ---> ExportSumInfo                                 | v1.0.0 - 17/03/2022 (Luna)                                                    ;
;   --•  "UtFil" ---> ImportSumInfo                                 | v2.0.0 - 18/03/2022 (Luna)                                                    ;
;   --•  "UtFil" ---> OpenDrawingDBX                                | v1.0.0 - ##/##/#### ((gile))                                                  ;
;   --•  "VlCol" ---> GetItem                                       | v1.0.0 - ##/##/#### ((gile))                                                  ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (MISI) returns the number of successfully imported drawing properties in the historic line, with the name of the files used.        ;
;     Ex. :                                                                                                                                         ;
;       MISI                                                                                                                                        ;
;       Please select the source .dwg file [Current/Browse/Skip] <Current> : _B                                                                     ;
;       Please select the .txt file for export [Source/Browse/Temporary] <Source> : _B                                                              ;
;       13 / 13 drawing properties successfully imported...                                                                                         ;
;       |   Source drawing : "Dessin3.dwg"                                                                                                          ;
;       |   Folder         : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA\"                                                                      ;
;       |                                                                                                                                           ;
;       |   Target drawing : "Dessin5.dwg"                                                                                                          ;
;       |   Folder         : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA\Nouveau dossier\"                                                      ;
;       |                                                                                                                                           ;
;       |   TXT file used  : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA\Nouveau dossier\Dessin3_ExportSummaryInfo.txt"                         ;
;       |                                                                                                                                           ;
;       command:                                                                                                                                    ;
;       MISI                                                                                                                                        ;
;       Please select the source .dwg file [Current/Browse/Skip] <Current> :                                                                        ;
;       Please select the .txt file for export [Source/Browse/Temporary] <Source> : _T                                                              ;
;       8 / 8 drawing properties successfully imported...                                                                                           ;
;       |   Source drawing : "Dessin1.dwg"                                                                                                          ;
;       |   Folder         : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA\"                                                                      ;
;       |                                                                                                                                           ;
;       |   Target drawing : "Dessin5.dwg"                                                                                                          ;
;       |   Folder         : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA"                                                                       ;
;       |                                                                                                                                           ;
;       |   TXT file used  : ""C:\Users\luna\AppData\Local\Temp\MISI001.txt"                                                                        ;
;       |                                                                                                                                           ;
;       8 / 8 drawing properties successfully imported...                                                                                           ;
;       |   Source drawing : "Dessin1.dwg"                                                                                                          ;
;       |   Folder         : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA\"                                                                      ;
;       |                                                                                                                                           ;
;       |   Target drawing : "Dessin3.dwg"                                                                                                          ;
;       |   Folder         : "C:\Users\luna\OneDrive\Documents\C3042 - CS PA"                                                                       ;
;       |                                                                                                                                           ;
;       |   TXT file used  : ""C:\Users\luna\AppData\Local\Temp\MISI001.txt"                                                                        ;
;       |                                                                                                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Update of the functions (GetFolder) and (ImportSumInfo), correcting the issue if CustomKey does already exist, displays info   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:MISI (/ getfolder ExportSumInfo ImportSumInfo OpenDrawingDBX GetItem str2lst lg DWG_file TXT_file d Sfolder Tfolder r)
  ;;; Definition of local functions
  (defun getfolder (filename / pos)
    (setq pos (vl-string-position (ascii "\\") filename 0 T))
    (cons (substr filename 1 (1+ pos)) (substr filename (+ pos 2)))
  )
  
  (defun ExportSumInfo (DWG_file TXT_file / *error* doc SumInfo file n odbx key val)
    (defun *error* (msg)
      (if file (close file))
      (and odbx (vlax-release-object doc))
      (princ msg)
    )
    (cond
      ( (null DWG_file)
        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      )
      ( (not
          (and
            (setq doc
              (GetItem
                (vla-get-Documents (vlax-get-acad-object))
                (strcat (vl-filename-base DWG_file) ".dwg")
              )
            )
            (= DWG_file (vla-get-FullName doc))
          )
        )
        (setq
          doc (OpenDrawingDBX DWG_file)
          odbx T
        )
      )
    )
    (setq
      SumInfo (vla-get-SummaryInfo doc)
      n -1
    )
    (if (setq file (open TXT_file "W"))
      (progn
        (foreach p '(Author Comments HyperlinkBase KeyWords Subject Title)
          (write-line
            (strcat (vl-prin1-to-string p) ";" (vl-prin1-to-string (vlax-get SumInfo p)))
            file
          )
        )
        (write-line "" file)
        (repeat (vla-NumCustomInfo SumInfo)
          (vla-GetCustomByIndex SumInfo (setq n (1+ n)) 'key 'val)
          (write-line
            (strcat key ";" (vl-prin1-to-string val))
            file
          )
        )
        (setq file (close file))
        (and odbx (vlax-release-object doc))
        TXT_file
      )
    )
  )

  (defun ImportSumInfo (DWG_file TXT_file / *error* doc SumInfo n i file odbx line lst)
    (defun *error* (msg)
      (if file (close file))
      (and odbx (vlax-release-object doc))
      (princ msg)
    )
    (cond
      ( (null DWG_file)
        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      )
      ( (not
          (and
            (setq doc
              (GetItem
                (vla-get-Documents (vlax-get-acad-object))
                (strcat (vl-filename-base DWG_file) ".dwg")
              )
            )
            (= DWG_file (vla-get-FullName doc))
          )
        )
        (setq
          doc (OpenDrawingDBX DWG_file)
          odbx T
        )
      )
    )
    (setq
      SumInfo (vla-get-SummaryInfo doc)
      n 0
      i 0
    )
    (if (setq file (open TXT_file "R"))
      (progn
        (while
          (and
            (setq line (read-line file))
            (setq lst (mapcar 'read (str2lst line ";")))
            (member (car lst) '(Author Comments HyperlinkBase KeyWords Subject Title))
          )
          (if (/= "" (cadr lst))
            (vlax-put SumInfo (car lst) (cadr lst))
          )
          (setq n (1+ n))
        )
        (while (setq line (read-line file))
          (setq lst (str2lst line ";"))
          (or
            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-AddCustomInfo (list SumInfo (car lst) (read (cadr lst))))))
            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-SetCustomByKey (list SumInfo (car lst) (read (cadr lst))))))
            (setq i (1+ i))
          )
          (setq n (1+ n))
        )
        (setq file (close file))
        (and odbx (vlax-release-object doc))
        (cons (- n i) n)
      )
    )
  )
  
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
  
  (defun GetItem (col name / obj)
    (vl-catch-all-apply
      (function (lambda () (setq obj (vla-item col name))))
    )
    obj
  )
  
  (defun str2lst (str sep / pos)
    (if (setq pos (vl-string-search sep str))
      (cons
        (substr str 1 pos)
        (str2lst (substr str (+ (strlen sep) pos 1)) sep)
      )
      (list str)
    )
  )
  ;;; End of definition for local functions
  (if (member (getvar "LOCALE") '("FR" "FRA"))
    (setq lg T)
  )
  (initget "Courant Parcourir Ignorer _Current Browse Skip")
  (setq DWG_file
    (getkword
      (if lg
        "\nVeuillez sélectionner le fichier .dwg source [Courant/Parcourir/Ignorer] <Courant> : "
        "\nPlease select the source .dwg file [Current/Browse/Skip] <Current> : "
      )
    )
  )
  (setq DWG_file
    (cond
      ( (= DWG_file "Browse")
        (getfiled
          (if lg
            "Sélection du fichier .dwg source"
            "Selection of source .dwg file"
          )
          (getvar "DWGPREFIX")
          "dwg;dwt"
          0
        )
      )
      ( (= DWG_file "Skip") nil)
      ((strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
    )
  )
  (if DWG_file
    (progn
      (initget "Source Parcourir Temporaire _Source Browse Temporary")
      (setq TXT_file
        (getkword
          (if lg
            "\nVeuillez sélectionner le fichier .txt pour l'export [Source/Parcourir/Temporaire] <Source> : "
            "\nPlease select the .txt file for export [Source/Browse/Temporary] <Source> : "
          )
        )
      )
      (setq TXT_file
        (cond
          ( (or (= TXT_file "Source") (null TXT_file))
            (strcat (substr DWG_file 1 (- (strlen DWG_file) 4)) "_ExportSummaryInfo.txt")
          )
          ( (= TXT_file "Browse")
            (getfiled
              (if lg
                "Sélection du fichier .txt d'export"
                "Selection of .txt export file"
              )
              (strcat (substr DWG_file 1 (- (strlen DWG_file) 4)) "_ExportSummaryInfo")
              "txt"
              1
            )
          )
          ( (= TXT_file "Temporary")
            (setq d T)
            (vl-filename-mktemp "MISI.txt")
          )
        )
      )
      (setq Sfolder (getfolder DWG_file))
      (setq Tfolder Sfolder)
      (setq TXT_file (ExportSumInfo DWG_file TXT_file))
    )
    (progn
      (setq TXT_file
        (getfiled
          (if lg
            "Sélection du fichier .txt existant"
            "Selection of existing .txt file"
          )
          (getvar "DWGPREFIX")
          "txt"
          0
        )
      )
      (setq
        Sfolder (cons "..." "...")
        Tfolder (cons (getvar "DWGPREFIX") (getvar "DWGNAME"))
      )
    )
  )
  (if TXT_file
    (while
      (setq filename
        (getfiled
          (if lg
            "Sélection du fichier .dwg cible"
            "Selection of target .dwg file"
          )
          (car Tfolder)
          "dwg;dwt"
          0
        )
      )
      (setq Tfolder (getfolder filename))
      (if
        (and
          (/= filename DWG_file)
          (setq r (ImportSumInfo filename TXT_file))
        )
        (princ
          (if lg
            (strcat
              "\n"
              (itoa (car r))
              " / "
              (itoa (cdr r))
              " propriétés du dessin importées avec succès..."
              "\n|   Dessin source : \"" (cdr Sfolder) "\""
              "\n|   Dossier       : \"" (car Sfolder) "\""
              "\n|"
              "\n|   Dessin cible  : \"" (cdr Tfolder) "\""
              "\n|   Dossier       : \"" (car Tfolder) "\""
              "\n|"
              "\n|   Fichier TXT   : \"" TXT_file "\""
              "\n|"
            )
            (strcat
              "\n"
              (itoa (car r))
              " / "
              (itoa (cdr r))
              " drawing properties successfully imported..."
              "\n|   Source drawing : \"" (cdr Sfolder) "\""
              "\n|   Folder         : \"" (car Sfolder) "\""
              "\n|"
              "\n|   Target drawing : \"" (cdr Tfolder) "\""
              "\n|   Folder         : \"" (car Tfolder) "\""
              "\n|"
              "\n|   TXT file used  : \"" TXT_file "\""
              "\n|"
            )
          )
        )
      )
    )
  )
  (if d (vl-file-delete TXT_file))
  (princ)
)