
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                 AUTOLOADING OF SPECIFIC APPLICATIONS AND MESSAGE'S PROMPTING FOR EACH DRAWING                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                                   []-----------------------------------------[]                                                   ;

;--- This function loads the extended functions that implements ActiveX and AutoCAD reactor support for AutoLISP, and also provide ActiveX utility  ;
;--- and data conversion, dictionary handling, and curve measurement functions. If the extensions are already loaded, (vl-load-com) does nothing    ;
(vl-load-com)                                                       ;--•  Loads the extended AutoLISP functions related to ActiveX support          ;

;                                                   []-----------------------------------------[]                                                   ;

;--- These lines are temporary. Deletes the useless dictionary's entries from "URBASOLAR", if they're found in the drawing                          ;
(vlax-ldata-delete "URBASOLAR" "FORCEDLANGUAGE")                    ;--•  Delete the entry "FORCEDLANGUAGE" from "URBASOLAR" dictionary             ;
(vlax-ldata-delete "URBASOLAR" "LOADEDPROGLIST")                    ;--•  Delete the entry "LOADEDPROGLIST" from "URBASOLAR" dictionary             ;

;                                                   []-----------------------------------------[]                                                   ;

;--- In order to know how long the LISP file takes to load all the functions and commands in the current drawing, set the '$time$' variable         ;
(setq $time$ (getvar "MILLISECS"))                                  ;--•  Start the time calculator variable '$time$' to the current value          ;

;                                                   []-----------------------------------------[]                                                   ;

;--- To have access to the Julian conversion's functions before the execution of this file, load the file "Julian.lsp" at the begining              ;
(load "Julian.lsp")                                                 ;--•  Load the ExpressTools file "Julian.lsp" before, to access its functions   ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This variable $lst$ is set to get all the function and command's name loaded by the LISP file. So it's needed to set it as global variable and ;
;--- reset it after each reload of the LISP file                                                                                                    ;
(setq $lst$ '())                                                    ;--•  Reset the $lst$ global variable for each load of .lsp file                ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This variable $k$ is set as a binary integer value, 1 for function's section and 2 for command's section                                       ;
(setq $k$ nil)                                                      ;--•  Reset the $k$ global variable for each load of .lsp file                  ;

;                                                   []-----------------------------------------[]                                                   ;

;--- Sets the default value of "SHOWLSPVERSION" environment variable to 6 wich means the programs version will be add in the list                   ;
(if (null (getenv "SHOWLSPVERSION"))                                ;--•  Checks if the drawing have an environment variable named "SHOWLSPVERSION" ;
  (setenv "SHOWLSPVERSION" "6")                                     ;--•  If :False, set the environment variable "SHOWLSPVERSION" to 6             ;
) 

;                                                   []-----------------------------------------[]                                                   ;

;--- Sets the default value of "FORCEDLANGUAGE" environment variable to 0 wich means the programs will use the language depending on the value of   ;
;--- "LOCALE" systeme variable                                                                                                                      ;
(if (null (getenv "FORCEDLANGUAGE"))                                ;--•  Checks if the drawing have an environment variable named "FORCEDLANGUAGE" ;
  (setenv "FORCEDLANGUAGE" "0")                                     ;--•  If :False, set the environment variable "FORCEDLANGUAGE" to 0             ;
)                                                                   ;--•  End (if)                                                                  ;

;                                                   []-----------------------------------------[]                                                   ;

;--- Sets the default value of "LOADEDPROGLIST" environment variable to 6 wich means only commands and system variables will be displayed at the    ;
;--- start-up of each drawing                                                                                                                       ;
(if (null (getenv "LOADEDPROGLIST"))                                ;--•  Checks if the drawing have an environment variable named "LOADEDPROGLIST" ;
  (setenv "LOADEDPROGLIST" "6")                                     ;--•  If :False, set the environment variable "LOADEDPROGLIST" to 6             ;
)                                                                   ;--•  End (if)                                                                  ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This unnamed function creates the TextStyle "MonoSpace" if not loaded on the current drawing (necessary for full functionnalities of LineTypes ;
;--- defined in the "UBS_LType.lin" file, with text).                                                                                               ;
((lambda ()                                                         ;--•  Definition and execution of unnamed function (lambda)                     ;
  (if (not (tblsearch "STYLE" "MonoSpace"))                         ;--•  Check if "MonoSpace" Text Style is not in the "STYLE" SymbolTable         ;
    (entmake                                                        ;--•  If :True, adds the "MonoSpace" entity in "AcDbTextStyleTableRecord"       ;
      '(                                                            ;--•  Start (list)                                                              ;
        (0 . "STYLE")                                               ;--•  DXF Code 0    = Text string indicating the entity type (= "STYLE")        ;
        (100 . "AcDbSymbolTableRecord")                             ;--•  DXF Code 100  = Subclass data marker (= "AcDbSymbolTableRecord")          ;
        (100 . "AcDbTextStyleTableRecord")                          ;--•  DXF Code 100  = Subclass data marker (= "AcDbTextStyleTableRecord")       ;
        (2 . "MonoSpace")                                           ;--•  DXF Code 2    = Name of text style (= "MonoSpace")                        ;
        (70 . 0)                                                    ;--•  DXF Code 70   = Standard flag value (bit-coded values) (= 0)              ;
        (40 . 0.0)                                                  ;--•  DXF Code 40   = Fixed text height, 0 if not fixed (= 0)                   ;
        (41 . 1.0)                                                  ;--•  DXF Code 41   = Width factor (= 1.0)                                      ;
        (50 . 0.0)                                                  ;--•  DXF Code 50   = Oblique angle (= 0.0)                                     ;
        (71 . 0)                                                    ;--•  DXF Code 71   = Text generation flags (= 0)                               ;
        (42 . 2.5)                                                  ;--•  DXF Code 42   = Last height used (= 2.5)                                  ;
        (3 . "monos.ttf")                                           ;--•  DXF Code 3    = Primary font file name (= "monos.ttf")                    ;
        (4 . "")                                                    ;--•  DXF Code 4    = Bigfont file name, blank if none (= "")                   ;
      )                                                             ;--•  End (list)                                                                ;
    )                                                               ;--•  End (entmake)                                                             ;
  )                                                                 ;--•  End (if)                                                                  ;
))                                                                  ;--•  End (lambda)                                                              ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This function named ($pLstAdd$) is necessary to add the functions and commands's name to the $lst$ variable while loading them in drawing file ;
(defun $getenv-list$ ()                                             ;--•  Definition of '$getenv-list$' function with 0 arguments                   ;
  (mapcar                                                           ;--•  Start (mapcar)                                                            ;
    '(lambda (x) (cons x (getenv x)))                               ;--•  Foreach element, construct a doted-pair ("tag" . "value")                 ;
    (vl-remove-if                                                   ;--•  Start (vl-remove-if)                                                      ;
      '(lambda (k)                                                  ;--•  Start (lambda) with 1 argument                                            ;
        (member                                                     ;--•  Start (member)                                                            ;
          k                                                         ;--•  Name of environment variable                                              ;
          '("LastTemplate" "acedChangeCmdEchoWithoutUndo" "PATH" "TaskBar" "ProfileStorage" "InsertUnitsDefTarget" "InsertUnitsDefSource"
            "Measureinit" "DisplayLicenseUIAtLaunchTime" "MeshMaxFaceValue" "JsApiUrl" "ISOLinetype" "ISOHatch" "ANSILinetype"
            "ANSIHatch" "ActUI" "CMLInternetSearchPrefix" "CMLInternetSearchURL" "ToolTipInitialDelay" "FavoriteMaterialLibrary"
          )                                                         ;--•  End (list)                                                                ;
        )                                                           ;--•  End (member)                                                              ;
      )                                                             ;--•  End (lambda)                                                              ;
      (vl-registry-descendents                                      ;--•  Start (vl-registry-descendents)                                           ;
        (strcat                                                     ;--•  Start (strcat)                                                            ;
          "HKEY_CURRENT_USER\\"                                     ;--•  ...                                                                       ;
          (vlax-product-key)                                        ;--•  ...                                                                       ;
          "\\FixedProfile\\General"                                 ;--•  ...                                                                       ;
        )                                                           ;--•  End (strcat)                                                              ;
        ""                                                          ;--•  Empty string to avoid the nil return of (vl-registry-descendents)         ;
      )                                                             ;--•  End (vl-registry-descendents)                                             ;
    )                                                               ;--•  End (vl-remove-if)                                                        ;
  )                                                                 ;--•  End (mapcar)                                                              ;
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This function named ($pLstAdd$) is necessary to add the functions and commands's name to the $lst$ variable while loading them in drawing file ;
(defun $pLstAdd$ (filename / f file line Date Author Version Class ProgName)
  (defun f (l n / s)                                                ;--•  Local definition of 'f' function with 2 arguments                         ;
    (if (wcmatch l (strcat ";--- " n "*"))                          ;--•  Check if 'l' contains a specific pattern at the beginning                 ;
      (setq                                                         ;--•  Start (setq)                                                              ;
        s (substr l (+ 3 (vl-string-search "> " l)))                ;--•  Set 's' as the end of the line, starting after the "> "                   ;
        s (substr s 1 (vl-string-search " " s))                     ;--•  Set 's' as the beginning of 's', ending at the first " " encountered      ;
      )                                                             ;--•  End (setq)                                                                ;
    )                                                               ;--•  End (if)                                                                  ;
  )                                                                 ;--•  End (defun)                                                               ;
  (and                                                              ;--•  Start (and)                                                               ;
    (setq filename (findfile filename))                             ;--•  Check if the specified file can be found and set 'filename' as full path  ;
    (setq file (open filename "R"))                                 ;--•  Open the file and set 'file' as the document object                       ;
    (while                                                          ;--•  Start (while)                                                             ;
      (and                                                          ;--•  Start (and)                                                               ;
        (setq line (read-line file))                                ;--•  Set 'line' as the next line of the opened .lsp file                       ;
        (or                                                         ;--•  Start (or)                                                                ;
          (null Date)                                               ;--•  If 'Date' is not set yet, then :True                                      ;
          (null Author)                                             ;--•  If 'Author' is not set yet, then :True                                    ;
          (null Version)                                            ;--•  If 'Version' is not set yet, then :True                                   ;
          (null Class)                                              ;--•  If 'Class' is not set yet, then :True                                     ;
        )                                                           ;--•  End (or)                                                                  ;
      )                                                             ;--•  End (and)                                                                 ;
      (setq                                                         ;--•  Start (setq)                                                              ;
        Date (cond (Date) ((f line "Last")))                        ;--•  Set 'Date' as the value got in the opened .lsp file                       ;
        Author (cond (Author) ((f line "Author")))                  ;--•  Set 'Author' as the value got in the opened .lsp file                     ;
        Version (cond (Version) ((f line "Version")))               ;--•  Set 'Version' as the value got in the opened .lsp file                    ;
        Class (cond (Class) ((f line "Class")))                     ;--•  Set 'Class' as the value got in the opened .lsp file                      ;
      )                                                             ;--•  End (setq)                                                                ;
    )                                                               ;--•  End (while)                                                               ;
    (null (close file))                                             ;--•  If the opened .lsp file is well closed, then :True                        ;
    (setq ProgName (load filename ""))                              ;--•  Set 'ProgName' the name of the last program loaded from .lsp file         ;
    (not (equal "" ProgName))                                       ;--•  Check if 'ProgName' is not equal to an empty string "" (= not loaded)     ;
    (setq                                                           ;--•  Start (setq)                                                              ;
      $lst$                                                         ;--•  '$lst$' value                                                             ;
        (cons                                                       ;--•  Start (cons)                                                              ;
          (cons                                                     ;--•  Start (cons)                                                              ;
            $k$                                                     ;--•  '$k$' value                                                               ;
            (list                                                   ;--•  Start (list)                                                              ;
              (vl-prin1-to-string ProgName)                         ;--•  'ProgName' value, converted into a string                                 ;
              Date                                                  ;--•  'Date' value                                                              ;
              Author                                                ;--•  'Author' value                                                            ;
              Version                                               ;--•  'Version' value                                                           ;
              Class                                                 ;--•  'Class' value                                                             ;
            )                                                       ;--•  End (list)                                                                ;
          )                                                         ;--•  End (cons)                                                                ;
          $lst$                                                     ;--•  '$lst$' value                                                             ;
        )                                                           ;--•  End (cons)                                                                ;
    )                                                               ;--•  End (setq)                                                                ;
  )                                                                 ;--•  End (and)                                                                 ;
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This function named ($pLstAdd-U$) is necessary to add the functions and commands's name to the $lst$ variable for                              ;
;--- "UBS - LISP (Remote edition).lsp" file                                                                                                         ;
(defun $pLstAdd-U$ (ProgName Date Author Version Class)             ;--•  Definition of '$pLstAdd-U$' function with 5 arguments                     ;
  (setq $lst$                                                       ;--•  If :True, set '$lst$' as                                                  ;
    (cons                                                           ;--•  Start (cons)                                                              ;
      (cons                                                         ;--•  Start (cons)                                                              ;
        $k$                                                         ;--•  '$k$' value (1 = function, 2 = command) as key                            ;
        (list                                                       ;--•  Start (list)                                                              ;
          ProgName                                                  ;--•  Name of the program                                                       ;
          Date                                                      ;--•  Date of last modification of the program                                  ;
          Author                                                    ;--•  Author of the program                                                     ;
          Version                                                   ;--•  Version of the program                                                    ;
          Class                                                     ;--•  Class of the program                                                      ;
        )                                                           ;--•  End (list)                                                                ;
      )                                                             ;--•  End (cons)                                                                ;
      $lst$                                                         ;--•  $lst$                                                                     ;
    )                                                               ;--•  End (cons)                                                                ;
  )                                                                 ;--•  End (setq)                                                                ;
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This function displays the programs list loaded in the drawing. The programs list depends on "LOADEDPROGLIST" Extend data value                ;
(defun $princLoadedProgList$ (k / f sp slv lst)                     ;--•  Definition of '$princLoadedProgList$' function with 1 arguments           ;
  (defun f (str lst)                                                ;--•  Local definition of 'f' function with 2 arguments                         ;
    (princ                                                          ;--•  Start (princ)                                                             ;
      (strcat                                                       ;--•  Start (strcat)                                                            ;
        "\n Listing of "                                            ;--•  First string displayed                                                    ;
        (itoa (length lst))                                         ;--•  Length of 'lst'                                                           ;
        " LISP "                                                    ;--•  Second string displayed                                                   ;
        str                                                         ;--•  'str' value, representing the type of programs displayed                  ;
        " loaded in the drawing :"                                  ;--•  Third string displayed                                                    ;
        (apply                                                      ;--•  Start (apply)                                                             ;
          'strcat                                                   ;--•  Applying the (strcat) function to the list...                             ;
          (mapcar                                                   ;--•  Start (mapcar)                                                            ;
            '(lambda (x) (strcat "\n   --•  " (cdr x)))             ;--•  For each element of 'lst', print a new line with the program's infos      ;
            (reverse lst)                                           ;--•  Reverse the 'lst' to print because of the reverse effect of (cons)        ;
          )                                                         ;--•  End (mapcar)                                                              ;
        )                                                           ;--•  End (apply)                                                               ;
        "\n­"                                                        ;--•  Add a new line at the end to separate each program's type                 ;
      )                                                             ;--•  End (strcat)                                                              ;
    )                                                               ;--•  End (princ)                                                               ;
  )                                                                 ;--•  End (defun)                                                               ;
  (defun sp (n / s)                                                 ;--•  Local definition of 'sp' function with 1 argument                         ;
    (setq s "")                                                     ;--•  Set 's' as an empty string ""                                             ;
    (repeat n (setq s (strcat s " ")))                              ;--•  Add 'n' " " to the string 's'                                             ;
    s                                                               ;--•  Return 's' value                                                          ;
  )                                                                 ;--•  End (defun)                                                               ;
  (defun FRd2U (str / dd mm yyyy)                                   ;--•  Local definition of 'FRd2U' function with 1 argument                      ;
    (setq                                                           ;--•  Start (setq)                                                              ;
      dd (substr str 1 2)                                           ;--•  Set 'dd' as the 1st character of 'str' with 2 characters length           ;
      mm (substr str 4 2)                                           ;--•  Set 'mm' as the 4th character of 'str' with 2 characters length           ;
      yyyy (substr str 7 4)                                         ;--•  Set 'yyyy' as the 7th character of 'str' with 4 characters length         ;
    )                                                               ;--•  End (setq)                                                                ;
    (atoi (strcat yyyy mm dd))                                      ;--•  Reset the order of date as YYYYMMDD and convert it as an integer          ;
  )                                                                 ;--•  End (defun)                                                               ;
  (defun DateDiff (d1 d2)                                           ;--•  Local definition of 'DateDiff' function with 2 arguments                  ;
    (fix (abs (- (dtoj d1) (dtoj d2))))                             ;--•  Calculate the difference between 2 dates formatted as YYYYMMDD (integer)  ;
  )                                                                 ;--•  End (defun)                                                               ;
  (setq slv (atoi (getenv "SHOWLSPVERSION")))                       ;--•  Set 'slv' as the value stored in environment variable "SHOWLSPVERSION"    ;
  (setq lst                                                         ;--•  Set 'lst' as...                                                           ;
    (mapcar                                                         ;--•  Start (mapcar)                                                            ;
      '(lambda (x / y p d a v c)                                    ;--•  Start (lambda) with 1 argument                                            ;
        (setq                                                       ;--•  Start (setq)                                                              ;
          y (cdr x)                                                 ;--•  Set 'y' as the list of information about the program                      ;
          p (car y)                                                 ;--•  Set 'p' as the name of the program, in first position of 'y' list         ;
          d (cadr y)                                                ;--•  Set 'd' as the date of last modification, in second position of 'y' list  ;
          a (caddr y)                                               ;--•  Set 'a' as the author's name, in third position of 'y' list               ;
          v (cadddr y)                                              ;--•  Set 'v' as the version number, in fourth position of 'y' list             ;
          c (last y)                                                ;--•  Set 'c' as the class of the program, in last position of 'y' list         ;
        )                                                           ;--•  End (setq)                                                                ;
        (cons                                                       ;--•  Start (cons)                                                              ;
          (car x)                                                   ;--•  Key integer representing the program's type                               ;
          (strcat                                                   ;--•  Start (strcat)                                                            ;
            (cond ( (= 1 (logand slv 1)) c) (""))                   ;--•  If bit 0 of "SHOWLSPVERSION" is set, add the Class                        ;
            (cond ( (= 1 (logand slv 1)) " ---> ") (""))            ;--•  If bit 0 of "SHOWLSPVERSION" is set, add the separator string             ;
            p                                                       ;--•  Name of the program (always displayed)                                    ;
            (cond                                                   ;--•  Start (cond)                                                              ;
              ( (or                                                 ;--•  1st condition, start (or)                                                 ;
                  (= 2 (logand slv 2))                              ;--•  Check if bit 1 of "SHOWLSPVERSION" is set                                 ;
                  (= 4 (logand slv 4))                              ;--•  Check if bit 2 of "SHOWLSPVERSION" is set                                 ;
                  (= 8 (logand slv 8))                              ;--•  Check if bit 3 of "SHOWLSPVERSION" is set                                 ;
                )                                                   ;--•  End (or)                                                                  ;
                (sp                                                 ;--•  Start (sp)                                                                ;
                  (-                                                ;--•  Start (-)                                                                 ;
                    53                                              ;--•  34, corresponds to 27 characters (for program + " ") + 7 (for Class)      ;
                    (if (= 1 (logand slv 1)) (strlen c) 7)          ;--•  Check if bit 0 of "SHOWLSPVERSION" is set, to adapt the value 7 if needed ;
                    (strlen p)                                      ;--•  Length of the program's name, to be sure only 27 characters are allowed   ;
                  )                                                 ;--•  End (-)                                                                   ;
                )                                                   ;--•  End (sp)                                                                  ;
              )                                                     ;--•  End 1st condition                                                         ;
              ("")                                                  ;--•  Empty string if not                                                       ;
            )                                                       ;--•  End (cond)                                                                ;
            (cond ( (= 2 (logand slv 2)) "| v") (""))               ;--•  If bit 1 of "SHOWLSPVERSION" is set, add the separator string             ;
            (cond ( (= 2 (logand slv 2)) v) (""))                   ;--•  If bit 1 of "SHOWLSPVERSION" is set, add the Version                      ;
            (cond ( (= 4 (logand slv 4)) " - ") (""))               ;--•  If bit 2 of "SHOWLSPVERSION" is set, add the separator string             ;
            (cond ( (= 4 (logand slv 4)) d) (""))                   ;--•  If bit 2 of "SHOWLSPVERSION" is set, add the Date                         ;
            (cond ( (= 8 (logand slv 8)) " (") (""))                ;--•  If bit 3 of "SHOWLSPVERSION" is set, add the separator string             ;
            (cond ( (= 8 (logand slv 8)) a) (""))                   ;--•  If bit 3 of "SHOWLSPVERSION" is set, add the Author                       ;
            (cond ( (= 8 (logand slv 8)) ")") (""))                 ;--•  If bit 3 of "SHOWLSPVERSION" is set, add the separator string             ;
            (if (> 28 (DateDiff (FRd2U d) (fix (getvar "CDATE"))))  ;--•  Check if program's date minus current date is under 28 days               ;
              "  [NEW]"                                             ;--•  If :True, prompt "  [NEW]", for recent release                            ;
              ""                                                    ;--•  If :False, prompt an empty string ""                                      ;
            )                                                       ;--•  End (if)                                                                  ;
          )                                                         ;--•  End (strcat)                                                              ;
        )                                                           ;--•  End (cons)                                                                ;
       )                                                            ;--•  End (lambda)                                                              ;
      $lst$                                                         ;--•  '$lst$' value                                                             ;
    )                                                               ;--•  End (mapcar)                                                              ;
  )                                                                 ;--•  End (setq)                                                                ;
  (if (= 1 (logand k 1))                                            ;--•  Check if bit 0 of "LOADEDPROGLIST" is set                                 ;
    (f                                                              ;--•  If :True, then start (f)                                                  ;
      "functions"                                                   ;--•  For functions                                                             ;
      (vl-remove-if-not '(lambda (x) (= 1 (car x))) lst)            ;--•  Remove the programs from 'lst' that are not functions (key = 1)           ;
    )                                                               ;--•  End (f)                                                                   ;
  )                                                                 ;--•  End (if)                                                                  ;
  (if (= 2 (logand k 2))                                            ;--•  Check if bit 1 of "LOADEDPROGLIST" is set                                 ;
    (f                                                              ;--•  If :True, then start (f)                                                  ;
      "commands"                                                    ;--•  For commands                                                              ;
      (vl-remove-if-not '(lambda (x) (= 2 (car x))) lst)            ;--•  Remove the programs from 'lst' that are not commands (key = 2)            ;
    )                                                               ;--•  End (f)                                                                   ;
  )                                                                 ;--•  End (if)                                                                  ;
  (if (= 4 (logand k 4))                                            ;--•  Check if bit 2 of "LOADEDPROGLIST" is set                                 ;
    (f                                                              ;--•  If :True, then start (f)                                                  ;
      "environment variables"                                       ;--•  For environment variables                                                 ;
      (mapcar                                                       ;--•  Start (mapcar)                                                            ;
        '(lambda (x) (cons 4 (strcat (car x) " = " (cdr x))))       ;--•  Construct a dotted-pair list with the key and the name/value of variables ;
        ($getenv-list$)                                             ;--•  Retrieves the list of all environment variables found on the PC           ;
      )                                                             ;--•  End (mapcar)                                                              ;
    )                                                               ;--•  End (f)                                                                   ;
  )                                                                 ;--•  End (if)                                                                  ;
  (if (= 8 (logand k 8))                                            ;--•  Check if bit 3 of "LOADEDPROGLIST" is set                                 ;
    (f                                                              ;--•  If :True, then start (f)                                                  ;
      "\"URBASOLAR\" dictionary's entries"                          ;--•  For "URBASOLAR" dictionary's entries                                      ;
      (mapcar                                                       ;--•  Start (mapcar)                                                            ;
        '(lambda (x) (cons 8 (strcat (car x) " = " (vl-princ-to-string (cdr x)))))
        (vlax-ldata-list "URBASOLAR")                               ;--•  Retrieves the list of all environment variables found on the PC           ;
      )                                                             ;--•  End (mapcar)                                                              ;
    )                                                               ;--•  End (f)                                                                   ;
  )                                                                 ;--•  End (if)                                                                  ;
  (princ)                                                           ;--•  (princ) to return nothing and not nil or anything else                    ;
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This command modify the value of environment variable "LOADEDPROGLIST" to a new value to know how to prompt the programs list for the drawing  ;
(defun c:LOADEDPROGLIST (/ b c)                                     ;--•  Definition of 'LOADEDPROGLIST' command                                    ;
  (setq b (getenv "LOADEDPROGLIST"))                                ;--•  Set 'b' as the value of "LOADEDPROGLIST" environment variable             ;
  (while                                                            ;--•  Start (while)                                                             ;
    (null                                                           ;--•  Start (null)                                                              ;
      (cond                                                         ;--•  Start (cond)                                                              ;
        ( (null                                                     ;--•  Start (null), 1st condition                                               ;
            (progn                                                  ;--•  Start (progn)                                                             ;
              (initget "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ?")   ;--•  Initialize the (get-) function with 5 keywords                            ;
              (setq c                                               ;--•  Start (setq), set 'c'                                                     ;
                (getkword                                           ;--•  Start (getkword)                                                          ;
                  (strcat                                           ;--•  Start (strcat)                                                            ;
                    "\nEntrez une nouvelle valeur pour LOADEDPROGLIST [0/1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/?] <"
                    b                                               ;--•  Set 'b' as the default answer                                             ;
                    "> :"                                           ;--•  End of the (getkword) line                                                ;
                  )                                                 ;--•  End (strcat)                                                              ;
                )                                                   ;--•  End (getkword)                                                            ;
              )                                                     ;--•  End (setq)                                                                ;
            )                                                       ;--•  End (progn)                                                               ;
          )                                                         ;--•  End (null)                                                                ;
          (setq c b)                                                ;--•  If :True, set 'c' as 'b'                                                  ;
        )                                                           ;--•  End of the 1st condition                                                  ;
        ( (= c "?")                                                 ;--•  Check if 'c' equals "?"                                                   ;
          (princ                                                    ;--•  Start (princ)                                                             ;
            (strcat                                                 ;--•  Start (strcat)                                                            ;
              "\nLOADEDPROGLIST (Environment Variable)"
              "\nA bit-coded that controls the display of LISP programs's list for the drawing."
              " The bits can be added together in any combination to form a value between 0 and 15"
              "\nType:          Integer"
              "\nSaved in:      Windows"
              "\nInitial value: 6"
              "\n  +-----------+-----------------------------------------------------------------------+"
              "\n  |   Value   |                             Description                               |"
              "\n  +-----------+-----------------------------------------------------------------------+"
              "\n  | 0         | The LISP program's list is not displayed when the drawing is opened   |"
              "\n  | 1 (bit 0) | Display the LISP functions's list when the drawing is opened          |"
              "\n  | 2 (bit 1) | Display the LISP commands's list when the drawing is opened           |"
              "\n  | 4 (bit 2) | Display the LISP environment variables's list when the drawing is     |"
              "\n  |           | opened (it's advisable to keep this bit active)                       |"
              "\n  | 8 (bit 3) | Display the LISP \"URBASOLAR\" dictionary's entries saved in current    |"
              "\n  |           | drawing (not saved between different drawings)                        |"
              "\n  +-----------+-----------------------------------------------------------------------+"
            )                                                       ;--•  End (strcat)                                                              ;
          )                                                         ;--•  End (princ)                                                               ;
          (textscr)
          nil                                                       ;--•  To continue with the loop (while)                                         ;
        )                                                           ;--•  End of the 2nd condition                                                  ;
        (c)                                                         ;--•  If 'c' equals "0", "1", "2" or "3", convert it as an integer              ;
      )                                                             ;--•  End (cond)                                                                ;
    )                                                               ;--•  End (null)                                                                ;
  )                                                                 ;--•  End (while)                                                               ;
  (setenv "LOADEDPROGLIST" c)                                       ;--•  Set the environment variable "LOADEDPROGLIST" as 'c' value                ;
  ($princLoadedProgList$ (atoi c))                                  ;--•  Start ($princLoadedProgList$) function to display the list                ;
  (textscr) (princ)
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This command modify the value of environment variable "FORCEDLANGUAGE" to a new value to know how to prompt the string by using (LgT) function ;
(defun c:FORCEDLANGUAGE (/ b c)                                     ;--•  Definition of 'FORCEDLANGUAGE' command                                    ;
  (setq b (getenv "FORCEDLANGUAGE"))                                ;--•  Set 'b' as the value of "FORCEDLANGUAGE" environment variable             ;
  (while                                                            ;--•  Start (while)                                                             ;
    (null                                                           ;--•  Start (null)                                                              ;
      (cond                                                         ;--•  Start (cond)                                                              ;
        ( (null                                                     ;--•  Start (null), 1st condition                                               ;
            (progn                                                  ;--•  Start (progn)                                                             ;
              (initget "0 1 2 ?")                                   ;--•  Initialize the (get-) function with 4 keywords                            ;
              (setq c                                               ;--•  Start (setq), set 'c'                                                     ;
                (getkword                                           ;--•  Start (getkword)                                                          ;
                  (strcat                                           ;--•  Start (strcat)                                                            ;
                    "\nEntrez une nouvelle valeur pour FORCEDLANGUAGE [0/1/2/?] <"
                    b                                               ;--•  Set 'b' as the default answer                                             ;
                    "> :"                                           ;--•  End of the (getkword) line                                                ;
                  )                                                 ;--•  End (strcat)                                                              ;
                )                                                   ;--•  End (getkword)                                                            ;
              )                                                     ;--•  End (setq)                                                                ;
            )                                                       ;--•  End (progn)                                                               ;
          )                                                         ;--•  End (null)                                                                ;
          (setq c b)                                                ;--•  If :True, set 'c' as 'b'                                                  ;
        )                                                           ;--•  End of the 1st condition                                                  ;
        ( (= c "?")                                                 ;--•  Check if 'c' equals "?"                                                   ;
          (princ                                                    ;--•  Start (princ)                                                             ;
            (strcat                                                 ;--•  Start (strcat)                                                            ;
              "\nFORCEDLANGUAGE (Environment Variable)"
              "\nControls the language of LISP messages prompted for the user"
              "\nType:          Integer"
              "\nSaved in:      Windows"
              "\nInitial value: 0"
              "\n  +-------+---------------------------------------------------------------------------+"
              "\n  | Value |                                Description                                |"
              "\n  +-------+---------------------------------------------------------------------------+"
              "\n  |   0   | If LOCALE = FR, displays the messages in french, otherwise in english     |"
              "\n  |   1   | Displays the messages in english, independently of LOCALE value           |"
              "\n  |   2   | Displays the messages in french, independently of LOCALE value            |"
              "\n  +-------+---------------------------------------------------------------------------+"
            )                                                       ;--•  End (strcat)                                                              ;
          )                                                         ;--•  End (princ)                                                               ;
          (textscr)
          nil                                                       ;--•  To continue with the loop (while)                                         ;
        )                                                           ;--•  End of the 2nd condition                                                  ;
        (c)                                                         ;--•  If 'c' equals "0", "1", or "2", convert it as an integer                  ;
      )                                                             ;--•  End (cond)                                                                ;
    )                                                               ;--•  End (null)                                                                ;
  )                                                                 ;--•  End (while)                                                               ;
  (atoi (setenv "FORCEDLANGUAGE" c))                                ;--•  Set the environment variable "FORCEDLANGUAGE" as 'c' value                ;
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;

;--- This command modify the value of environment variable "SHOWLSPVERSION" to a new value to know if the version is checked for each program       ;
(defun c:SHOWLSPVERSION (/ b c)                                     ;--•  Definition of 'SHOWLSPVERSION' command                                    ;
  (setq b (getenv "SHOWLSPVERSION"))                                ;--•  Set 'b' as the value of "SHOWLSPVERSION" environment variable             ;
  (while                                                            ;--•  Start (while)                                                             ;
    (null                                                           ;--•  Start (null)                                                              ;
      (cond                                                         ;--•  Start (cond)                                                              ;
        ( (null                                                     ;--•  Start (null), 1st condition                                               ;
            (progn                                                  ;--•  Start (progn)                                                             ;
              (initget "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ?")   ;--•  Initialize the (get-) function with 3 keywords                            ;
              (setq c                                               ;--•  Start (setq), set 'c'                                                     ;
                (getkword                                           ;--•  Start (getkword)                                                          ;
                  (strcat                                           ;--•  Start (strcat)                                                            ;
                    "\nEntrez une nouvelle valeur pour SHOWLSPVERSION [0/1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/?] <"
                    b                                               ;--•  Set 'b' as the default answer                                             ;
                    "> :"                                           ;--•  End of the (getkword) line                                                ;
                  )                                                 ;--•  End (strcat)                                                              ;
                )                                                   ;--•  End (getkword)                                                            ;
              )                                                     ;--•  End (setq)                                                                ;
            )                                                       ;--•  End (progn)                                                               ;
          )                                                         ;--•  End (null)                                                                ;
          (setq c b)                                                ;--•  If :True, set 'c' as 'b'                                                  ;
        )                                                           ;--•  End of the 1st condition                                                  ;
        ( (= c "?")                                                 ;--•  Check if 'c' equals "?"                                                   ;
          (princ                                                    ;--•  Start (princ)                                                             ;
            (strcat                                                 ;--•  Start (strcat)                                                            ;
              "\nSHOWLSPVERSION (Environment Variable)"
              "\nA bit-coded that controls the displayed informations about the loaded programs at the opening of the drawing or for LOADEDPROGLIST."
              " The bits can be added together in any combination to form a value between 0 and 15."
              "\nThe format if everything is displayed is :"
              "\n\"Class . ProgramName                | v#.#.# - ##/##/#### (Author)\""
              "\nType:          Integer"
              "\nSaved in:      Windows"
              "\nInitial value: 1"
              "\n  +-----------+-----------------------------------------------------------------------+"
              "\n  |   Value   |                             Description                               |"
              "\n  +-----------+-----------------------------------------------------------------------+"
              "\n  | 0         | Only the program's name is displayed                                  |"
              "\n  | 1 (bit 0) | Display the string-code representing the \"Class\" of the program       |"
              "\n  | 2 (bit 1) | Display the \"Version\" number of the program (v#.#.#)                  |"
              "\n  | 4 (bit 2) | Display the \"Date\" of last modification of the program                |"
              "\n  | 8 (bit 3) | Display the \"Author\" of the program                                   |"
              "\n  +-----------+-----------------------------------------------------------------------+"
            )                                                       ;--•  End (strcat)                                                              ;
          )                                                         ;--•  End (princ)                                                               ;
          (textscr)
          nil                                                       ;--•  To continue with the loop (while)                                         ;
        )                                                           ;--•  End of the 2nd condition                                                  ;
        (c)                                                         ;--•  'c' value                                                                 ;
      )                                                             ;--•  End (cond)                                                                ;
    )                                                               ;--•  End (null)                                                                ;
  )                                                                 ;--•  End (while)                                                               ;
  (atoi (setenv "SHOWLSPVERSION" c))                                ;--•  Set the environment variable "SHOWLSPVERSION" as 'c' value                ;
)                                                                   ;--•  End (defun)                                                               ;

;                                                   []-----------------------------------------[]                                                   ;



; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                               LOADING ALL FUNCTION'S FILES FOR EACH NEW DRAWING                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;--- Add a new line for each new file you want to load in AutoCAD everytime a drawing is opened.                                                    ;
(setq $k$ 1)                                                        ;--•  Define the global variable $k$ to 1 for function's section                ;

;                                                   []-----------------------------------------[]                                                   ;

;---                  [BaApp] APPLICATION-HANDLING                                                                                                  ;
($pLstAdd$ "BaApp . Excel.IsOpenedWorkBook.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaApp . LM.acapp.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaApp . MFiles-field-generator.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaApp . MFiles-ID-list.lsp")                                               ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaApp . Shell.Open.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaAri] ARITHMETIC                                                                                                            ;
($pLstAdd$ "BaAri . CrosshairColor->RGB.lsp")                                          ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . fixdiv.lsp")                                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.ACI-OLE.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.ACI-RGB.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.ACI-True.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.OLE-ACI.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.OLE-RGB.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.OLE-True.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.RGB-ACI.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.RGB-OLE.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.RGB-True.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.True-ACI.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.True-OLE.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.True-RGB.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.rand.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.randrange.lsp")                                                 ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.round.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . LM.roundm.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . round.lsp")                                                        ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . SwapBit.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaAri . VP-scale.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaEqc] EQUALITY AND CONDITIONNALS                                                                                            ;
($pLstAdd$ "BaEqc . bit.lsp")                                                          ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaEqc . lst_equal.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaEqc . pt-member.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaErr] ERROR-HANDLING                                                                                                        ;
($pLstAdd$ "BaErr . LM.CatchApply.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaFun] FUNCTION-HANDLING                                                                                                     ;
($pLstAdd$ "BaFun . FunctionLog.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaFun . mAtom.lsp")                                                        ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaLst] LIST MANIPULATION                                                                                                     ;
($pLstAdd$ "BaLst . 2D-coord-pt-lst.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . 3D-coord-pt-lst.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . 2D-Point.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . divlist.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . DXF_List.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . FormAssoc.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . get-DXF-list.lsp")                                                 ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . get-DXF-value.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . get-pattern-list.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . loop-a-list-properties.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . make-a-list-properties.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . msubst.lsp")                                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . prompt-list.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . remove-duplicates.lsp")                                            ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . sort-cons.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . sort-list.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaLst . sublist.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaStr] STRING MANIPULATION                                                                                                   ;
($pLstAdd$ "BaStr . space.lsp")                                                        ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaStr . Sup-Incr.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaStr . ThsdSpace.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "BaStr . Wildcard-trim.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [BaSym] SYMBOL-HANDLING                                                                                                       ;


;                                                   []-----------------------------------------[]                                                   ;

;---                  [UtDac] DATA CONVERSION                                                                                                       ;
($pLstAdd$ "UtDac . atol.lsp")                                                         ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDac . LM.SafearrayVariant.lsp")                                          ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDac . lst2str.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDac . str2lst.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDac . SliceNumber.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [UtDev] DEVICE ACCESS                                                                                                         ;

;---                  [UtDis] DISPLAY CONTROL                                                                                                       ;
($pLstAdd$ "UtDis . _princ.lsp")                                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . alert-princ.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . grcircle.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . LM.grsnap.displaysnap.lsp")                                        ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . LM.grsnap.parsepoint.lsp")                                         ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . LM.grsnap.snapfunction.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . LM.grsnap.snapmode.lsp")                                           ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtDis . LM.grsnap.snapsymbols.lsp")                                        ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [UtFil] FILE-HANDLING                                                                                                         ;
($pLstAdd$ "UtFil . ApplyDocsMethods.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtFil . read-file.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [UtGeo] GEOMETRIC                                                                                                             ;
($pLstAdd$ "UtGeo . get-pt-list.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtGeo . osnap-poly.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [UtCom] QUERY AND COMMAND                                                                                                     ;
($pLstAdd$ "UtCom . BinaryVarSwap.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtCom . SetVarList.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtCom . units.lsp")                                                        ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [UtMem] MEMORY MANAGEMENT                                                                                                     ;

;---                  [UtUse] USER INPUT                                                                                                            ;
($pLstAdd$ "UtUse . getkdh.lsp")                                                       ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [UtWin] WINDOWS REGISTRY                                                                                                      ;
($pLstAdd$ "UtWin . get-Date.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "UtWin . LgT.lsp")                                                          ;--•  Loading the function file and add it to $lst$ variable ;


;                                                   []-----------------------------------------[]                                                   ;

;---                  [DtXdt] EXTENDED DATA-HANDLING                                                                                                ;

;---                  [DtObj] OBJECT-HANDLING                                                                                                       ;
($pLstAdd$ "DtObj . _putprefix.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtObj . _putsuffix.lsp")                                                   ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtObj . Add-Poly2D-Point.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtObj . Array-Def.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtObj . Explore-DXF.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtObj . GetAnyProperty.lsp")                                               ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtObj . SetAnyProperty.lsp")                                               ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [DtSel] SELECTION SET MANIPULATION                                                                                            ;
($pLstAdd$ "DtSel . LM.DO-MoveAbove.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.DO-MoveBelow.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.DO-MoveToBottom.lsp")                                           ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.DO-MoveToTop.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.DO-SwapOrder.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.ss-vla.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.ssBoundingBox.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . LM.ssBoundingBoxMidPt.lsp")                                        ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . UCSssBoundingBox.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . MuteSel.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . PVBB-Draw.lsp")                                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . Select-Filter.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSel . ZoomObjects.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [DtSyt] SYMBOL TABLE AND DICTIONARY-HANDLING                                                                                  ;
($pLstAdd$ "DtSyt . delenv.lsp")                                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSyt . flt_tbl.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "DtSyt . layer-get-or-create.lsp")                                          ;--•  Loading the function file and add it to $lst$ variable ;

;                                                   []-----------------------------------------[]                                                   ;

;---                  [DbOpc] DIALOG BOX OPENING AND CLOSING                                                                                        ;

;---                  [DbTil] TILE AND ATTRIBUTE-HANDLING                                                                                           ;

;---                  [DbLst] LIST BOX AND POP-UP LIST-HANDLING                                                                                     ;
($pLstAdd$ "DbLst . ListBox.lsp")                                                      ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [DbImg] IMAGE TILE-HANDLING                                                                                                   ;

;---                  [DbApp] APPLICATION-SPECIFIC DATA-HANDLING                                                                                    ;


;                                                   []-----------------------------------------[]                                                   ;

;---                  [VlCol] ACTIVEX COLLECTION MANIPULATION                                                                                       ;
($pLstAdd$ "VlCol . vla-collection-list.lsp")                                          ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlDtc] ACTIVEX DATA CONVERSION                                                                                               ;
($pLstAdd$ "VlDtc . ConvName.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlMet] ACTIVEX METHOD INVOCATION                                                                                             ;
($pLstAdd$ "VlMet . lwpoly-AddVertex.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlMet . UCS2WCSMatrix.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlMet . WCS2UCSMatrix.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlObj] ACTIVEX OBJECT-HANDLING                                                                                               ;
($pLstAdd$ "VlObj . LM.outputtext.puttextstring.lsp")                                  ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . LM.outputtext.updatefield.lsp")                                    ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-att-list.lsp")                                                 ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-att-value.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-dyn-AllowedValues.lsp")                                        ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-dyn-list.lsp")                                                 ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-dyn-value.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-dyn-VisibilityName.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . get-dyn-VisibilityValue.lsp")                                      ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . set-att-list.lsp")                                                 ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . set-att-value.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . set-dyn-FlipState.lsp")                                            ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . set-dyn-list.lsp")                                                 ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . set-dyn-value.lsp")                                                ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlObj . set-dyn-VisibilityValue.lsp")                                      ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlPrp] ACTIVEX PROPERTY-HANDLING                                                                                             ;
($pLstAdd$ "VlPrp . get-layouts-pos.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlPrp . set-layouts-pos.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlPrp . set-layout-name.lsp")                                              ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlPrp . dumpallproperties-list.lsp")                                       ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlPrp . vlax-dump-object-list.lsp")                                        ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlPrp . LM.vlax-dump-object-list.lsp")                                     ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlCrv] CURVE MEASUREMENT                                                                                                     ;

;---                  [VlDic] DICTIONARY                                                                                                            ;
($pLstAdd$ "VlDic . LM.SortentsTable.lsp")                                             ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlDrw] HANDLING DRAWING OBJECTS                                                                                              ;
($pLstAdd$ "VlDrw . LM.acdoc.lsp")                                                     ;--•  Loading the function file and add it to $lst$ variable ;
($pLstAdd$ "VlDrw . LM.getOwner.lsp")                                                  ;--•  Loading the function file and add it to $lst$ variable ;

;---                  [VlRct] REACTORS                                                                                                              ;

;---                  [VlSpc] VLX NAMESPACE                                                                                                         ;

;---                  [VlCom] NAMESPACE COMMUNICATION                                                                                               ;



; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                               LOADING ALL COMMAND'S FILES FOR EACH NEW DRAWING                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;--- Add a new line for each new file you want to load in AutoCAD everytime a drawing is opened.                                                    ;
(setq $k$ 2)                                                        ;--•  Define the global variable $k$ to 2 for command's section                 ;

;                                                   []-----------------------------------------[]                                                   ;

;---                  [PsUcart] TITLE BLOCK-HANDLING                                                                                                ;
($pLstAdd$ "PsUcart . DATECART.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "PsUcart . MFATTCART.lsp")                                                  ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "PsUcart . MODCART.lsp")                                                    ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "PsUcart . NAMECART.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [PsViewp] VIEWPORT MANIPULATION                                                                                               ;
($pLstAdd$ "PsViewp . COPYCHSPACE.lsp")                                                ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "PsViewp . FNTAERIENNE.lsp")                                                ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [PsLaytb] LAYOUT-HANDLING                                                                                                     ;
($pLstAdd$ "PsLaytb . LAYOUTCOPY.lsp")                                                 ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "PsLaytb . QuickLayoutSwitch.lsp")                                          ;--•  Loading the command file and add it to $lst$ variable  ;


;                                                   []-----------------------------------------[]                                                   ;

;---                  [SdBlock] BLOCK LIST-HANDLING                                                                                                 ;
($pLstAdd$ "SdBlock . FixBadAttBlocks.lsp")                                            ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [SdLayer] LAYER LIST-HANDLING                                                                                                 ;

;---                  [SdXdata] EXTENDED DATA MANIPULATION                                                                                          ;


;                                                   []-----------------------------------------[]                                                   ;

;---                  [UtFiles] FILE-HANDLING                                                                                                       ;
($pLstAdd$ "UtFiles . ADM-Activate.lsp")                                               ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtFiles . ADM-Close.lsp")                                                  ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtFiles . ADM-Save.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtFiles . HANDLEPREVIEW.lsp")                                              ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtFiles . RPCustom.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtFiles . RADPURGE.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtFiles . VP-RADPURGE.lsp")                                                ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [UtGeodt] GEOMETRIC DATA                                                                                                      ;
($pLstAdd$ "UtGeodt . ALTPOINT.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtGeodt . LONGCUMUL.lsp")                                                  ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [UtObjet] OBJECT-HANDLING                                                                                                     ;
($pLstAdd$ "UtObjet . CPH.lsp")                                                        ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . GETLAYER.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . LCtoFC.lsp")                                                     ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . OpenDeepL.lsp")                                                  ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . PLINE3Dto2D.lsp")                                                ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . POLYADDPOINT.lsp")                                               ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . POLYDELPOINT.lsp")                                               ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtObjet . PVROWNUMBERING.lsp")                                             ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [UtVarsy] SYSTEME VARIABLE-HANDLING                                                                                           ;
($pLstAdd$ "UtVarsy . PSLTSCALECUSTOM.lsp")                                            ;--•  Loading the command file and add it to $lst$ variable  ;
($pLstAdd$ "UtVarsy . TileModeSwitch.lsp")                                             ;--•  Loading the command file and add it to $lst$ variable  ;


;                                                   []-----------------------------------------[]                                                   ;

;---                  [BdBound] BOUNDING BOX MANIPULATION                                                                                           ;
($pLstAdd$ "BbBound . PreViewBoundingBox.lsp")                                         ;--•  Loading the command file and add it to $lst$ variable  ;

;---                  [BdSelct] SELECTION SET MANIPULATION                                                                                          ;
($pLstAdd$ "BbSelct . MID_MOVE.lsp")                                                   ;--•  Loading the command file and add it to $lst$ variable  ;




; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                            DISPLAY THE LIST OF PROGRAMS LOADED FOR EACH NEW DRAWING                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                                   []-----------------------------------------[]                                                   ;

(princ
  (strcat
    "\n­"
    "\n  +----------------------------------------------------------------------------------+"
    "\n  |                         -URBASOLAR- LOADED PROGRAMS LIST                         |"
    "\n  +----------------------------------------------------------------------------------+"
    "\n­"
  )
)

;--- This function prompt the list of LISP functions and/or commands loaded in the current drawing.                                                 ;
($princLoadedProgList$ (atoi (getenv "LOADEDPROGLIST")))
(princ)

;                                                   []-----------------------------------------[]                                                   ;

;--- This function displays the time elapsed from the opening of the LISP file until the end of its execution.                                      ;
(princ
  (strcat
    "\n                               []-------------------[]                                "
    "\n­"
    "\n   Time needed to execute the file \"UBS - LISP (Complete edition).lsp\" :"
    "\n     ~> Elapsed time (milliseconds) = " (itoa (setq $time$ (- (getvar "MILLISECS") $time$))) " ms..."
    "\n     ~> Elapsed time (seconds)      = " (rtos (/ $time$ 1000.0) 2 2) " s..."
    "\n   Number of LISP files loaded in the current drawing :"
    "\n     ~> Functions (not for user)    = " (itoa (length (vl-remove-if-not '(lambda (p) (= (car p) 1)) $lst$))) " u..."
    "\n     ~> Commands (for user)         = " (itoa (length (vl-remove-if-not '(lambda (p) (= (car p) 2)) $lst$))) " u..."
    "\n­"
    "\n  +----------------------------------------------------------------------------------+"
    "\n  |                               -URBASOLAR- END LIST                               |"
    "\n  +----------------------------------------------------------------------------------+"
    "\n­"
  )
)
;(textscr)
(princ)