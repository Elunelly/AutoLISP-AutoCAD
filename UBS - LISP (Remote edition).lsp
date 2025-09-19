
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
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ LOADING ALL FUNCTION'S FILES FOR EACH NEW DRAWING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;--- Add the content of each function file if you want to load them directly with this file instead of using (load) function, which is slower.      ;
(setq $k$ 1)                                                        ;--•  Define the global variable $k$ to 1 for function's section                ;

;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaApp] APPLICATION-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "EXCEL:ISOPENEDWORKBOOK" "19/09/2022" "Ranjit Singh/Luna" "1.1.0" "\"BaApp\"") ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] Excel:IsOpenedWorkBook []-----------------------[]                                   ;
;--- Date of creation       > 26/06/2017                                                                                                            ;
;--- Last modification date > 19/09/2022                                                                                                            ;
;--- Author                 > Ranjit Singh/Luna                                                                                                     ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

(defun Excel:IsOpenedWorkBook (filepath / XLSobj wb lst)
  (setq XLSobj (vlax-get-or-create-object "Excel.application"))
  (vlax-for wb (vlax-get XLSobj 'WorkBooks)
    (setq lst (cons (cons (strcase (vlax-get wb 'FullName)) wb) lst))
  )
  (vlax-release-object XLSobj)
  (assoc (strcase filepath) lst)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ACAPP" "12/11/2010" "LeeMac" "1.0.0" "\"BaApp\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] LM:acapp []-----------------------[]                                          ;
;--- Date of creation       > 12/11/2010                                                                                                            ;
;--- Last modification date > 12/11/2010                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

(defun LM:acapp nil
  (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
  (LM:acapp)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "MFILES-FIELD-GENERATOR" "19/12/2022" "Luna" "1.0.0" "\"BaApp\"")              ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] MFiles-field-generator []-----------------------[]                                   ;
;--- Date of creation       > 19/12/2022                                                                                                            ;
;--- Last modification date > 19/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

(defun MFiles-field-generator (lst / p)
  (setq p (MFiles-ID-list))
  (setq lst
    (mapcar
      '(lambda (n / i x k v s)
        (setq
          n (SliceNumber (vl-princ-to-string n))
          i (1+ (cdr n))
          n (car n)
          x (nth n p)
          k (car x)
          v (cdr x)
        )
        (if x
          (progn
            (if (vl-string-search "(s)" k)
              (setq
                k (strcat k " #" (itoa i))
                v (strcat v "n"  (itoa i))
              )
            )
            (cons k v)
          )
        )
      )
      lst
    )
  )
  (if (not (member nil lst))
    (cons
      (lst2str (mapcar 'car lst) " > ")
      (strcat "%<\\M-Files " (lst2str (mapcar 'cdr lst) "_") ">%")
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "MFILES-ID-LIST" "19/12/2022" "Luna" "1.0.0" "\"BaApp\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] MFiles-ID-list []-----------------------[]                                       ;
;--- Date of creation       > 19/12/2022                                                                                                            ;
;--- Last modification date > 19/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

(defun MFiles-ID-list ()
  (list
    (cons "Projet(s)"                                       "PGFE106511CD7B4CBAAB72BA8DAAD04C46") ; #00
    (cons "Code"                                            "PG821E546988074DCC93B9004E8B1CDB19") ; #01
    (cons "Nom du Projet"                                   "PG657DD3AAE5D941E7903C91F9CDBE5089") ; #02
    (cons "Commune"                                         "PG8875B0DCFED340CDAEB409E12D7D5FE4") ; #03
    (cons "Département"                                     "PG10AD0DD70B244F79996DEA07602355EF") ; #04
    (cons "Phase de design"                                 "PG244CF5DABF044278AC113C1017C8D835") ; #05
    (cons "Trigramme phase"                                 "PG18CD3574A3044970AD945B135B5AE67E") ; #06
    (cons "Créateur"                                        "PG7E23981CEA5E45C59622D23CDC57317E") ; #07
    (cons "Triptique"                                       "PGF9CADF4B98E74C1993987A4D23E3164C") ; #08
    (cons "Dessinateur(s) Projeteur (s)"                    "PG79D94E3637014DE59F9279E3E3419FD0") ; #09
    (cons "Contributeur - Dessinateur(s) Projeteur(s)"      "PG778D2C441D6A47928C265E29644FABAD") ; #10
    (cons "Chargé(s) Affaire(s)"                            "PG18EA95888BAC4EACBDB693DC1C8644F7") ; #11
    (cons "Contributeur - Chargé(s) Affaire(s)"             "PGF322C50702FF43B28F38F4BBA4C9136F") ; #12
    (cons "Chef(s) de Projets Développement"                "PG57F89F35A3304B2EA352BF6BE87152F6") ; #13
    (cons "Contributeur - Chef(s) de Projets Développement" "PG4481AFAA29F24DE3B1E46781A2B30E48") ; #14
    (cons "Chargé(s) d'Etude(s)"                            "PGECA7DEE952A34FCBA5C8AD4256A59527") ; #15
    (cons "Contributeur - Chargé(s) d'Etude(s)"             "PG6A29938E245640AA8A84F95B20E96EC6") ; #16
    (cons "Chef(s) de Projets Construction"                 "PG8760825C00594719950D20DE3FF9C89E") ; #17
    (cons "Contributeur - Chef(s) de Projets Construction"  "PGDEBDD516C6794D59A95E88EABDC853DE") ; #18
    (cons "Chargé(s) de Travaux"                            "PGB0D5220D8A1E482492A1767D23539AE9") ; #19
    (cons "Contributeur - Chargé(s) de Travaux"             "PGFC25A392881E4D558B887BB91732B50B") ; #20
    (cons "Automaticien(s)"                                 "PGF1D0DFF36BA0427298A8D7EDBA3FF1BD") ; #21
    (cons "Contributeur - Automaticien(s)"                  "PG9BA0A300264D4A91897AC2048E3FC1AB") ; #22
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SHELL:OPEN" "12/12/2006" "Patrick_35" "1.0.0" "\"BaApp\"")                    ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] Shell:Open []-----------------------[]                                         ;
;--- Date of creation       > 12/12/2006                                                                                                            ;
;--- Last modification date > 12/12/2006                                                                                                            ;
;--- Author                 > Patrick_35                                                                                                            ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

(defun Shell:Open (filepath / shell)
  (if filepath
    (progn
      (setq shell (vlax-create-object "Shell.Application"))
      (vlax-invoke shell 'Open filename)
      (vlax-release-object shell)
    )
  )
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaAri] ARITHMETIC ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "CROSSHAIRCOLOR->RGB" "12/08/2022" "Luna" "1.0.0" "\"BaAri\"")                 ;--•  Add the function's informations to $lst$ variable ;

;                                    []-----------------------[] CrosshairColor->RGB []-----------------------[]                                    ;
;--- Date of creation       > 12/08/2022                                                                                                            ;
;--- Last modification date > 12/08/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun CrosshairColor->RGB (/ cr oc )
  (setq
    cr (vla-get-ModelCrosshairColor (vla-get-Display (vla-get-Preferences (vlax-get-acad-object))))
    oc (vlax-variant-value (vlax-variant-change-type cr vlax-vbLong))
  )
  (LM:OLE->RGB oc)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "FIXDIV" "25/06/2020" "Luna" "1.0.1" "\"BaAri\"")                              ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] fixdiv []-----------------------[]                                           ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 25/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun fixdiv (n m / d)
  (cond
    ( (not
        (member
          nil
          (mapcar
            '(lambda (x) (= x (ascii "0")))
            (vl-string->list 
              (substr
                (vl-princ-to-string (setq d (/ n (atof (rtos m 2 1)))))
                (+ 2 (vl-string-position (ascii ".") (vl-princ-to-string d)))
              )
            )
          )
        )
      )
      (setq m (fix m))
    )
    ( T
      (if
        (not
          (member
            nil
            (mapcar
              '(lambda (x) (= x (ascii "0")))
              (vl-string->list
                (substr
                  (vl-princ-to-string (setq m (/ n (atof (rtos (1+ (fix d)) 2 1)))))
                  (+ 2 (vl-string-position (ascii ".") (vl-princ-to-string m)))
                )
              )
            )
          )
        )
        (setq m (fix m))
        (setq m (1+ (fix m)))
      )
    )
  )
  m
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ACI->OLE" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:ACI->OLE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:ACI->OLE ( c )
  (apply 'LM:RGB->OLE (LM:ACI->RGB c))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ACI->RGB" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:ACI->RGB []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:ACI->RGB ( c / o r )
  (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda ( )
            (vla-put-colorindex o c)
            (list (vla-get-red o) (vla-get-green o) (vla-get-blue o))
           )
        )
      )
      (vlax-release-object o)
      (if (vl-catch-all-error-p r)
        (prompt (strcat "\nError: " (vl-catch-all-error-message r)))
        r
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ACI->TRUE" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:ACI->True []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:ACI->True ( c / o r )
  (apply 'LM:RGB->True (LM:ACI->RGB c))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:OLE->ACI" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:OLE->ACI []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:OLE->ACI ( c )
  (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:OLE->RGB" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:OLE->RGB []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:OLE->RGB ( c )
  (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:OLE->TRUE" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:OLE->True []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:OLE->True ( c )
  (apply 'logior
    (mapcar
      '(lambda ( x ) (lsh (lsh (lsh (fix c) x) -24) (- x 8)))
      '(24 16 08)
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:RGB->ACI" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:RGB->ACI []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:RGB->ACI ( R G B / c o )
  (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
    (progn
      (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o R G B) (vla-get-colorindex o))))
      (vlax-release-object o)
      (if (vl-catch-all-error-p c)
        (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
        c
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:RGB->OLE" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:RGB->OLE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:RGB->OLE ( R G B )
  (logior (fix R) (lsh (fix G) 8) (lsh (fix B) 16))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:RGB->TRUE" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:RGB->True []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:RGB->True ( R G B )
  (logior (lsh (fix R) 16) (lsh (fix G) 8) (fix B))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:TRUE->ACI" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:True->ACI []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:True->ACI ( c / o r )
  (apply 'LM:RGB->ACI (LM:True->RGB c))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:TRUE->OLE" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:True->OLE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:True->OLE ( c )
  (apply 'logior
    (mapcar
      '(lambda ( x ) (lsh (lsh (lsh (fix c) x) -24) (- x 8)))
      '(08 16 24)
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:TRUE->RGB" "19/06/2014" "LeeMac" "1.4.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:True->RGB []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2014                                                                                                            ;
;--- Last modification date > 19/06/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:True->RGB ( c )
  (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(8 16 24))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:RAND" "05/03/2016" "LeeMac" "1.0.0" "\"BaAri\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] LM:rand []-----------------------[]                                          ;
;--- Date of creation       > 05/03/2016                                                                                                            ;
;--- Last modification date > 05/03/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:rand ( / a c m )
  (setq
    m   4294967296.0
    a   1664525.0
    c   1013904223.0
    $xn (rem (+ c (* a (cond ($xn) ((getvar "DATE"))))) m)
  )
  (/ $xn m)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:RANDRANGE" "05/03/2016" "LeeMac" "1.0.0" "\"BaAri\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:randrange []-----------------------[]                                       ;
;--- Date of creation       > 05/03/2016                                                                                                            ;
;--- Last modification date > 05/03/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:randrange ( a b )
  (+ (min a b) (fix (* (LM:rand) (1+ (abs (- a b))))))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ROUND" "28/08/2017" "LeeMac" "1.1.0" "\"BaAri\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] LM:round []-----------------------[]                                         ;
;--- Date of creation       > 28/08/2017                                                                                                            ;
;--- Last modification date > 28/08/2017                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:round ( n )
  (fix (+ n (if (minusp n) -0.5 0.5)))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ROUNDM" "28/08/2017" "LeeMac" "1.1.0" "\"BaAri\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] LM:roundm []-----------------------[]                                        ;
;--- Date of creation       > 28/08/2017                                                                                                            ;
;--- Last modification date > 28/08/2017                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun LM:roundm ( n m )
  (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "ROUND" "06/12/2021" "Luna" "1.0.0" "\"BaAri\"")                               ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] round []-----------------------[]                                           ;
;--- Date of creation       > 06/12/2021                                                                                                            ;
;--- Last modification date > 06/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun round (n p f)
  (cond
    ( (= p 0) (if (null f) (LM:round n)))
    ( (> p 0)
      (cond
        ( (null f) (LM:roundm n (expt 10.0 (- p))))
        ( (= f '<) ((lambda ( r ) (cond ((equal 0.0 r 1e-8) n) ((< n 0) (- n r p)) ((- n r)))) (rem n p)))
        ( (= f '>) ((lambda ( r ) (cond ((equal 0.0 r 1e-8) n) ((< n 0) (- n r)) ((+ n (- p r))))) (rem n p)))
        ( (= f T) (LM:roundm n p))
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SWAPBIT" "24/06/2022" "Luna" "1.0.0" "\"BaAri\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] SwapBit []-----------------------[]                                          ;
;--- Date of creation       > 24/06/2022                                                                                                            ;
;--- Last modification date > 24/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun SwapBit (a b)
  (- (logior b a) (logand b a))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "VP-SCALE" "29/12/2021" "Luna" "2.0.0" "\"BaAri\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] VP-scale []-----------------------[]                                         ;
;--- Date of creation       > 09/12/2021                                                                                                            ;
;--- Last modification date > 29/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaAri"                                                                                                               ;

(defun VP-scale (name / cvunits Heo Hep u xp)
  (defun cvunits (value from-unit to-unit / f)
    (defun f (u)
      (if (= (type u) 'INT)
        (cond
          ((= u 1) "inch")
          ((= u 2) "foot")
          ((= u 3) "mile")
          ((= u 4) "millimeter")
          ((= u 5) "centimeter")
          ((= u 6) "meter")
          ((= u 7) "kilometer")
          ((= u 8) "microinch")
          ((= u 9) "millipouce")
          ((= u 10) "yard")
          ((= u 11) "Angstrom")
          ((= u 12) "nanometer")
          ((= u 13) "micron")
          ((= u 14) "decimeter")
          ((= u 15) "dekameter")
          ((= u 16) "hectometer")
          ((= u 17) "gigameter")
          ((= u 18) "astronomical_unit")
          ((= u 19) "light_year")
          ((= u 20) "parsec")
          ((= u 21) "survey_foot")
        )
        u
      )
    )
    
    (cvunit value (f from-unit) (f to-unit))
  )

  (if (= "VIEWPORT" (cdr (assoc 0 (entget name))))
    (setq
      u (getvar "INSUNITS")
      Heo (float (cvunits (cdr (assoc 45 (entget name))) u 4))
      Hep (float (cvunits (cdr (assoc 41 (entget name))) u 6))
      xp (/ Hep Heo)
      xp (strcat "1/" (ThsdSpace (rtos (/ 1 xp) 2 0) " "))
    )
  )
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaEqc] EQUALITY AND CONDITIONNALS ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "BIT" "30/06/2022" "Luna" "1.0.0" "\"BaEqc\"")                                 ;--•  Add the function's informations to $lst$ variable ;

;                                            []-----------------------[] bit []-----------------------[]                                            ;
;--- Date of creation       > 30/06/2022                                                                                                            ;
;--- Last modification date > 30/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaEqc"                                                                                                               ;

(defun bit (a b)
  (= a (logand a b))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LST_EQUAL" "21/06/2020" "Luna" "1.1.1" "\"BaEqc\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] lst_equal []-----------------------[]                                         ;
;--- Date of creation       > 25/05/2019                                                                                                            ;
;--- Last modification date > 21/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.1                                                                                                                 ;
;--- Class                  > "BaEqc"                                                                                                               ;

(defun lst_equal (lst1 lst2)

  (if (= (vl-position (last lst1) lst1) (vl-position (last lst2) lst2))
    (while
      (and
        lst1
        lst2
        (= (type (car lst1)) (type (car lst2)))
        (= (car lst1) (car lst2))
      )
      (setq lst1 (cdr lst1)
            lst2 (cdr lst2)
      )
    )
  )
  (if (and
        (null lst1)
        (null lst2)
      )
    T
    nil
  )

)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "PT-MEMBER" "13/01/2022" "Luna" "2.0.0" "\"BaEqc\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] pt-member []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 13/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaEqc"                                                                                                               ;

(defun pt-member (pt pt-list fuzz)
  (setq pt (mapcar 'rtos pt))
  (car
    (member
      T
      (mapcar
        '(lambda (p)
          (apply
            'and
            (mapcar
              '(lambda (a b) (equal a b fuzz))
              pt
              (mapcar 'rtos p)
            )
          )
         )
        pt-list
      )
    )
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaErr] ERROR-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:CATCHAPPLY" "09/02/2015" "LeeMac" "1.2.0" "\"BaErr\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] LM:CatchApply []-----------------------[]                                       ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "BaErr"                                                                                                               ;

(defun LM:CatchApply ( fun prm / rtn )
  (if (not (vl-catch-all-error-p (setq rtn (vl-catch-all-apply fun prm))))
    rtn
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaFun] FUNCTION-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "FUNCTIONLOG" "11/07/2022" "Luna" "1.0.0" "\"BaFun\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] FunctionLog []-----------------------[]                                        ;
;--- Date of creation       > 11/07/2022                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaFun"                                                                                                               ;

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

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "MATOM" "31/12/2021" "Luna" "1.0.0" "\"BaFun\"")                               ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] mAtom []-----------------------[]                                           ;
;--- Date of creation       > 31/12/2021                                                                                                            ;
;--- Last modification date > 31/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaFun"                                                                                                               ;

(defun mAtom (a n f / l)
  (if (> n 0)
    (progn
      (repeat n
        (setq l (cons a l))
      )
      (setq a (vl-catch-all-apply f (reverse l)))
    )
    (setq a nil)
  )
  (if (and a (not (vl-catch-all-error-p a)))
    a
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaLst] LIST MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "2D-COORD->PT-LST" "##/##/####" "(gile)" "1.0.0" "\"BaLst\"")                  ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] 2D-coord->pt-lst []-----------------------[]                                     ;
;--- Date of creation       > ##/##/####                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > (gile)                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun 2D-coord->pt-lst (lst)
  (if lst (cons (list (car lst) (cadr lst)) (2D-coord->pt-lst (cddr lst))))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "3D-COORD->PT-LST" "##/##/####" "(gile)" "1.0.0" "\"BaLst\"")                  ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] 3D-coord->pt-lst []-----------------------[]                                     ;
;--- Date of creation       > ##/##/####                                                                                                            ;
;--- Last modification date > ##/##/####                                                                                                            ;
;--- Author                 > (gile)                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun 3D-coord->pt-lst (lst)
  (if lst (cons (list (car lst) (cadr lst) (caddr lst)) (3D-coord->pt-lst (cdddr lst))))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "2D-POINT" "12/08/2022" "Luna" "1.0.0" "\"BaLst\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] 2D-Point []-----------------------[]                                          ;
;--- Date of creation       > 12/08/2022                                                                                                            ;
;--- Last modification date > 12/08/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun 2D-Point (pt / lg)
  (cond
    ( (or (not (listp pt)) (not (setq lg (vl-list-length pt)))) nil)
    ( (= 3 lg) (reverse (cdr (reverse pt))))
    ( (= 2 lg) pt)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "DIVLIST" "16/06/2022" "Luna" "2.0.0" "\"BaLst\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] divlist []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 16/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun divlist (lst lng)
  (cond
    ( (or
        (null lng)
        (minusp lng)
      )
      lst
    )
    ( lst
      (cons
        (sublist lst 1 lng)
        (divlist (sublist lst (1+ lng) nil) lng)
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "DXF_LIST" "06/05/2022" "Luna" "1.2.2" "\"BaLst\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] DXF_List []-----------------------[]                                         ;
;--- Date of creation       > 27/05/2019                                                                                                            ;
;--- Last modification date > 06/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.2.2                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun DXF_List (lst str pos tri sup)
  (if tri
    (setq lst (sort-list (remove-duplicates lst) '<))
  )
  (cond
    ( (and str (= pos "left"))
      (setq lst (apply 'strcat (mapcar '(lambda (x) (strcat str (vl-princ-to-string x))) lst)))
      (if sup
        (setq lst (vl-string-left-trim str lst))
      )
    )
    ( (and str (= pos "right"))
      (setq lst (apply 'strcat (mapcar '(lambda (x) (strcat (vl-princ-to-string x) str)) lst)))
      (if sup
        (setq lst (vl-string-right-trim str lst))
      )
    )
  )
  lst
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "FORMASSOC" "30/06/2022" "Luna" "1.0.0" "\"BaLst\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] FormAssoc []-----------------------[]                                         ;
;--- Date of creation       > 30/06/2022                                                                                                            ;
;--- Last modification date > 30/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun FormAssoc (format lst flag)
  (if (bit 1 flag)
    (setq lst (mapcar '(lambda (x) (if (= 'STR (type (car x))) (cons (strcase (car x)) (cdr x)) x)) lst))
  )
  (setq lst
    (mapcar
      '(lambda (a / b key value)
        (setq b a)
        (cond
          ( (vl-symbolp a) (vl-symbol-value a))
          ( (not (listp a)) a)
          ( (while
              (and
                (car b)
                (not (or (listp (car b)) (vl-symbolp (car b))))
                (if (and (= 'STR (type (car b))) (bit 1 flag))
                  (setq key (strcase (car b)))
                  (setq key (car b))
                )
                (or
                  (null (setq value (cdr (assoc key lst))))
                  (if (bit 2 flag) (= "" value))
                )
                (setq b (cdr b))
              )
              (setq value nil)
            )
          )
          ( (or (listp (last a)) (vl-symbolp (last a)))
            (apply
              (last a)
              (list
                (cond
                  ( (bit 24 flag) (cons key value))
                  ( (bit 8 flag) value)
                  ( (bit 16 flag) key)
                )
              )
            )
          )
          ( (bit 24 flag) (cons key value))
          ( (bit 8 flag) value)
          ( (bit 16 flag) key)
        )
       )
      format
    )
  )
  (cond
    ( (member nil lst) nil)
    ( (bit 4 flag) (lst2str lst ""))
    (lst)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DXF-LIST" "23/05/2022" "Luna" "1.0.1" "\"BaLst\"")                        ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] get-DXF-list []-----------------------[]                                       ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 23/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun get-DXF-list (entlist code / n value lst)
  (setq n -1)
  (while (setq value (get-DXF-value entlist code (setq n (1+ n))))
    (setq lst (cons value lst))
  )
  (reverse lst)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DXF-VALUE" "05/02/2022" "Luna" "1.1.0" "\"BaLst\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] get-DXF-value []-----------------------[]                                       ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 05/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun get-DXF-value (entlist key pos)
  (if (and entlist (< 0 pos))
    (get-DXF-value (cdr (member (assoc key entlist) entlist)) key (1- pos))
    (cdr (assoc key entlist))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-PATTERN-LIST" "25/06/2020" "Luna" "3.0.0" "\"BaLst\"")                    ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] get-pattern-list []-----------------------[]                                     ;
;--- Date of creation       > 20/03/2020                                                                                                            ;
;--- Last modification date > 25/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun get-pattern-list (lst flag)
  (mapcar
    '(lambda (x / tag str value)
      (if (= "." (setq str (vl-string-trim "0123456789" (setq tag (vl-princ-to-string x)))))
        (setq str "")
      )
      (setq
        value
          (list
            (if (wcmatch tag "#*")
              (if (= str "")
                tag
                (substr tag 1 (vl-string-search (substr str 1 1) tag))
              )
              ""
            )
            (cond
              ((= flag 0) (strcase str))
              ((= flag 1) (strcase str t))
              (t str)
            )
            (if
              (and
                (/= str "")
                (wcmatch tag "*#")
              )
              (substr tag (1+ (strlen (vl-string-right-trim "0123456789" tag))))
              ""
            )
          )
      )
      (cons x (list value))
     )
    lst
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LOOP-A-LIST-PROPERTIES" "28/02/2022" "Luna" "2.0.0" "\"BaLst\"")              ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] loop-a-list-properties []-----------------------[]                                  ;
;--- Date of creation       > 07/01/2022                                                                                                            ;
;--- Last modification date > 28/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun loop-a-list-properties (jsel PropertyList value fun flag / i name lst)
  (if jsel
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        lst
          (make-a-list-properties
            lst
            (mapcar
              '(lambda (pp / ppType)
                (cdr
                  (GetAnyProperty
                    name
                    "*"
                    pp
                  )
                )
               )
              PropertyList
            )
            (eval value)
            fun
            flag
          )
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "MAKE-A-LIST-PROPERTIES" "04/02/2022" "Luna" "2.0.0" "\"BaLst\"")              ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] make-a-list-properties []-----------------------[]                                  ;
;--- Date of creation       > 06/01/2022                                                                                                            ;
;--- Last modification date > 04/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun make-a-list-properties (lst k-lst value fun flag / key search)
  (if (null (cdr k-lst))
    (if (setq search (assoc (setq key (car k-lst)) lst))
      (subst (cons key (apply fun (if flag (list value (cdr search)) (list (cdr search) value)))) search lst)
      (append lst (list (cons key value)))
    )
    (if (setq search (assoc (setq key (car k-lst)) lst))
      (subst (cons key (make-a-list-properties (cdr search) (cdr k-lst) value fun flag)) search lst)
      (append lst (list (cons key (make-a-list-properties (cdr search) (cdr k-lst) value fun flag))))
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "MSUBST" "04/01/2022" "Luna" "2.0.0" "\"BaLst\"")                              ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] msubst []-----------------------[]                                          ;
;--- Date of creation       > 06/05/2021                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun msubst (lst pos-lst entlist / new-item key pos tmp old-item)
  (foreach new-item lst
    (setq key (car new-item))
    (if pos-lst
      (setq
        pos (car pos-lst)
        pos-lst (cdr pos-lst)
      )
      (setq pos 0) 
    )
    (if
      (and
        (setq tmp (vl-remove-if-not '(lambda (x) (= (car x) key)) entlist))
        (setq old-item (nth pos tmp))
      )
      (setq entlist (subst new-item old-item entlist))
    )
  )
  entlist
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "PROMPT-LIST" "22/03/2021" "Luna" "1.0.0" "\"BaLst\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] prompt-list []-----------------------[]                                        ;
;--- Date of creation       > 08/03/2021                                                                                                            ;
;--- Last modification date > 22/03/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun prompt-list (lst fun / lstprompt i)
  (defun lstprompt (lst / l)
    (prompt (strcat "\n" (space (* (1- i) 3)) "("))
    (foreach l (if fun (vl-sort lst fun) lst)
      (if (and (listp l) (vl-list-length l))
        (progn (setq i (1+ i)) (lstprompt l) (setq i (1- i)))
        (prompt (strcat "\n" (space (* i 3)) (vl-prin1-to-string l)))
      )
    )
    (prompt (strcat "\n" (space (* (1- i) 3)) ")"))
  )

  (setq i 1)
  (lstprompt lst)
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "REMOVE-DUPLICATES" "29/04/2022" "Luna" "2.0.0" "\"BaLst\"")                   ;--•  Add the function's informations to $lst$ variable ;

;                                     []-----------------------[] remove-duplicates []-----------------------[]                                     ;
;--- Date of creation       > 18/02/2020                                                                                                            ;
;--- Last modification date > 29/04/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun remove-duplicates (lst)
  (if lst
    (cons (car lst) (remove-duplicates (vl-remove (car lst) (cdr lst))))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SORT-CONS" "25/06/2020" "Luna" "1.0.1" "\"BaLst\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] sort-cons []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 25/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun sort-cons (lst / tmp-list n)
  (if
    (and
      (not (member nil (mapcar 'vl-consp lst)))
      (null (vl-remove nil (mapcar 'vl-list-length lst)))
    )
    (progn
      (while lst
        (setq
          tmp-list (cons (cons (car lst) (length (vl-remove-if-not '(lambda (x) (equal x (car lst))) lst))) tmp-list)
          lst (vl-remove (car lst) lst)
        )
      )
      (setq
        tmp-list
          (vl-sort
            tmp-list
            '(lambda (e1 e2)
              (if (= (vl-princ-to-string (caar e1)) (vl-princ-to-string (caar e2)))
                (< (vl-princ-to-string (cdar e1)) (vl-princ-to-string (cdar e2)))
                (< (vl-princ-to-string (caar e1)) (vl-princ-to-string (caar e2)))
              )
             )
          )
      )
      (foreach n tmp-list
        (repeat (cdr n)
          (setq lst (cons (car n) lst))
        )
      )
      (setq lst (reverse lst))
    )
    lst
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SORT-LIST" "06/05/2022" "Luna" "1.3.1" "\"BaLst\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] sort-list []-----------------------[]                                         ;
;--- Date of creation       > 20/03/2020                                                                                                            ;
;--- Last modification date > 06/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.3.1                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun sort-list (lst fun)
  (mapcar 'car
    (vl-sort
      (get-pattern-list lst 2)
      '(lambda (a b)
        (setq a (cadr a) b (cadr b))
        (if (= (car a) (car b))
          (if (= (cadr a) (cadr b))
            ((eval fun) (atoi (caddr a)) (atoi (caddr b)))
            ((eval fun) (cadr a) (cadr b))
          )
          ((eval fun) (atoi (car a)) (atoi (car b)))
        )
       )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SUBLIST" "24/06/2020" "Luna" "1.1.0" "\"BaLst\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] sublist []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "BaLst"                                                                                                               ;

(defun sublist (lst s l)
  (repeat (1- s) (setq lst (cdr lst)))
  (setq lst (reverse lst))
  (if
    (or
      (null l)
      (minusp l)
      (not (<= l (- (length lst) (1- s))))
    )
    lst
    (repeat (- (length lst) l)
      (setq lst (cdr lst))
    )
  )
  (reverse lst)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaStr] STRING MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SPACE" "22/02/2021" "Luna" "1.0.0" "\"BaStr\"")                               ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] space []-----------------------[]                                           ;
;--- Date of creation       > 22/02/2021                                                                                                            ;
;--- Last modification date > 22/02/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

(defun space (n / str)
  (setq str "")
  (repeat n
    (setq str (strcat str " "))
  )
  str
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SUP-INCR" "24/01/2022" "Luna" "1.0.1" "\"BaStr\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] Sup-Incr []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

(defun Sup-Incr (value inc / Sup-Incr-Letter Sup-Incr-Number)
  (defun Sup-Incr-Letter (str inc / _incrementalpha a)
    (defun _incrementalpha (a b / c d e)
      (cond
        ( (cond
            ( (< 47 (setq c (car a)) 58)
              (setq
                d 48
                e 10
              )
            )
            ( (< 64 c 91)
              (setq
                d 65
                e 26
              )
            )
            ( (< 96 c 123)
              (setq
                d 97
                e 26
              )
            )
          )
          (setq
            c (+ (- c d) b)
            b (/ c e)
          )
          (if (not (minusp c))
            (cons
              (+ d (rem c e))
              (if (zerop b)
                (cdr a)
                (if (cdr a)
                  (_incrementalpha (cdr  a) b)
                  (_incrementalpha (list d) (if (= 10 e) b (1- b)))
                )
              )
            )
            (cons
              (+ d e (rem c e))
              (if (and (cdr a) (= (length (cdr a)) 1) (= (cadr a) d))
                '(0)
                (if (zerop b)
                  (_incrementalpha (cdr a) (1- b))
                  (_incrementalpha (list d) (if (= 10 e) b (1- b)))
                )
              )
            )
          )
        )
        ( (cons
            c
            (if (cdr a)
              (_incrementalpha (cdr a) b)
              (_incrementalpha (list 65) (1- b))
            )
          )
        )
      )
    )

    (vl-list->string
      (reverse
        (vl-remove
          0
          (if (setq a (reverse (vl-string->list str)))
            (_incrementalpha a inc)
            (_incrementalpha '(65) (1- inc))
          )
        )
      )
    )
  )

  (defun Sup-Incr-Number (str inc / _rtos _decimalplaces num incd maxd slen strd)
    (defun _rtos (r p / d v)
      (setq d (getvar "DIMZIN"))
      (setvar "DIMZIN" 0)
      (setq v (rtos r 2 p))
      (setvar "DIMZIN" d)
      v
    )

    (defun _decimalplaces (str / pos)
      (if (setq pos (vl-string-position 46 str))
        (- (strlen str) pos 1)
        0
      )
    )
      
    (setq num (+ (distof str) (distof inc)))
    (if (minusp (distof str))
      (setq str (substr str 2))
    )
    (if (minusp (distof inc))
      (setq inc (substr inc 2))
    )
    (setq
      incd (_decimalplaces inc)
      strd (_decimalplaces str)
      maxd (max incd strd)
      slen (strlen str)
    )
    (cond
      ( (and (< 0 strd) (< 0 incd))
        (setq slen (+ (- slen strd) maxd))
      )
      ( (and (= 0 strd) (< 0 incd))
        (setq slen (+ incd slen 1))
      )
    )
    (setq str (_rtos num maxd))
    (if (minusp num)
      (setq str (substr str 2))
    )
    (while (< (strlen str) slen)
      (setq str (strcat "0" str))
    )
    (if (minusp num)
      (strcat "-" str)
      str
    )
  )

  (if (distof value 2)
    (Sup-Incr-Number value (vl-princ-to-string inc))
    (Sup-Incr-Letter value (fix inc))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "THSDSPACE" "29/12/2021" "Luna" "1.0.0" "\"BaStr\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] ThsdSpace []-----------------------[]                                         ;
;--- Date of creation       > 29/12/2021                                                                                                            ;
;--- Last modification date > 29/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

(defun ThsdSpace (str sep / pos n)
  (cond
    ( (and
        (setq n (distof str))
        (setq pos (vl-string-search "." str))
      )
      (strcat (ThsdSpace (substr str 1 pos) sep) (substr str (1+ pos)))
    )
    ( (and
        n
        (setq n (if (minusp n) 4 3))
        (> (strlen str) n)
      )
      (strcat
        (ThsdSpace (substr str 1 (- (strlen str) 3)) sep)
        sep
        (substr str (1+ (- (strlen str) 3)))
      )
    )
    ( (and
        n
        (<= (strlen str) n)
      )
      str
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "WILDCARD-TRIM" "10/12/2021" "Luna" "1.0.0" "\"BaStr\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] Wildcard-trim []-----------------------[]                                       ;
;--- Date of creation       > 10/12/2021                                                                                                            ;
;--- Last modification date > 10/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

(defun wildcard-trim (w / i lst a)
  (repeat (setq i 256)
    (if (wcmatch (setq a (chr (setq i (1- i)))) w)
      (setq lst (cons a lst))
    )
  )
  lst
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BaSym] SYMBOL-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;


;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtDac] DATA CONVERSION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "ATOL" "05/01/2021" "Luna" "1.1.0" "\"UtDac\"")                                ;--•  Add the function's informations to $lst$ variable ;

;                                            []-----------------------[] atol []-----------------------[]                                           ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 05/01/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

(defun atol (str)
  (mapcar 'chr (vl-string->list (vl-princ-to-string str)))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:SAFEARRAYVARIANT" "09/02/2015" "LeeMac" "1.2.0" "\"UtDac\"")               ;--•  Add the function's informations to $lst$ variable ;

;                                    []-----------------------[] LM:SafearrayVariant []-----------------------[]                                    ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

(defun LM:SafearrayVariant ( typ lst )
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray typ (cons 0 (1- (length lst))))
      lst
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LST2STR" "01/02/2022" "Luna" "1.1.0" "\"UtDac\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] lst2str []-----------------------[]                                          ;
;--- Date of creation       > 05/03/2021                                                                                                            ;
;--- Last modification date > 01/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

(defun lst2str (lst sep)
  (if lst
    (vl-string-left-trim
      sep
      (apply
        'strcat
        (mapcar '(lambda (x) (strcat sep (vl-princ-to-string x))) lst)
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "STR2LST" "15/04/2017" "(gile)" "1.0.0" "\"UtDac\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] str2lst []-----------------------[]                                          ;
;--- Date of creation       > 15/04/2017                                                                                                            ;
;--- Last modification date > 15/04/2017                                                                                                            ;
;--- Author                 > (gile)                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

(defun str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons
      (substr str 1 pos)
      (str2lst (substr str (+ (strlen sep) pos 1)) sep)
    )
    (list str)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SLICENUMBER" "13/01/2022" "Luna" "2.0.0" "\"UtDac\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] SliceNumber []-----------------------[]                                        ;
;--- Date of creation       > 29/12/2021                                                                                                            ;
;--- Last modification date > 13/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

(defun SliceNumber (n / p)
  (if (distof n)
    (if (setq p (vl-string-search "." n))
      (cons
        (atoi (substr n 1 p))
        (atoi (substr n (+ p 2)))
      )
      (cons (atoi n) 0)
    )
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtDev] DEVICE ACCESS ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtDis] DISPLAY CONTROL ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "_PRINC" "##/##/2005" "Michael_Puckett" "1.0.0" "\"UtDis\"")                   ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] _princ []-----------------------[]                                          ;
;--- Date of creation       > ##/##/2005                                                                                                            ;
;--- Last modification date > ##/##/2005                                                                                                            ;
;--- Author                 > Michael_Puckett                                                                                                       ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun _princ (str)
  (princ str)
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "ALERT-PRINC" "26/01/2022" "Luna" "1.0.0" "\"UtDis\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] alert-princ []-----------------------[]                                        ;
;--- Date of creation       > 26/01/2022                                                                                                            ;
;--- Last modification date > 26/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun alert-princ (msg)
  (alert msg)
  (princ msg)
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GRCIRCLE" "16/09/2022" "Luna" "3.1.0" "\"UtDis\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] grcircle []-----------------------[]                                          ;
;--- Date of creation       > 12/08/2022                                                                                                            ;
;--- Last modification date > 16/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.1.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

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

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:GRSNAP:DISPLAYSNAP" "21/08/2014" "LeeMac" "1.0.0" "\"UtDis\"")             ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] LM:grsnap:displaysnap []-----------------------[]                                   ;
;--- Date of creation       > 21/08/2014                                                                                                            ;
;--- Last modification date > 21/08/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun LM:grsnap:displaysnap ( pnt lst col / scl )
  (setq
    scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
    pnt (trans pnt 1 2)
  )
  (grvecs
    (cons col lst)
    (list
      (list scl 0.0 0.0 (car  pnt))
      (list 0.0 scl 0.0 (cadr pnt))
      (list 0.0 0.0 scl 0.0)
      '(0.0 0.0 0.0 1.0)
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:GRSNAP:PARSEPOINT" "21/08/2014" "LeeMac" "1.0.0" "\"UtDis\"")              ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] LM:grsnap:parsepoint []-----------------------[]                                    ;
;--- Date of creation       > 21/08/2014                                                                                                            ;
;--- Last modification date > 21/08/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
  (defun str->lst ( str / pos )
    (if (setq pos (vl-string-position 44 str))
      (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
      (list str)
    )
  )

  (if (wcmatch str "`@*")
    (setq str (substr str 2))
    (setq bpt '(0.0 0.0 0.0))
  )           
  (if
    (and
      (setq lst (mapcar 'distof (str->lst str)))
      (vl-every 'numberp lst)
      (< 1 (length lst) 4)
    )
    (mapcar '+ bpt lst)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:GRSNAP:SNAPFUNCTION" "21/08/2014" "LeeMac" "1.0.0" "\"UtDis\"")            ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] LM:grsnap:snapfunction []-----------------------[]                                   ;
;--- Date of creation       > 21/08/2014                                                                                                            ;
;--- Last modification date > 21/08/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun LM:grsnap:snapfunction ( )
  (eval
    (list
      'lambda
      '( p o / q )
      (list
        'if
        '(zerop (logand 16384 o))
        (list
          'if
          '(setq q
            (cdar
              (vl-sort
                (vl-remove-if
                  'null
                  (mapcar
                    (function
                      (lambda ( a / b )
                        (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                          (list (distance p b) b (car a))
                        )
                      )
                    )
                    '(
                      (0001 . "_end")
                      (0002 . "_mid")
                      (0004 . "_cen")
                      (0008 . "_nod")
                      (0016 . "_qua")
                      (0032 . "_int")
                      (0064 . "_ins")
                      (0128 . "_per")
                      (0256 . "_tan")
                      (0512 . "_nea")
                      (2048 . "_app")
                      (8192 . "_par")
                    )
                  )
                )
                '(lambda ( a b ) (< (car a) (car b)))
              )
            )
          )
          (list
            'LM:grsnap:displaysnap
            '(car q)
            (list
              'cdr
              (list
                'assoc
                '(cadr q)
                (list
                  'quote
                  (LM:grsnap:snapsymbols (atoi (cond ((getenv "AutoSnapSize")) ("5"))))
                )
              )
            )
            (LM:OLE->ACI
              (if (= 1 (getvar 'cvport))
                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
              )
            )
          )
        )
      )
      '(cond ((car q)) (p))
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:GRSNAP:SNAPMODE" "21/08/2014" "LeeMac" "1.0.0" "\"UtDis\"")                ;--•  Add the function's informations to $lst$ variable ;

;                                    []-----------------------[] LM:grsnap:snapmode []-----------------------[]                                     ;
;--- Date of creation       > 21/08/2014                                                                                                            ;
;--- Last modification date > 21/08/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun LM:grsnap:snapmode ( str )
  (vl-some
    (function
      (lambda ( x )
        (if (wcmatch (car x) (strcat (strcase str t) "*"))
          (progn
            (princ (cadr x)) (caddr x)
          )
        )
      )
    )
    '(
      ("endpoint"      " of " 00001)
      ("midpoint"      " of " 00002)
      ("center"        " of " 00004)
      ("node"          " of " 00008)
      ("quadrant"      " of " 00016)
      ("intersection"  " of " 00032)
      ("insert"        " of " 00064)
      ("perpendicular" " to " 00128)
      ("tangent"       " to " 00256)
      ("nearest"       " to " 00512)
      ("appint"        " of " 02048)
      ("parallel"      " to " 08192)
      ("none"          ""     16384)
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:GRSNAP:SNAPSYMBOLS" "21/08/2014" "LeeMac" "1.0.0" "\"UtDis\"")             ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] LM:grsnap:snapsymbols []-----------------------[]                                   ;
;--- Date of creation       > 21/08/2014                                                                                                            ;
;--- Last modification date > 21/08/2014                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDis"                                                                                                               ;

(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
  (setq
    -p (- p)
    q (1+  p)
    -q (- q)
    r (+ 2 p)
    -r (- r)
    i (/ pi 6.0)
    a 0.0
  )
  (repeat 12
    (setq
      l (cons (list (* r (cos a)) (* r (sin a))) l)
      a (- a i)
    )
  )
  (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
  (list
    (list 1
      (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
    )
    (list 2
      (list -r -q) (list 0  r) (list 0  r) (list r -q)
      (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
    )
    (cons 4 c)
    (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
    (list 16
      (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
      (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
      (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
    )
    (list 32
      (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
      (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
    )
    (list 64
      '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
      '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
      '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
      '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
    )
    (list 128
      (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
      (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
      (list -p q) (list -p -p) (list -p -p) (list q -p)
      (list -q q) (list -q -q) (list -q -q) (list q -q)
    )
    (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
    (list 512
      (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
      (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
    )
    (list 2048
      (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
      (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
      (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
      (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
      (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
      (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
    )
    (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtFil] FILE-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "APPLYDOCSMETHODS" "05/07/2022" "Glenn_White/Luna" "2.0.0" "\"UtFil\"")        ;--•  Add the function's informations to $lst$ variable ;

;                                     []-----------------------[] ApplyDocsMethods []-----------------------[]                                      ;
;--- Date of creation       > 16/01/2008                                                                                                            ;
;--- Last modification date > 05/07/2022                                                                                                            ;
;--- Author                 > Glenn_White/Luna                                                                                                      ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtFil"                                                                                                               ;

(defun ApplyDocsMethods (docs lst / col i name tmp)
  (setq
    col (vla-get-Documents (vlax-get-acad-object))
    i (vla-get-Count col)
    docs (if (listp docs) (mapcar 'strcase docs) docs)
  )
  (vlax-for Item col
    (and
      (setq name (vla-get-Name Item))
      (or
        (null docs)
        (and (listp docs) (member (strcase name) docs))
        (and (vl-symbolp docs) (= :vlax-False (vla-get-Active Item)))
      )
      (setq lst
        (vl-remove-if-not
          '(lambda (m)
            (and
              (vlax-method-applicable-p Item (car m))
              (not
                (vl-catch-all-error-p
                  (vl-catch-all-apply
                    'vlax-invoke-method
                    (append
                      (list Item (car m))
                      (vl-remove nil (list (cdr m)))
                    )
                  )
                )
              )
            )
          )
          lst
        )
      )
      (setq tmp (cons (cons name lst) tmp))
    )
  )
  (cons i tmp)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "READ-FILE" "06/07/2022" "Luna" "1.0.1" "\"UtFil\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] read-file []-----------------------[]                                         ;
;--- Date of creation       > 27/12/2021                                                                                                            ;
;--- Last modification date > 06/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFil"                                                                                                               ;

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


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtGeo] GEOMETRIC ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-PT-LIST" "31/12/2021" "Luna" "3.0.0" "\"UtGeo\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] get-pt-list []-----------------------[]                                        ;
;--- Date of creation       > 08/03/2019                                                                                                            ;
;--- Last modification date > 31/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "UtGeo"                                                                                                               ;

(defun get-pt-list (name / rp f o s e i l)
  (defun rp (p f)
    (mapcar '(lambda (c) (if (equal c 0.0 f) 0.0 c)) p)
  )
  
  (and
    name
    (setq f 1e-15)
    (setq o (cdr (assoc 0 (entget name))))
    (member o '("LWPOLYLINE" "POLYLINE" "LINE" "SPLINE" "ARC" "CIRCLE" "ELLIPSE"))
    (setq s (vlax-curve-getStartParam name))
    (setq e (vlax-curve-getEndParam name))
    (cond
      ( (member o '("LWPOLYLINE" "POLYLINE"))
        (repeat (setq i (1+ (fix e)))
          (setq l (cons (rp (vlax-curve-getPointAtParam name (setq i (1- i))) f) l))
        )
      )
      ( (member o '("LINE" "SPLINE"))
        (setq l
          (list
            (rp (vlax-curve-getPointAtParam name s) f)
            (rp (vlax-curve-getPointAtParam name e) f)
          )
        )
      )
      ( (member o '("ARC" "CIRCLE" "ELLIPSE"))
        (setq l
          (list
            (rp (cdr (assoc 10 (entget name))) f)
            (rp (vlax-curve-getPointAtParam name s) f)
            (rp (vlax-curve-getPointAtParam name e) f)
          )
        )
      )
    )
  )
  l
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "OSNAP-POLY" "04/01/2022" "Luna" "3.0.0" "\"UtGeo\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] osnap-poly []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "UtGeo"                                                                                                               ;

(defun osnap-poly (pt-list pt fuzz / dist p)
  (setq pt (trans pt 1 0))
  (foreach p pt-list
    (setq dist (cons (cons (distance p pt) p) dist))
  )
  (setq pt (assoc (apply 'min (mapcar 'car dist)) dist))
  (if
    (or
      (null fuzz)
      (<= (car pt) fuzz)
    )
    (cdr pt)
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtCom] QUERY AND COMMAND ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "BINARYVARSWAP" "27/05/2022" "Luna" "1.0.0" "\"UtCom\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] BinaryVarSwap []-----------------------[]                                       ;
;--- Date of creation       > 27/05/2022                                                                                                            ;
;--- Last modification date > 27/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

(defun BinaryVarSwap (var / val)
  (and
    (setq val (getvar var))
    (numberp val)
    (member val '(0 1))
    (setvar var (- 1 val))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SETVARLIST" "15/06/2022" "Luna" "2.0.0" "\"UtCom\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] SetVarList []-----------------------[]                                        ;
;--- Date of creation       > 13/06/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

(defun SetVarList (lst)
  (mapcar
    '(lambda (x / var sym val)
      (setq
        var (car x)
        sym (cadr x)
        val (caddr x)
      )
      (if (vl-symbolp sym)
        (set sym (getvar var))
        (setq sym (getvar var))
      )
      (if val
        (setq val (vl-catch-all-apply 'setvar (list var val)))
        (setq val (getvar var))
      )
      (list (strcase var) sym val)
     )
    lst
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "UNITS" "23/06/2022" "Luna" "1.0.0" "\"UtCom\"")                               ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] units []-----------------------[]                                           ;
;--- Date of creation       > 23/06/2022                                                                                                            ;
;--- Last modification date > 23/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtCom"                                                                                                               ;

(defun units (u)
  (cond
    ((= u 1)  "in")
    ((= u 2)  "ft")
    ((= u 3)  "mi")
    ((= u 4)  "mm")
    ((= u 5)  "cm")
    ((= u 6)  "m")
    ((= u 7)  "km")
    ((= u 8)  "µin")
    ((= u 9)  "min")
    ((= u 10) "yd")
    ((= u 11) "Å")
    ((= u 12) "nm")
    ((= u 13) "µm")
    ((= u 14) "dm")
    ((= u 15) "dam")
    ((= u 16) "hm")
    ((= u 17) "Gm")
  )
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtMem] MEMORY MANAGEMENT ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtUse] USER INPUT ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GETKDH" "15/06/2022" "Luna" "2.2.0" "\"UtUse\"")                              ;--•  Add the function's informations to $lst$ variable ;

;                                           []-----------------------[] getkdh []-----------------------[]                                          ;
;--- Date of creation       > 26/01/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.2.0                                                                                                                 ;
;--- Class                  > "UtUse"                                                                                                               ;

(defun getkdh (fun pfx arg sfx dft hlp / get bit kwd msg val)
  (defun get (msg / v)
    (apply 'initget arg)
    (if (null (setq v (apply (car fun) (vl-remove nil (mapcar '(lambda (x) (if (vl-symbolp x) (vl-symbol-value x) x)) (cdr fun))))))
      (setq v (cdr dft))
      v
    )
  )
  
  (and
    (member (car fun) (list 'getint 'getreal 'getdist 'getangle 'getorient 'getpoint 'getcorner 'getkword 'entsel 'nentsel 'nentselp))
    (= 'STR (type (cond (pfx) (""))) (type (cond (sfx) (""))))
    (listp arg)
    (if (null (setq bit (car (vl-remove-if-not '(lambda (x) (= 'INT (type x))) arg)))) (setq bit 0) bit)
    (if (null (setq kwd (car (vl-remove-if-not '(lambda (x) (= 'STR (type x))) arg))))
      (not kwd)
      (if (vl-string-search "_" kwd)
        (setq kwd (mapcar 'cons (str2lst (car (str2lst kwd "_")) " ") (str2lst (cadr (str2lst kwd "_")) " ")))
        (setq kwd (mapcar 'cons (str2lst kwd " ") (str2lst kwd " ")))
      )
    )
    (if hlp
      (if (not (assoc "?" kwd))
        (setq kwd (append kwd '(("?" . "?"))))
        T
      )
      T
    )
    (cond
      ( (null dft) (not dft))
      ( (member dft (mapcar 'car kwd))
        (setq dft (assoc dft kwd))
      )
      ( (member dft (mapcar 'cdr kwd))
        (setq dft (nth (vl-position (car (member dft (mapcar 'cdr kwd))) (mapcar 'cdr kwd)) kwd))
      )
      ( T (setq dft (cons (vl-princ-to-string dft) dft)))
    )
    (if (and dft (= 1 (logand 1 bit))) (setq bit (1- bit)) T)
    (setq arg (vl-remove nil (list bit (lst2str (vl-remove nil (list (lst2str (mapcar 'car kwd) " ") (lst2str (mapcar 'cdr kwd) " "))) "_"))))
    (if (or pfx kwd dft sfx)
      (setq
        msg
          (strcat
            (cond (pfx) (""))
            (if kwd (strcat " [" (lst2str (mapcar 'car kwd) "/") "]") "")
            (if dft (strcat " <" (car dft) ">") "")
            (cond (sfx) (""))
          )
      )
      (not (setq msg nil))
    )
    (if hlp
      (while (= "?" (setq val (get msg)))
        (cond
          ( (listp hlp) (eval hlp))
          ((princ hlp))
        )
      )
      (setq val (get msg))
    )
  )
  val
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtWin] WINDOWS REGISTRY ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DATE" "19/06/2020" "Luna" "1.0.0" "\"UtWin\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] get-Date []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 19/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtWin"                                                                                                               ;

(defun get-Date (/ r d h)

  (setq
    r (rtos (getvar "CDATE") 2 6)
    d (strcat (substr r 7 2) "/" (substr r 5 2) "/" (substr r 1 4))
    h (strcat (substr r 10 2) ":" (substr r 12 2) ":" (substr r 14 2))
    r (strcat d " - " h)
  )

)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LGT" "12/05/2022" "Luna" "1.1.0" "\"UtWin\"")                                 ;--•  Add the function's informations to $lst$ variable ;

;                                            []-----------------------[] LgT []-----------------------[]                                            ;
;--- Date of creation       > 10/01/2022                                                                                                            ;
;--- Last modification date > 12/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtWin"                                                                                                               ;

(defun LgT (en fr flag)
  (cond
    ( (null flag) (LgT en fr (if (getenv "FORCEDLANGUAGE") (atoi (getenv "FORCEDLANGUAGE")) 0)))
    ( (= flag 1) en)
    ( (= flag 2) fr)
    ( T (LgT en fr (if (= (getvar "LOCALE") "FR") 2 1)))
  )
)



;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DtXdt] EXTENDED DATA-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DtObj] OBJECT-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "_PUTPREFIX" "14/01/2021" "Luna" "1.0.1" "\"DtObj\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] _putprefix []-----------------------[]                                        ;
;--- Date of creation       > 08/08/2018                                                                                                            ;
;--- Last modification date > 14/01/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun _putprefix (name txt)
  (if
    (and
      (= 'ename (type name))
      (vlax-property-available-p (setq name (vlax-ename->vla-object name)) 'textprefix)
      (vlax-write-enabled-p name)
    )
    (progn
      (vla-put-textprefix name txt)
      T
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "_PUTSUFFIX" "14/01/2021" "Luna" "1.0.1" "\"DtObj\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] _putsuffix []-----------------------[]                                        ;
;--- Date of creation       > 08/08/2018                                                                                                            ;
;--- Last modification date > 14/01/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun _putsuffix (name txt)
  (if
    (and
      (= 'ename (type name))
      (vlax-property-available-p (setq name (vlax-ename->vla-object name)) 'textsuffix)
      (vlax-write-enabled-p name)
    )
    (progn
      (vla-put-textsuffix name txt)
      T
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "ADD-POLY2D-POINT" "10/05/2022" "Luna" "2.0.0" "\"DtObj\"")                    ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] Add-Poly2D-Point []-----------------------[]                                     ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 10/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun Add-Poly2D-Point (name Start-pt Add-pt / entlist pt-list pos add)
  (and
    (= (cdr (assoc 0 (entget name))) "LWPOLYLINE")
    (setq entlist (entget name))
    (setq pt-list (get-pt-list name))
    (setq Start-pt (osnap-poly pt-list Start-pt 1E2))
    (setq pos (+ 5 (- (length entlist) (length (member Start-pt (mapcar 'cdr entlist))))))
    (setq add  
      (list  
        (assoc 40 (sublist entlist (- pos 4) nil))
        (assoc 41 (sublist entlist (- pos 4) nil))
        (assoc 42 (sublist entlist (- pos 4) nil))
        (assoc 91 (sublist entlist (- pos 4) nil))
      )
    )
    (setq entlist
      (entmod
        (append
          (sublist entlist 1 pos)
          (append
            (list
              (cons
                10
                (if (/= 2 (length Add-pt))
                  (list (car Add-pt) (cadr Add-pt))
                  Add-pt
                )
              )
            )
            add
          )
          (sublist entlist (1+ pos) nil)
        )
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "ARRAY-DEF" "06/09/2021" "Luna" "2.0.0" "\"DtObj\"")                           ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] Array-Def []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 06/09/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun Array-Def (name / i ent Array-Properties Array-Definition Array-ItemList ItemList ent-lst lst n p)

  (if
    (and
      (= (cdr (assoc 0 (entget name))) "INSERT")
      (wcmatch (getpropertyvalue name "Classname") "AcDbAssociative*Array")
    )
    (progn
      (setq
        Array-properties (entget (cdr (assoc 330 (entget (cdr (assoc 330 (entget name)))))))
        Array-ItemList (entget (cdr (assoc 360 Array-Properties)))
        i (get-DXF-value Array-ItemList 90 4)
      )
      (while (setq Array-ItemList (member '(100 . "AcDbAssocArrayItem") Array-ItemList))
        (setq
          n
            (strcat
              (itoa (get-DXF-value Array-ItemList 90 2))
              ","
              (itoa (get-DXF-value Array-ItemList 90 3))
              ","
              (itoa (get-DXF-value Array-ItemList 90 4))
            )
          p
            (cond
              ((= (logand (get-DXF-value Array-ItemList 90 5) 1) 1) 0)
              ((= (logand (get-DXF-value Array-ItemList 90 5) 8) 8) 1)
            )
          ItemList (cons (cons n p) ItemList)
          Array-ItemList (cdr Array-ItemList)
        )
      )
      (setq
        Array-definition (tblsearch "BLOCK" (cdr (assoc 2 (entget (cdr (assoc -2 (tblsearch "BLOCK" (cdr (assoc 2 (entget name))))))))))
        ent (cdr (assoc -2 Array-Definition))
        lst
          (list
            (cons "TotalObject" (apply '+ (mapcar 'cdr ItemList)))
            (cons "ColumnSpacing" (cdr (assoc 40 (member '(1 . "ItemSpacing") Array-Properties))))
            (cons "Columns" (cdr (assoc 90 (cdddr (member '(1 . "Items") Array-Properties)))))
            (cons "RowSpacing" (cdr (assoc 40 (member '(1 . "RowSpacing") Array-Properties))))
            (cons "Rows" (cdr (assoc 90 (cdddr (member '(1 . "Rows") Array-Properties)))))
            (cons "LevelSpacing" (cdr (assoc 40 (member '(1 . "LevelSpacing") Array-Properties))))
            (cons "Levels" (cdr (assoc 90 (cdddr (member '(1 . "Levels") Array-Properties)))))
          )
      )
      (while ent
        (setq
          ent-lst (cons ent ent-lst)
          ent (entnext ent)
        )
      )
      (setq lst (append lst (list (cons 90 (length ent-lst)) (cons 330 ent-lst))))
    )
  )
  lst

)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "EXPLORE-DXF" "16/12/2022" "Luna" "3.0.1" "\"DtObj\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] Explore-DXF []-----------------------[]                                        ;
;--- Date of creation       > 22/02/2021                                                                                                            ;
;--- Last modification date > 16/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.1                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun Explore-DXF (entlist n lst flag / prompt-entlist filename file)
  (defun prompt-entlist (entlist str-list / eType eName DXFcode eList DXFpath i str)
    (mapcar
      'set
      (list 'eType 'eName 'DXFcode 'eList 'DXFpath 'i)
      str-list
    )
    (cond
      ((= (length str-list) 2)
        (setq i '(0)
              eList (list eType)
              DXFpath '(0)
              str
          (strcat  "Explore-DXF in progress from \""
            eType
            "\" ("
            (vl-prin1-to-string eName)
            ") : "
          )
        )
      )
      (t
        (setq eList (append eList (list eType))
              DXFpath (append DXFpath (list ""))
              i (append i (list 1))
              str
          (strcat  (apply 'strcat (mapcar '(lambda (s) (strcat (itoa s) ".")) i))
            "  \""
            eType
            "\" ("
            (vl-prin1-to-string eName)
            ") at "
            (itoa DXFcode)
            " code, from : "
            "\n  "
            (apply 'strcat (mapcar '(lambda (e c) (strcat "-> " e (if (numberp c) (strcat " (" (itoa c) ") ") ""))) eList DXFpath))
          )
        )
      )
    )
    (if (and flag
       file
       str
        )
      (write-line (strcat "\n" str "\n") file)
      (prompt  (strcat  "\n" str "\n|"))
    )
    (mapcar
      '(lambda (l)
        (setq str (strcat (space (+ (* (length i) 2) 4)) (vl-prin1-to-string l)))
        (if (and flag
           file
           str
            )
          (write-line str file)
          (prompt  (strcat  str "\n|"))
        )
       )
      entlist
    )
    (if (<= (length i) n)
      (progn  
        (foreach l entlist
          (if (and
            (member (car l) lst)
            (= (type (cdr l)) 'ENAME)
            (setq entlist (entget (cdr l)))
            (setq i (subst (1+ (last i)) (last i) i))
            (setq eList (subst eType (last eList) eList))
            (setq DXFpath (subst (car l) (last DXFpath) DXFpath))
              )
            (prompt-entlist
              entlist
              (list  (cdr (assoc 0 entlist))
                (cdr (assoc -1 entlist))
                (car l)
                eList
                DXFpath
                i
              )
            )
          )
        )
      )
      (setq i (reverse (cdr (reverse i)))
            eList (reverse (cdr (reverse eList)))
            DXFpath (reverse (cdr (reverse DXFpath)))
      )
    )
    (princ)
  )

  (if flag
    (setq filename
      (getfiled "Save file - DXF Export"
          (strcat (getvar "DWGPREFIX") (rtos (getvar "CDATE") 2 0) "_" (vl-filename-base (getvar "DWGNAME")) " - " (cdr (assoc 0 entlist)) ".txt")
          "txt"
          45
      )
          file (open filename "w")
    )
  )
  (prompt-entlist
    entlist
    (list  (cdr (assoc 0 entlist))
      (cdr (assoc -1 entlist))
    )
  )
  (if (and flag file)
    (progn
      (write-line "\nEnd of exploration..." file)
      (close file)
      (prompt "\nFile has been created !")
      (startapp "NotePad" filename)
    )
    (prompt "\nEnd of exploration...")
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GETANYPROPERTY" "05/02/2022" "Luna" "2.0.0" "\"DtObj\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] GetAnyProperty []-----------------------[]                                      ;
;--- Date of creation       > 30/12/2021                                                                                                            ;
;--- Last modification date > 05/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun GetAnyProperty (name search key / getName getProperty value)
  (defun getName (name flag)
    (cond
      ( (= flag 0) (cdr (assoc 0 (entget name))))
      ( (= flag 1) (vla-get-ObjectName (vlax-ename->vla-object name)))
      ( (= flag 2) (vla-get-ObjectName (vlax-ename->vla-object name)))
    )
  )
  
  (defun getProperty (name flag key / value)
    (cond
      ( (= flag 0)
        (setq key (SliceNumber (vl-princ-to-string key)))
        (if (setq value (get-DXF-value (entget name) (car key) (cdr key)))
          (cons (car key) value)
        )
      )
      ( (= flag 1)
        (if (listp key)
          (if (null (vl-catch-all-error-p (setq value (vl-catch-all-apply 'getpropertyvalue (append (list name) key)))))
            (cons key value)
          )
          (if (null (vl-catch-all-error-p (setq value (vl-catch-all-apply 'getpropertyvalue (list name key)))))
            (cons key value)
          )
        )
      )
      ( (= flag 2)
        (if (null (vl-catch-all-error-p (setq value (vl-catch-all-apply 'vlax-get (list (vlax-ename->vla-object name) key)))))
          (cons key value)
        )
      )
    )
  )
  
  (and
    name
    (setq name (ConvName name 'ENAME))
    (setq flag
      (cond
        ( (vl-symbolp key) 2)
        ( (numberp key) 0)
        ( (or (listp key) (= 'STR (type key))) 1)
      )
    )
    (wcmatch (strcase (getName name flag)) (strcase search))
    (setq value (getProperty name flag key))
  )
  value
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SETANYPROPERTY" "05/02/2022" "Luna" "2.0.0" "\"DtObj\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] SetAnyProperty []-----------------------[]                                      ;
;--- Date of creation       > 29/12/2021                                                                                                            ;
;--- Last modification date > 05/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtObj"                                                                                                               ;

(defun SetAnyProperty (name search key value / getName setProperty)
  (defun getName (name flag)
    (cond
      ( (= flag 0) (cdr (assoc 0 (entget name))))
      ( (= flag 1) (vla-get-ObjectName (vlax-ename->vla-object name)))
      ( (= flag 2) (vla-get-ObjectName (vlax-ename->vla-object name)))
    )
  )
  
  (defun setProperty (name flag key value)
    (cond
      ((= flag 0)
        (vl-catch-all-apply
          'entmod
          (list
            (if
              (and
                (setq key (SliceNumber (vl-princ-to-string key)))
                (get-DXF-value (entget name) (car key) (cdr key))
              )
              (subst (cons (car key) value) (cons key (get-DXF-value (entget name) (car key) (cdr key))) (entget name))
              (append (entget name) (list (cons (car key) value)))
            )
          )
        )
      )
      ((= flag 1)
        (if (listp key)
          (vl-catch-all-apply 'setpropertyvalue (append (list name) key (list value)))
          (vl-catch-all-apply 'setpropertyvalue (list name key value))
        )
      )
      ((= flag 2) (vl-catch-all-apply 'vlax-put (list (vlax-ename->vla-object name) key value)))
    )
  )
  
  (and
    name
    (setq name (ConvName name 'ENAME))
    (setq flag
      (cond
        ( (vl-symbolp key) 2)
        ( (numberp key) 0)
        ( (or (listp key) (= 'STR (type key))) 1)
      )
    )
    (wcmatch (strcase (getName name flag)) (strcase search))
    (not (vl-catch-all-error-p (setProperty name flag key value)))
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DtSel] SELECTION SET MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:DO-MOVEABOVE" "14/09/2022" "LeeMac" "2.0.0" "\"DtSel\"")                   ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] LM:DO-MoveAbove []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:DO-MoveAbove ( obs obj / tab )
  (if
    (and
      (or
        (listp obs)
        (setq obs (LM:ss->vla obs))
      )
      (setq tab (LM:sortentstable (LM:getowner (car obs))))
    )
    (not (vla-moveabove tab (LM:safearrayvariant vlax-vbobject obs) obj))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:DO-MOVEBELOW" "14/09/2022" "LeeMac" "2.0.0" "\"DtSel\"")                   ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] LM:DO-MoveBelow []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:DO-MoveBelow ( obs obj / tab )
  (if
    (and
      (or
        (listp obs)
        (setq obs (LM:ss->vla obs))
      )
      (setq tab (LM:sortentstable (LM:getowner (car obs))))
    )
    (not (vla-movebelow tab (LM:safearrayvariant vlax-vbobject obs) obj))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:DO-MOVETOBOTTOM" "14/09/2022" "LeeMac" "2.0.0" "\"DtSel\"")                ;--•  Add the function's informations to $lst$ variable ;

;                                     []-----------------------[] LM:DO-MoveToBottom []-----------------------[]                                    ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:DO-MoveToBottom ( obs / tab )
  (if
    (and
      (or
        (listp obs)
        (setq obs (LM:ss->vla obs))
      )
      (setq tab (LM:sortentstable (LM:getowner (car obs))))
    )
    (not (vla-movetobottom tab (LM:safearrayvariant vlax-vbobject obs)))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:DO-MOVETOTOP" "14/09/2022" "LeeMac" "2.0.0" "\"DtSel\"")                   ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] LM:DO-MoveToTop []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:DO-MoveToTop ( obs / tab )
  (if
    (and
      (or
        (listp obs)
        (setq obs (LM:ss->vla obs))
      )
      (setq tab (LM:sortentstable (LM:getowner (car obs))))
    )
    (not (vla-movetotop tab (LM:safearrayvariant vlax-vbobject obs)))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:DO-SWAPORDER" "14/09/2022" "LeeMac" "2.0.0" "\"DtSel\"")                   ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] LM:DO-SwapOrder []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 14/09/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:DO-SwapOrder ( ob1 ob2 / tab )
  (if (setq tab (LM:sortentstable (LM:getowner ob1)))
    (not (vla-swaporder tab ob1 ob2))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:SS->VLA" "09/02/2015" "LeeMac" "1.2.0" "\"DtSel\"")                        ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:ss->vla []-----------------------[]                                         ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:ss->vla ( sel / idx lst )
  (if (= 'pickset (type sel))
    (repeat (setq idx (sslength sel))
      (setq lst (cons (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))) lst))
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:SSBOUNDINGBOX" "04/01/2017" "LeeMac" "1.2.0" "\"DtSel\"")                  ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] LM:ssBoundingBox []-----------------------[]                                     ;
;--- Date of creation       > 04/01/2017                                                                                                            ;
;--- Last modification date > 04/01/2017                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
  (repeat (setq idx (sslength sel))
    (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
    (if
      (and
        (vlax-method-applicable-p obj 'getboundingbox)
        (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
      )
      (setq
        ls1 (mapcar 'min (vlax-safearray->list llp) (cond (ls1) ((vlax-safearray->list llp))))
        ls2 (mapcar 'max (vlax-safearray->list urp) (cond (ls2) ((vlax-safearray->list urp))))
      )
    )
  )
  (if (and ls1 ls2) (list ls1 ls2))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:SSBOUNDINGBOXMIDPT" "09/06/2022" "Luna" "1.0.1" "\"DtSel\"")               ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] LM:ssBoundingBoxMidPt []-----------------------[]                                   ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 09/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun LM:ssboundingboxMidPt ( sel / Pts Pt1 Pt2 )
  (if
    (and
      (setq Pts (LM:ssBoundingBox sel))
      (setq Pt1 (car Pts))
      (setq Pt2 (cadr Pts))
    )
    (trans (polar Pt1 (angle Pt1 Pt2) (/ (distance Pt1 Pt2) 2.0)) 0 1)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "UCSSSBOUNDINGBOX" "07/10/2022" "LeeMac/(gile)/Luna" "1.0.0" "\"DtSel\"")      ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] UCSssBoundingBox []-----------------------[]                                     ;
;--- Date of creation       > 07/10/2022                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > LeeMac/(gile)/Luna                                                                                                    ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun UCSssBoundingBox (sel / i obj sa1 sa2 pt1 pt2 lst)
  (repeat (setq i (sslength sel))
    (setq obj (vlax-ename->vla-object (ssname sel (setq i (1- i)))))
    (vla-TransformBy obj (UCS2WCSMatrix))
    (if
      (and
        (vlax-method-applicable-p obj 'getboundingbox)
        (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'sa1 'sa2))))
      )
      (setq
        pt1 (mapcar 'min (vlax-safearray->list sa1) (cond (pt1) ((vlax-safearray->list sa1))))
        pt2 (mapcar 'max (vlax-safearray->list sa2) (cond (pt2) ((vlax-safearray->list sa2))))
      )
    )
    (vla-TransformBy obj (WCS2UCSMatrix))
  )
  (setq lst
    (cond
      ( (equal (car pt1) (car pt2) 1e-007)
        (list
          pt1
          (list (car pt1) (cadr pt1) (caddr pt2))
          pt2
          (list (car pt1) (cadr pt2) (caddr pt1))
        )
      )
      ( (equal (cadr pt1) (cadr pt2) 1e-007)
        (list
          pt1
          (list (car pt1) (cadr pt1) (caddr pt2))
          pt2
          (list (car pt2) (cadr pt1) (caddr pt1))
        )
      )
      ( (equal (caddr pt1) (caddr pt2) 1e-007)
        (list
          pt1
          (list (car pt1) (cadr pt2) (caddr pt1))
          pt2
          (list (car pt2) (cadr pt1) (caddr pt1))
        )
      )
      ( (and pt1 pt2)
        (list
          (mapcar '(lambda (x y) (/ (+ x y) 2.)) pt1 pt2)
          (mapcar '- pt2 pt1)
        )
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "MUTESEL" "16/06/2022" "Luna" "1.0.0" "\"DtSel\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] MuteSel []-----------------------[]                                          ;
;--- Date of creation       > 16/06/2022                                                                                                            ;
;--- Last modification date > 16/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun MuteSel (msg fun / *error* nom sel)
  (defun *error* (msg)
    (setvar "NOMUTT" nom)
    (princ msg)
    nil
  )
  (setq nom (getvar "NOMUTT"))
  (princ msg)
  (setvar "NOMUTT" 1)
  (setq sel (eval fun))
  (setvar "NOMUTT" nom)
  sel
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "PVBB-DRAW" "07/10/2022" "LeeMac/Luna" "1.4.0" "\"DtSel\"")                    ;--•  Add the function's informations to $lst$ variable ;

;                                         []-----------------------[] PVBB-Draw []-----------------------[]                                         ;
;--- Date of creation       > 04/01/2017                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > LeeMac/Luna                                                                                                           ;
;--- Version                > 1.4.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun PVBB-Draw ( sel / box obj spc )
  (if
    (and
      (setq box (UCSssBoundingBox sel))
      (setq spc
        (vlax-get-property
          (vla-get-activedocument (vlax-get-acad-object))
          (if (= 1 (getvar 'cvport))
            'paperspace
            'modelspace
          )
        )
      )
      (if (= 4 (length box))
        (if (apply '= (mapcar 'caddr box))
          (progn
            (setq obj (vlax-invoke spc 'AddLightweightPolyline (apply 'append (mapcar '(lambda (p) (reverse (cdr (reverse p)))) box))))
            (vla-put-Closed obj :vlax-true)
            (vla-put-elevation obj (caddar box))
            obj
          )
          (progn
            (setq obj (vlax-invoke spc 'Add3DPoly (apply 'append box)))
            (vla-put-Closed obj :vlax-true)
            obj
          )
        )
        (if (= 2 (length box))
          (setq obj (vla-AddBox spc (vlax-3D-point (car box)) (car (cadr box)) (cadr (cadr box)) (caddr (cadr box))))
        )
      )
      (null (vla-TransformBy obj (WCS2UCSMatrix)))
    )
    (cons obj (if (= 4 (length box)) (mapcar '(lambda (p) (trans p 1 0)) box) (list (trans (car box) 1 0) (cadr box))))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SELECT-FILTER" "11/07/2022" "Luna" "3.2.1" "\"DtSel\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] Select-Filter []-----------------------[]                                       ;
;--- Date of creation       > 18/02/2020                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.2.1                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun Select-Filter  (mode filter arg-lst flg-lst flag / jsel name vl-name i n e key-lst tmp-lst lst modf-ss KeyList-Gen check-list)
  (defun modf-ss (arg-lst dxf add / tmp-lst)
    (cond
      ( (and
          (listp (last arg-lst))
          (setq tmp-lst (assoc dxf (last arg-lst)))
        )
        (subst
          (subst (cons dxf add) tmp-lst (last arg-lst))
          (last arg-lst)
          arg-lst
        )
      )
      ( (and
          (listp (last arg-lst))
          (not tmp-lst)
        )
        (subst
          (append (last arg-lst) (list (cons dxf add)))
          (last arg-lst)
          arg-lst
        )
      )
      ( T (append arg-lst (list (list (cons dxf add)))))
    )
  )
  
  (defun check-list (ppt-lst lst / tag value)
    (vl-remove-if-not
      '(lambda (ppt)
        (member T
          (mapcar
            '(lambda (x)
              (and
                (wcmatch
                  (strcase (vl-princ-to-string (car ppt)))
                  (strcase (vl-princ-to-string (car x)))
                )
                (wcmatch
                  (strcase (vl-princ-to-string (cdr ppt)))
                  (strcase (vl-princ-to-string (cdr x)))
                )
              )
             )
            lst
          )
        )
       )
      ppt-lst
    )
  )
  
  (defun KeyList-Gen (key lst / sub)
    (if (cdr key)
      (if (setq sub (assoc (car key) lst))
        (subst
          (cons (car key) (KeyList-Gen (cdr key) (cdr sub)))
          sub
          lst
        )
        (cons
          (cons (car key) (KeyList-Gen (cdr key) (cdr sub)))
          lst
        )
      )
      (if (setq sub (assoc (car key) lst))
        (subst (cons (car key) (1+ (cdr sub))) sub lst)
        (cons (cons (car key) 1) lst)
      )
    )
  )

  (vl-load-com)
  (cond
    ( (= mode "BLC")
      (setq
        arg-lst (modf-ss arg-lst 2 (lst2str filter ","))
        arg-lst (modf-ss arg-lst 0 "INSERT")
      )
    )
    ( (= mode "ATT")
      (setq
        arg-lst (modf-ss arg-lst 2 (lst2str filter ","))
        arg-lst (modf-ss arg-lst 0 "INSERT")
        arg-lst (modf-ss arg-lst 66 1)
      )
    )
    ( (= mode "DYN")
      (setq
        arg-lst (modf-ss arg-lst 2 "`*U*")
        arg-lst (modf-ss arg-lst 0 "INSERT")
      )
    )
    ( (= mode "DXF")
      (foreach dxf filter
        (setq flg-lst (car (modf-ss (list flg-lst) dxf "*")))
      )
    )
  )
  (if
    (and
      (setq jsel (vl-catch-all-apply 'ssget arg-lst))
      (not (vl-catch-all-error-p jsel))
    )
    (progn
      (repeat (setq i (sslength jsel))
        (setq
          name (ssname jsel (setq i (1- i)))
          vl-name (vlax-ename->vla-object name)
        )
        (cond
          ( (and
              (= mode "BLC")
              (vlax-property-available-p vl-name 'EffectiveName)
              (wcmatch (strcase (vla-get-EffectiveName vl-name)) (strcase (lst2str filter ",")))
              (if flg-lst (setq lst (check-list (LM:vlax-dump-object->list vl-name) flg-lst)) T)
            )
            (setq key-lst (KeyList-Gen (list (vla-get-EffectiveName vl-name)) key-lst))
          )
          ( (and
              (= mode "ATT")
              (vlax-property-available-p vl-name 'EffectiveName)
              (wcmatch (strcase (vla-get-EffectiveName vl-name)) (strcase (lst2str filter ",")))
              (setq lst
                (if flg-lst
                  (check-list (get-Att-list vl-name) flg-lst)
                  (get-Att-list vl-name)
                )
              )
            )
            (foreach att lst
              (setq key-lst (KeyList-Gen (list att (vla-get-EffectiveName vl-name)) key-lst))
            )
          )
          ( (and
              (= mode "DYN")
              (vlax-property-available-p vl-name 'EffectiveName)
              (wcmatch (strcase (vla-get-EffectiveName vl-name)) (strcase (lst2str filter ",")))
              (= 1 (getpropertyvalue name "IsDynamicBlock"))
              (setq lst
                (if flg-lst
                  (check-list (get-Dyn-list vl-name) flg-lst)
                  (get-Dyn-list vl-name)
                )
              )
            )
            (foreach dyn lst
              (setq key-lst (KeyList-Gen (list dyn (vla-get-EffectiveName vl-name)) key-lst))
            )
          )
          ( (and
              (= mode "DXF")
              (if flg-lst
                (setq lst
                  (check-list
                    (append
                      (entget name)
                      (if
                        (and
                          (= "INSERT" (cdr (assoc 0 (entget name))))
                          (assoc 66 (entget name))
                        )
                        (get-att-list vl-name)
                      )
                      (if
                        (and
                          (= "INSERT" (cdr (assoc 0 (entget name))))
                          (= 1 (getpropertyvalue name "IsDynamicBlock"))
                        )
                        (get-Dyn-list vl-name)
                      )
                    )
                    flg-lst
                  )
                )
                (assoc 0 (entget name))
              )
            )
            (foreach dxf lst
              (setq key-lst
                (KeyList-Gen
                  (if (= (car dxf) 0)
                    (list dxf)
                    (list dxf (cdr (assoc 0 (entget name))))
                  )
                  key-lst
                )
              )
            )
          )
          ( T (ssdel name jsel))
        )
      )
      (if (and flag (> (sslength jsel) 0))
        (progn
          (prompt
            (strcat 
              "\nNumber of selected object(s) = "
              (itoa (sslength jsel))
              "u"
              (cond
                ( (= mode "BLC") "\nList of found block(s) :")
                ( (= mode "ATT") "\nList of found attribute(s) :")
                ( (= mode "DYN") "\nList of found dynamic property(ies) :")
                ( (= mode "DXF") "\nList of found object(s) properties :")
              )
            )
          )
          (foreach key
            (vl-sort
              key-lst
              '(lambda (a b)
                (if (listp (car a))
                  (if (= (caar a) (caar b))
                    (< (cdar a) (cdar b))
                    (< (caar a) (caar b))
                  )
                  (< (car a) (car b))
                )
               )
            )
            (if (listp (cdr key))
              (progn
                (prompt
                  (strcat
                    "\n  - "
                    (if (listp (car key))
                      (strcat
                        (vl-prin1-to-string (caar key))
                        " = "
                        (vl-princ-to-string (cdar key))
                      )
                      (vl-prin1-to-string (car key))
                    )
                    " ("
                    (itoa (setq n (apply '+ (mapcar 'cdr (cdr key)))))
                    "u - "
                    (rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
                    "%)"
                  )
                )
                (mapcar
                  '(lambda (e)
                    (prompt
                      (strcat
                        "\n     -> "
                        (vl-prin1-to-string (car e))
                        " ("
                        (itoa (cdr e))
                        "u - "
                        (rtos (* 100 (/ (cdr e) (atof (rtos n 2 2)))) 2 2)
                        "%)"
                      )
                    )
                   )
                  (vl-sort (cdr key) '(lambda (a b) (< (car a) (car b))))
                )
              )
              (prompt
                (strcat
                  "\n  - "
                  (vl-prin1-to-string (car key))
                  " ("
                  (itoa (setq n (cdr key)))
                  "u - "
                  (rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
                  "%)"
                )
              )
            )
          )
          (prompt "\n")
        )
      )
    )
    (setq jsel nil)
  )
  (princ)
  (sssetfirst nil jsel)
  jsel
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "ZOOMOBJECTS" "06/07/2022" "Luna" "1.0.0" "\"DtSel\"")                         ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] ZoomObjects []-----------------------[]                                        ;
;--- Date of creation       > 06/07/2022                                                                                                            ;
;--- Last modification date > 06/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

(defun ZoomObjects (jsel / ptlst)
  (and
    jsel
    (setq ptlst (LM:ssBoundingBox jsel))
    (vla-ZoomWindow
      (vlax-get-acad-object)
      (vlax-3D-point (car ptlst))
      (vlax-3D-point (last ptlst))
    )
  )
  jsel
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DtSyt] SYMBOL TABLE AND DICTIONARY-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "DELENV" "04/07/2022" "Patrick_35" "1.0.0" "\"DtSyt\"")                        ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] delenv []-----------------------[]                                           ;
;--- Date of creation       > 13/09/2012                                                                                                            ;
;--- Last modification date > 13/09/2012                                                                                                            ;
;--- Author                 > Patrick_35                                                                                                            ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSyt"                                                                                                               ;

(defun delenv (env / key)
  (setq key (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\FixedProfile\\General"))
  (if (vl-registry-read key env)
    (vl-registry-delete key env)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "FLT_TBL" "20/06/2022" "Luna" "4.0.0" "\"DtSyt\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] flt_tbl []-----------------------[]                                          ;
;--- Date of creation       > 16/04/2019                                                                                                            ;
;--- Last modification date > 20/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.0.0                                                                                                                 ;
;--- Class                  > "DtSyt"                                                                                                               ;

(defun flt_tbl (tag search flag / lst value)
  (setq value (cdr (assoc 2 (tblnext tag T))))
  (while (/= value nil)
    (if
      (=
        T
        (wcmatch
          (if flag value (strcase value))
          (if flag search (strcase search))
        )
      )
      (setq lst (cons value lst))
    )
    (setq value (cdr (assoc 2 (tblnext tag))))
  )
  lst
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LAYER-GET-OR-CREATE" "13/06/2024" "Luna" "1.0.0" "\"DtSyt\"")                 ;--•  Add the function's informations to $lst$ variable ;

;                                    []-----------------------[] layer-get-or-create []-----------------------[]                                    ;
;--- Date of creation       > 13/06/2024                                                                                                            ;
;--- Last modification date > 13/06/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "DtSyt"                                                                                                               ;

(defun layer-get-or-create ( name color ltype lwght plot / layer entlist color62 color42 color43)
  (cond
    ( (= 'INT (type color))
      (setq color62 (cons 62 color))
      (setq color42 nil)
      (setq color43 nil)
    )
    ( (= 'LIST (type color))
      (setq color62 (cons 62 (LM:RGB->ACI (car color) (cadr color) (caddr color))))
      (setq color42 (cons 420 (LM:RGB->True (car color) (cadr color) (caddr color))))
      (setq color43 nil)
    )
  )
  (setq
    ltype (if ltype (cons 6 ltype))
    lwght (if lwght (cons 370 lwght))
    plot (if plot (cons 290 plot))
  )
  (if (setq layer (tblobjname "LAYER" name))
    (setq
      entlist (entget layer)
      entlist (entmod (subst '(70 . 0) (assoc 70 entlist) entlist))
      entlist (if color (entmod (subst color62 (assoc 62 entlist) entlist)) entlist)
      entlist
        (if color
          (entmod
            (if (assoc 420 entlist)
              (if color42
                (subst color42 (assoc 420 entlist) entlist)
                (vl-remove (assoc 420 entlist) entlist)
              )
              (if color42
                (append entlist (list color42))
                entlist
              )
            )
          )
          entlist
        )
      entlist
        (if color
          (entmod
            (if (assoc 430 entlist)
              (if color43
                (subst color43 (assoc 430 entlist) entlist)
                (vl-remove (assoc 430 entlist) entlist)
              )
              (if color43
                (append entlist (list color43))
                entlist
              )
            )
          )
          entlist
        )
      entlist (if ltype (entmod (subst ltype (assoc 6 entlist) entlist)) entlist)
      entlist (if lwght (entmod (subst lwght (assoc 370 entlist) entlist)) entlist)
      entlist (if plot (entmod (subst plot (assoc 290 entlist) entlist)) entlist)
    )
    (entmake
      (vl-remove
        nil
        (list
          '(0 . "LAYER")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbLayerTableRecord")
          (cons 2 name)
          '(70 . 0)
          color62
          color42
          color43
          (cond (ltype) ('(6 . "Continuous")))
          (cond (lwght) ('(370 . 0)))
          (cond (plot) ('(290 . 1)))
        )
      )
    )
  )
)



;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DbOpc] DIALOG BOX OPENING AND CLOSING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DbTil] TILE AND ATTRIBUTE-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DbLst] LIST BOX AND POP-UP LIST-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LISTBOX" "11/05/2022" "Luna" "4.0.0" "\"DbLst\"")                             ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] ListBox []-----------------------[]                                          ;
;--- Date of creation       > 15/04/2017                                                                                                            ;
;--- Last modification date > 11/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.0.0                                                                                                                 ;
;--- Class                  > "DbLst"                                                                                                               ;

(defun ListBox (title msg lst value flag h / vl-list-search LB-select tmp file DCL_ID choice tlst)
  (defun vl-list-search (p l)
    (vl-remove-if-not '(lambda (x) (wcmatch x p)) l)
  )
  
  (defun LB-select (str)
    (if (= "" str)
      "0 selected"
      (strcat (itoa (length (str2lst str " "))) " selected")
    )
  )
  
  (setq
    tmp (vl-filename-mktemp "tmp.dcl")
    file (open tmp "w")
    tlst lst
  )
  (write-line
    (strcat "ListBox:dialog{width=" (itoa (+ (apply 'max (mapcar 'strlen (mapcar 'vl-princ-to-string lst))) 5)) ";label=\"" title "\";")
    file
  )
  (write-line
    ":edit_box{key=\"filter\";}"
    file
  )
  (if (and msg (/= msg ""))
    (write-line (strcat ":text{label=\"" msg "\";}") file)
  )
  (write-line
    (cond
      ( (= 0 flag) "spacer;:popup_list{key=\"lst\";}")
      ( (= 1 flag) (strcat "spacer;:list_box{height=" (itoa (1+ (cond (h) (15)))) ";key=\"lst\";}"))
      ( T (strcat "spacer;:list_box{height=" (itoa (1+ (cond (h) (15)))) ";key=\"lst\";multiple_select=true;}:text{key=\"select\";}"))
    )
    file
  )
  (write-line ":text{key=\"count\";}" file)
  (write-line "spacer;ok_cancel;}" file)
  (close file)
  (setq DCL_ID (load_dialog tmp))
  (if (not (new_dialog "ListBox" DCL_ID))
    (exit)
  )
  (set_tile "filter" "*")
  (set_tile "count" (strcat (itoa (length lst)) " / " (itoa (length lst))))
  (start_list "lst")
  (mapcar 'add_list lst)
  (end_list)
  (set_tile
    "lst"
    (cond
      ( (and
          (= flag 2)
          (listp value)
        )
        (apply 'strcat (vl-remove nil (mapcar '(lambda (x) (if (member x lst) (strcat (itoa (vl-position x lst)) " "))) value)))
      )
      ( (member value lst) (itoa (vl-position value lst)))
      ( (itoa 0))
    )
  )
  (if (= flag 2)
    (progn
      (set_tile "select" (LB-select (get_tile "lst")))
      (action_tile "lst" "(set_tile \"select\" (LB-select $value))")
    )
  )
  (action_tile
    "filter"
    "(start_list \"lst\")
     (mapcar 'add_list (setq tlst (vl-list-search $value lst)))
     (end_list)
     (set_tile \"count\" (strcat (itoa (length tlst)) \" / \" (itoa (length lst))))"
  )
  (action_tile
    "accept"
    "(or 
      (= (get_tile \"lst\") \"\")
      (if (= 2 flag)
        (progn
          (foreach n (str2lst (get_tile \"lst\") \" \")
            (setq choice (cons (nth (atoi n) tlst) choice))
          )
          (setq choice (reverse choice))
        )
        (setq choice (nth (atoi (get_tile \"lst\")) tlst))
      )
    )
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog DCL_ID)
  (vl-file-delete tmp)
  choice
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DbImg] IMAGE TILE-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [DbApp] APPLICATION-SPECIFIC DATA-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;


;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlCol] ACTIVEX COLLECTION MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "VLA-COLLECTION->LIST" "19/08/2021" "Luna" "2.0.0" "\"VlCol\"")                ;--•  Add the function's informations to $lst$ variable ;

;                                    []-----------------------[] vla-collection->list []-----------------------[]                                   ;
;--- Date of creation       > 05/03/2021                                                                                                            ;
;--- Last modification date > 19/08/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlCol"                                                                                                               ;

(defun vla-collection->list (doc col flag / lst item i)
  (if
    (null
      (vl-catch-all-error-p
        (setq
          i 0
          col (vl-catch-all-apply 'vlax-get (list (cond (doc) ((vla-get-activedocument (vlax-get-acad-object)))) col))
        )
      )
    )
    (vlax-for item col
      (setq lst
        (cons
          (cons
            (if (vlax-property-available-p item 'Name)
              (vla-get-name item)
              (strcat "Unnamed_" (itoa (setq i (1+ i))))
            )
            (cond
              ( (= flag 0) (vlax-vla-object->ename item))
              (item)
            )
          )
          lst
        )
      )
    )
  )
  (reverse lst)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlDtc] ACTIVEX DATA CONVERSION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "CONVNAME" "04/01/2022" "Luna" "1.0.0" "\"VlDtc\"")                            ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] ConvName []-----------------------[]                                         ;
;--- Date of creation       > 04/01/2022                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlDtc"                                                                                                               ;

(defun ConvName (name flag / tp)
  (if (= (setq tp (type name)) flag)
    name
    ((eval (read (strcat "vlax-" (vl-symbol-name tp) "->" (vl-symbol-name flag)))) name)
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlMet] ACTIVEX METHOD INVOCATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LWPOLY-ADDVERTEX" "16/11/2023" "Luna" "1.0.0" "\"VlMet\"")                    ;--•  Add the function's informations to $lst$ variable ;

;                                     []-----------------------[] lwpoly-AddVertex []-----------------------[]                                      ;
;--- Date of creation       > 16/11/2023                                                                                                            ;
;--- Last modification date > 16/11/2023                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlMet"                                                                                                               ;

(defun lwpoly-AddVertex (obj pt i / e n s)
  (setq
    e (ConvName obj 'ENAME)
    obj (ConvName obj 'VLA-OBJECT)
    n (cdr (assoc 90 (entget e)))
    pt (2D-Point pt)
    s (vlax-make-safearray vlax-vbDouble '(0 . 1))
    i (cond ((null i) n) ((minusp i) 0) ((< i n) (fix i)) (n))
  )
  (vlax-safearray-fill s pt)
  (vla-AddVertex obj i s)
  (vla-Update obj)
  (if (= (1+ n) (cdr (assoc 90 (entget e)))) (1+ n))
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "UCS2WCSMATRIX" "21/01/2007" "Douglas C. Broad, Jr." "1.0.0" "\"VlMet\"")      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] UCS2WCSMatrix []-----------------------[]                                       ;
;--- Date of creation       > 21/01/2007                                                                                                            ;
;--- Last modification date > 21/01/2007                                                                                                            ;
;--- Author                 > Douglas C. Broad, Jr.                                                                                                 ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlMet"                                                                                                               ;

(defun UCS2WCSMatrix ()
  (vlax-tmatrix
    (append
      (mapcar
        '(lambda (vector origin) (append (trans vector 1 0 t) (list origin)))
        (list '(1 0 0) '(0 1 0) '(0 0 1))
        (trans '(0 0 0) 0 1)
      )
      (list '(0 0 0 1))
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "WCS2UCSMATRIX" "21/01/2007" "Douglas C. Broad, Jr." "1.0.0" "\"VlMet\"")      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] WCS2UCSMatrix []-----------------------[]                                       ;
;--- Date of creation       > 21/01/2007                                                                                                            ;
;--- Last modification date > 21/01/2007                                                                                                            ;
;--- Author                 > Douglas C. Broad, Jr.                                                                                                 ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlMet"                                                                                                               ;

(defun WCS2UCSMatrix ()
  (vlax-tmatrix
    (append
      (mapcar
        '(lambda (vector origin) (append (trans vector 0 1 t) (list origin)))
        (list '(1 0 0) '(0 1 0) '(0 0 1))
        (trans '(0 0 0) 1 0)
      )
      (list '(0 0 0 1))
    )
  )
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlObj] ACTIVEX OBJECT-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:OUTPUTTEXT:PUTTEXTSTRING" "16/01/2016" "LeeMac" "1.0.0" "\"VlObj\"")       ;--•  Add the function's informations to $lst$ variable ;

;                                []-----------------------[] LM:outputtext:puttextstring []-----------------------[]                                ;
;--- Date of creation       > 16/01/2016                                                                                                            ;
;--- Last modification date > 16/01/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun LM:outputtext:puttextstring ( obj str )
  (vla-put-textstring obj "") ;; to clear any existing string
  (vla-put-textstring obj str)
  T
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:OUTPUTTEXT:UPDATEFIELD" "16/01/2016" "LeeMac" "1.0.0" "\"VlObj\"")         ;--•  Add the function's informations to $lst$ variable ;

;                                 []-----------------------[] LM:outputtext:updatefield []-----------------------[]                                 ;
;--- Date of creation       > 16/01/2016                                                                                                            ;
;--- Last modification date > 16/01/2016                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun LM:outputtext:updatefield ( ent / cmd rtn )
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq rtn (vl-cmdf "_.UPDATEFIELD" ent ""))
  (setvar "CMDECHO" cmd)
  rtn
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-ATT-LIST" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] get-att-list []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-att-list (blk)
  (mapcar
    '(lambda (att)
      (cons
        (vla-get-tagstring att)
        (vla-get-textstring att)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-ATT-VALUE" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] get-att-value []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-att-value (blk tag)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
      (if (= tag (strcase (vla-get-tagstring att)))
        (vla-get-textstring att)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DYN-ALLOWEDVALUES" "30/01/2022" "LeeMac" "3.0.0" "\"VlObj\"")             ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] get-dyn-AllowedValues []-----------------------[]                                   ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-dyn-AllowedValues (blk tag)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (dyn)
      (if (= tag (strcase (vla-get-propertyname dyn)))
        (vlax-get dyn 'AllowedValues)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DYN-LIST" "30/01/2022" "LeeMac" "3.0.1" "\"VlObj\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] get-dyn-list []-----------------------[]                                       ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.1                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-dyn-list (blk)
  (mapcar
    '(lambda (dyn)
      (cons
        (vla-get-propertyname dyn)
        (vlax-get dyn 'value)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DYN-VALUE" "30/01/2022" "LeeMac" "3.0.0" "\"VlObj\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] get-dyn-value []-----------------------[]                                       ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-dyn-value (blk tag)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (dyn)
      (if (= tag (strcase (vla-get-propertyname dyn)))
        (vlax-get dyn 'value)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DYN-VISIBILITYNAME" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")            ;--•  Add the function's informations to $lst$ variable ;

;                                   []-----------------------[] get-dyn-VisibilityName []-----------------------[]                                  ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-dyn-VisibilityName (blk / vis)
  (setq blk (ConvName blk 'VLA-OBJECT))
  (if
    (and
      (vlax-property-available-p blk 'EffectiveName)
      (setq
        blk
          (vla-item
            (vla-get-blocks (vla-get-document blk))
            (vla-get-EffectiveName blk)
          )
      )
      (= :vlax-true (vla-get-IsDynamicBlock blk))
      (= :vlax-true (vla-get-HasExtensionDictionary blk))
      (setq
        vis
          (vl-some
            '(lambda (pair)
              (if
                (and
                  (= 360 (car pair))
                  (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                )
                (cdr pair)
              )
             )
            (dictsearch
              (ConvName (vla-getExtensionDictionary blk) 'ENAME)
              "ACAD_ENHANCEDBLOCK"
            )
          )
      )
    )
    (cdr (assoc 301 (entget vis)))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-DYN-VISIBILITYVALUE" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")           ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] get-dyn-VisibilityValue []-----------------------[]                                  ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun get-dyn-VisibilityValue (blk / vis)
  (if (setq vis (get-dyn-VisibilityName blk))
    (get-dyn-value blk vis)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-ATT-LIST" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] set-att-list []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun set-att-list (blk lst / itm tmp)
  (foreach att (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
    (if (setq itm (assoc (vla-get-tagstring att) lst))
      (progn (vla-put-textstring att (cdr itm)) (setq tmp (cons itm tmp)))
    )
  )
  tmp
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-ATT-VALUE" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] set-att-value []-----------------------[]                                       ;
;--- Date of creation       > 14/09/2017                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun set-att-value (blk tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
      (if (= tag (strcase (vla-get-tagstring att)))
        (progn (vla-put-textstring att val) val)
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getAttributes)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-DYN-FLIPSTATE" "30/01/2022" "LeeMac" "3.0.0" "\"VlObj\"")                 ;--•  Add the function's informations to $lst$ variable ;

;                                     []-----------------------[] set-dyn-FlipState []-----------------------[]                                     ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun set-dyn-FlipState (blk)
  (vl-some
    '(lambda (dyn / rtn)
      (if (equal '(0 1) (vlax-get dyn 'AllowedValues))
        (progn
          (vla-put-value dyn (setq rtn (- 1 (vlax-get dyn 'value))))
          rtn
        )
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-DYN-LIST" "30/01/2022" "LeeMac" "3.0.0" "\"VlObj\"")                      ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] set-dyn-list []-----------------------[]                                        ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun set-dyn-list (blk lst)
  (vl-remove
    nil
    (mapcar
      '(lambda (x)
        (if (set-dyn-value blk (car x) (cdr x))
          x
        )
      )
      lst
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-DYN-VALUE" "30/01/2022" "LeeMac" "2.1.0" "\"VlObj\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                       []-----------------------[] set-dyn-value []-----------------------[]                                       ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 30/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.1.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun set-dyn-value (blk tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (dyn / avl)
      (if
        (and
          (= tag (strcase (vla-get-propertyname dyn)))
          (cond ((setq avl (get-dyn-AllowedValues blk tag)) (member val avl)) ((not avl)))
          (null (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-value (list dyn val))))
        )
        val
      )
     )
    (vlax-invoke (ConvName blk 'VLA-OBJECT) 'getDynamicBlockProperties)
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-DYN-VISIBILITYVALUE" "04/01/2022" "LeeMac" "2.0.0" "\"VlObj\"")           ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] set-dyn-VisibilityValue []-----------------------[]                                  ;
;--- Date of creation       > 19/08/2013                                                                                                            ;
;--- Last modification date > 04/01/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlObj"                                                                                                               ;

(defun set-dyn-VisibilityValue (blk val / vis)
  (if
    (and
      (setq vis (get-dyn-VisibilityName blk))
      (member (strcase val) (mapcar 'strcase (get-dyn-AllowedValues blk vis)))
    )
    (set-dyn-value blk vis val)
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlPrp] ACTIVEX PROPERTY-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "GET-LAYOUTS-POS" "24/01/2022" "Luna" "2.0.0" "\"VlPrp\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] get-layouts-pos []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

(defun get-layouts-pos (/ tab ll l lst)
  (setq
    tab (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
    ll (layoutlist)
  )
  (foreach l (reverse ll)
    (setq lst (cons (cons l (vla-get-taborder (vla-item tab l))) lst))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-LAYOUTS-POS" "24/01/2022" "Luna" "2.0.0" "\"VlPrp\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] set-layouts-pos []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 24/01/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

(defun set-layouts-pos (f / i tab ll l)
  (setq
    i 0
    tab (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
    ll (cond ((null f) (layoutlist)) ((listp f) f) (f (vl-sort (layoutlist) f)))
  )
  (foreach l ll
    (vla-put-taborder (vla-item tab l) (setq i (1+ i)))
  )
  ll
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "SET-LAYOUT-NAME" "21/06/2024" "Luna" "1.1.1" "\"VlPrp\"")                     ;--•  Add the function's informations to $lst$ variable ;

;                                      []-----------------------[] set-layout-name []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 21/06/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.1                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

(defun set-layout-name (str-old str-new / ll)
  (setq ll (vla-collection->list nil 'layouts 1))
  (setq str-new (vl-string-trim " " str-new))
  (if (assoc str-old ll)
    (if (not (assoc str-new ll))
      (progn
        (vla-put-name (cdr (assoc str-old ll)) str-new)
        str-new
      )
      (progn
        (while (assoc str-new ll)
          (cond
            ( (wcmatch str-new "* (#)")
              (setq str-new (strcat (substr str-new 1 (- (strlen str-new) 2)) (itoa (1+ (atoi (substr str-new (- (strlen str-new) 1) 1)))) ")"))
            )
            ( (wcmatch str-new "* (##)")
              (setq str-new (strcat (substr str-new 1 (- (strlen str-new) 3)) (itoa (1+ (atoi (substr str-new (- (strlen str-new) 2) 2)))) ")"))
            )
            ( (setq str-new (strcat str-new " (2)")))
          )
        )
        (vla-put-name (cdr (assoc str-old ll)) str-new)
        str-new
      )
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "DUMPALLPROPERTIES->LIST" "10/12/2021" "Luna" "1.0.0" "\"VlPrp\"")             ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] dumpallproperties->list []-----------------------[]                                  ;
;--- Date of creation       > 10/12/2021                                                                                                            ;
;--- Last modification date > 10/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

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

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "VLAX-DUMP-OBJECT->LIST" "11/07/2022" "Luna" "1.0.0" "\"VlPrp\"")              ;--•  Add the function's informations to $lst$ variable ;

;                                  []-----------------------[] vlax-dump-object->list []-----------------------[]                                   ;
;--- Date of creation       > 11/07/2022                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

(defun vlax-dump-object->list (name flag / *error* get-Coordinate lst sep lng str m pos sch key value prp mth)
  (defun get-Coordinate (obj i / val)
    (if (not (vl-catch-all-error-p (setq val (vl-catch-all-apply 'vla-get-Coordinate (list obj (setq i (cond (i) (0))))))))
      (cons (vlax-safearray->list (vlax-variant-value val)) (get-Coordinate obj (1+ i)))
    )
  )
  (setq
    name (ConvName name 'VLA-Object)
    lst (vl-remove "" (FunctionLog (quote (vlax-dump-object name flag))))
    sep ";   "
    lng (strlen sep)
  )
  (while (and lst (not (wcmatch (car lst) (strcat sep "*"))))
    (setq lst (cdr lst))
  )
  (while (and lst (setq str (car lst)))
    (if (and flag (not (wcmatch (substr str lng) " *")))
      (setq m T)
      (and
        (setq pos (vl-string-search (setq sch (if m " (" " = ")) str))
        (setq str (substr str (1+ lng) (- pos lng)))
        (setq key (substr str 1 (vl-string-search " (" str)))
        (if (and (null m) (= key "Coordinate"))
          (setq value (get-Coordinate name nil))
          (cond
            ( m (setq value (atoi (substr (car lst) (+ 1 pos (strlen sch)) (- (vl-string-search ")" (car lst)) (+ 2 pos))))))
            ( (vl-catch-all-error-p (setq value (vl-catch-all-apply 'vlax-get (list name (read key))))) (setq value nil))
            (value)
          )
        )
        (if m
          (setq mth (cons (cons key value) mth))
          (setq prp (cons (cons key value) prp))
        )
      )
    )
    (setq lst (cdr lst))
  )
  (cond
    ( (and prp mth) (append (reverse prp) (list (cons -3 (reverse mth)))))
    ( prp (reverse prp))
    ( mth (mapcar 'car (reverse mth)))
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:VLAX-DUMP-OBJECT->LIST" "11/07/2022" "LeeMac" "2.0.0" "\"VlPrp\"")         ;--•  Add the function's informations to $lst$ variable ;

;                                 []-----------------------[] LM:vlax-dump-object->list []-----------------------[]                                 ;
;--- Date of creation       > 15/12/2016                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

(defun LM:vlax-dump-object->list (obj / prp GetObjectProperties GetVlaProperties GetVlaAtoms)
  (defun GetObjectProperties (obj)
    (vl-remove-if-not
      '(lambda ( prp )
        (and
          (vlax-property-available-p obj prp)
          (/= prp "Coordinate")
        )
       )
      (GetVlaProperties)
    )
  )
  
  (defun GetVlaProperties ()
    (eval
      (list 'defun 'GetVlaProperties '()
        (list 'quote
          (mapcar
            (function
              (lambda ( sym )
                (substr (vl-symbol-name sym) 9)
              )
            )
            (vl-remove-if-not
              (function
                (lambda ( sym )
                  (wcmatch (strcase (vl-symbol-name sym)) "VLA`-GET`-*")
                )
              )
              (GetVlaAtoms)
            )
          )
        )
      )
    )
    (GetVlaProperties)
  )
  
  (defun GetVlaAtoms ()
    (eval
      (list 'defun 'GetVlaAtoms '( )
        (list 'quote
          (vl-sort
            (vl-remove-if-not
              (function
                (lambda ( sym )
                  (wcmatch (vl-symbol-name sym) "vla`-*")
                )
              )
              (atoms-family 0)
            )
            (function
              (lambda ( a b )
                (<
                  (vl-symbol-name a)
                  (vl-symbol-name b)
                )
              )
            )
          )
        )
      )
    )
    (GetVlaAtoms)
  )
  
  (mapcar
    '(lambda (prp)
      (cons prp (vlax-get obj prp))
     )
    (GetObjectProperties obj)
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlCrv] CURVE MEASUREMENT ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlDic] DICTIONARY ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:SORTENTSTABLE" "09/02/2015" "LeeMac" "1.2.0" "\"VlDic\"")                  ;--•  Add the function's informations to $lst$ variable ;

;                                     []-----------------------[] LM:SortentsTable []-----------------------[]                                      ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "VlDic"                                                                                                               ;

(defun LM:SortentsTable ( obj / dic )
  (cond
    ( (LM:CatchApply 'vla-item (list (setq dic (vla-getextensiondictionary obj)) "acad_sortents")))
    ( (LM:CatchApply 'vla-addobject  (list dic "acad_sortents" "AcDbSortentsTable")))
  )
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlDrw] HANDLING DRAWING OBJECTS ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:ACDOC" "09/02/2015" "LeeMac" "1.2.0" "\"VlDrw\"")                          ;--•  Add the function's informations to $lst$ variable ;

;                                          []-----------------------[] LM:acdoc []-----------------------[]                                         ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "VlDrw"                                                                                                               ;

(defun LM:acdoc nil
  (eval (list 'defun 'LM:acdoc 'nil (vla-get-ActiveDocument (vlax-get-acad-object))))
  (LM:acdoc)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "LM:GETOWNER" "09/02/2015" "LeeMac" "1.2.0" "\"VlDrw\"")                       ;--•  Add the function's informations to $lst$ variable ;

;                                        []-----------------------[] LM:getOwner []-----------------------[]                                        ;
;--- Date of creation       > 09/02/2015                                                                                                            ;
;--- Last modification date > 09/02/2015                                                                                                            ;
;--- Author                 > LeeMac                                                                                                                ;
;--- Version                > 1.2.0                                                                                                                 ;
;--- Class                  > "VlDrw"                                                                                                               ;

(defun LM:getOwner ( obj )
  (eval
    (list 'defun 'LM:getowner '( obj )
      (if (vlax-method-applicable-p obj 'ownerID32)
        (list 'vla-objectIDtoobject32 (LM:acdoc) '(vla-get-ownerID32 obj))
        (list 'vla-objectIDtoobject   (LM:acdoc) '(vla-get-ownerID   obj))
      )
    )
  )
  (LM:getowner obj)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlRct] REACTORS ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlSpc] VLX NAMESPACE ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [VlCom] NAMESPACE COMMUNICATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;



; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ LOADING ALL COMMAND'S FILES FOR EACH NEW DRAWING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;--- Add the content of each command file if you want to load them directly with this file instead of using (load) function, which is slower.       ;
(setq $k$ 2)                                                        ;--•  Define the global variable $k$ to 2 for command's section                 ;

;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [PsUcart] TITLE BLOCK-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:DATECART" "23/12/2022" "Luna" "2.0.5" "\"PsUcart\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] DATECART []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 23/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.5                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

(defun c:DATECART ( / *error* set-cart-date layout-list ll)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun set-cart-date (ll str / jsel i name lst)
    (if
      (not
        (and
          str
          (= (strlen str) 10)
          (wcmatch str "##/##/####")
        )
      )
      (setq str (substr (get-date) 1 10))
    )
    (if (setq jsel (select-filter "BLC" '("*Cartouche*" "`*U*") (list "_X" (list (cons 410 (lst2str ll ",")) (cons 66 1))) nil nil))
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (if
          (and
            (assoc "DATE" (get-att-list name))
            (set-att-value name "DATE" str)
          )
          (setq lst (cons (cdr (assoc 410 (entget name))) lst))
          (ssdel name jsel)
        )
      )
    )
    (cons str lst)
  )

  (sssetfirst nil nil)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if
    (and
      (setq layout-list
        (ListBox
          (LgT
            "DATECART: Layout tab(s) selection"
            "DATECART: Sélection des présentation(s)"
            nil
          )
          (LgT
            "Please, select one or more layout tab(s) to be re-dated :"
            "Veuillez sélectionner la ou les présentation(s) à re-dater :"
            nil
          )
          (vl-remove-if '(lambda (x) (member (strcase x) '("TOOLKIT" "TRAVAIL"))) (layoutlist))
          (getvar "CTAB")
          2
          nil
        )
      )
      (setq ll
        (set-cart-date
          layout-list
          (getstring
            (strcat
              (LgT
                "\nPlease, define the desired date (ENTER for \""
                "\nVeuillez définir la date souhaitée (ENTREE pour \""
                nil
              )
              (substr (get-date) 1 10)
              "\") : "
            )
          )
        )
      )
    )
    (princ
      (LgT
        (strcat
          "\nThe new date \"" (car ll) "\" has been successfully defined on "
          (itoa (length (cdr ll)))
          " / "
          (itoa (length layout-list))
          " layout tabs"
          (if (cdr ll)
            (strcat
              " :\n  - "
              (lst2str (cdr ll) "\n  - ")
            )
            "..."
          )
        )
        (strcat
          "\nLa nouvelle date \"" (car ll) "\" a été définie avec succès sur "
          (itoa (length (cdr ll)))
          " / "
          (itoa (length layout-list))
          " présentations"
          (if (cdr ll)
            (strcat
              " :\n  - "
              (lst2str (cdr ll) "\n  - ")
            )
            "..."
          )
        )
        nil
      )
    )
    (princ
      (LgT
        "\nAn error has occurred..."
        "\nUne erreur est survenue..."
        nil
      )
    )
  )
  (sssetfirst nil nil)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:MFATTCART" "22/12/2022" "Luna" "2.0.0" "\"PsUcart\"")                       ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] MFATTCART []-----------------------[]                                         ;
;--- Date of creation       > 19/12/2022                                                                                                            ;
;--- Last modification date > 22/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

(defun c:MFATTCART (/ MFA-AutoAttributeUpdate param mode ent obj tag str)
  (defun MFA-AutoAttributeUpdate (i)
    (LM:outputtext:puttextstring obj (cdr (nth i param)))
    (LM:outputtext:updatefield ent)
    (princ
      (LgT
        (strcat
          "\nThe attribute \"" tag "\""
          " is now set with the path : "
          (car (nth i param))
        )
        (strcat
          "\nL'attribut \"" tag "\""
          " est désormais défini avec le chemin : "
          (car (nth i param))
        )
        nil
      )
    )
    (princ)
  )

  (setq param
    (mapcar
      'MFiles-field-generator
      (list
        '(0 1)    ;; 00 → Projet(s) #1 > Code
        '(0 2)    ;; 01 → Projet(s) #1 > Nom du Projet
        '(0 3)    ;; 02 → Projet(s) #1 > Commune
        '(0 7)    ;; 03 → Projet(s) #1 > Créateur
        '(0 09 8) ;; 04 → Projet(s) #1 > Dessinateur(s) Projeteur (s) #1 > Triptique
        '(0 10 8) ;; 05 → Projet(s) #1 > Contributeur - Dessinateur(s) Projeteur(s) #1 > Triptique
        '(0 11 8) ;; 06 → Projet(s) #1 > Chargé(s) Affaire(s) #1 > Triptique
        '(0 12 8) ;; 07 → Projet(s) #1 > Contributeur - Chargé(s) Affaire(s) #1 > Triptique
        '(0 13 8) ;; 08 → Projet(s) #1 > Chef(s) de Projets Développement #1 > Triptique
        '(0 14 8) ;; 09 → Projet(s) #1 > Contributeur - Chef(s) de Projets Développement #1 > Triptique
        '(0 15 8) ;; 10 → Projet(s) #1 > Chargé(s) d'Etude(s) #1 > Triptique
        '(0 16 8) ;; 11 → Projet(s) #1 > Contributeur - Chargé(s) d'Etude(s) #1 > Triptique
        '(0 17 8) ;; 12 → Projet(s) #1 > Chef(s) de Projets Construction #1 > Triptique
        '(0 18 8) ;; 13 → Projet(s) #1 > Contributeur - Chef(s) de Projets Construction #1 > Triptique
        '(0 19 8) ;; 14 → Projet(s) #1 > Chargé(s) de Travaux #1 > Triptique
        '(0 20 8) ;; 15 → Projet(s) #1 > Contributeur - Chargé(s) de Travaux #1 > Triptique
        '(0 21 8) ;; 16 → Projet(s) #1 > Automaticien(s) #1 > Triptique
        '(0 22 8) ;; 17 → Projet(s) #1 > Contributeur - Automaticien(s) #1 > Triptique
      )
    )
  )
  (setq mode
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nWhich mode do you want to use"
        "\nQuel mode désirez-vous utiliser"
        nil
      )
      (list
        (LgT
          "Auto Forced _Auto Forced"
          "Auto Forcé _Auto Forced"
          nil
        )
      )
      " ? "
      "Auto"
      (LgT
        (strcat
          "\nMFATTCART : Mode's functionnalities"
          "\nDefault value:     Auto"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Check the attribute tag, and if it matches with programmed tag,     |"
          "\n  |     Auto    | defines the associated M-Files field for it (Ex.: tag = PROJET, use |"
          "\n  |             | \"Projet(s) #1 > Nom du Projet\" as M-Files link).                    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | For each attribute selected, open a dialog box to select the        |"
          "\n  |    Forced   | desired M-Files link within a list (no multiple choice), then apply |"
          "\n  |             | the associated M-Files field to the attribute.                      |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nMFATTCART : Fonctionnalités des modes"
          "\nValeur par défaut: Auto"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Vérifie le nom d'étiquette d'attribut et s'il correspond à un nom   |"
          "\n  |     Auto    | programmé, définie le champ dynamique M-Files associé (Ex.: tag =   |"
          "\n  |             | PROJET, utilise \"Projet(s) #1 > Nom du Projet\" comme lien M-Files). |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Pour chaque attribut sélectionné, ouvre une boîte de dialogue pour  |"
          "\n  |    Forcé    | choisir le lien M-Files souhaité à partir d'une liste (pas de choix |"
          "\n  |             | multiple), puis y applique le champ dynamique M-Files associé.      |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (while
    (and
      (setq ent (car (nentsel (LgT "\nSelect an attribute : " "\nSélectionner un attribut : " nil))))
      (setq obj (vlax-ename->vla-object ent))
      (= "ATTRIB" (cdr (assoc 0 (entget ent))))
      (setq tag (cdr (assoc 2 (entget ent))))
    )
    (cond
      ( (= mode "Auto")
        (cond
          ( (= "CODE" tag) (MFA-AutoAttributeUpdate 0))
          ( (= "PROJET" tag) (MFA-AutoAttributeUpdate 1))
          ( (= "COMMUNE" tag) (MFA-AutoAttributeUpdate 2))
          ( (= "PROJETEUR" tag) (MFA-AutoAttributeUpdate 4))
          ( (= "CHARGE_ETUDE" tag) (MFA-AutoAttributeUpdate 10))
          ( (= "CHEF_PROJET_DEV" tag) (MFA-AutoAttributeUpdate 8))
          ( (= "CHARGE_AFFAIRE" tag) (MFA-AutoAttributeUpdate 6))
          ( (wcmatch tag "CHEF_PROJET_CONS*") (MFA-AutoAttributeUpdate 12))
          ( (= "CONDUCTEUR_TRAVAUX" tag) (MFA-AutoAttributeUpdate 14))
          ( (wcmatch tag "AUTEUR_*") (MFA-AutoAttributeUpdate 4))
          ( (princ
              (LgT
                (strcat "\nNo automatic completion for the attribute \"" tag "\"...")
                (strcat "\nAucune complétion automatique pour l'attribut \"" tag "\"...")
                nil
              )
            )
          )
        )
      )
      ( (= mode "Forced")
        (setq str
          (ListBox
            (LgT
              "MFATTCART: M-Files field expression"
              "MFATTCART: Expression de champs M-Files"
              nil
            )
            (LgT
              (strcat "Please, select the desired expression you want to apply on the attribute " tag " :")
              (strcat "Veuillez sélectionner l'expression de champ que vous souhaitez appliquer à l'attribut " tag " :")
              nil
            )
            (mapcar 'car param)
            nil
            1
            nil
          )
        )
        (MFA-AutoAttributeUpdate (vl-position str (mapcar 'car param)))
      )
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:MODCART" "12/07/2024" "Luna" "2.2.1" "\"PsUcart\"")                         ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] MODCART []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 12/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.2.1                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

(defun c:MODCART ( / param ignore date format lst layout block sep lng tmp laylist mode att blst str-mod str-aut)  
  (sssetfirst)
  (and
    (setq param
      (list
        (list "MODIFICATIONS_IND_F" "AUTEUR_F")
        (list "MODIFICATIONS_IND_E" "AUTEUR_E")
        (list "MODIFICATIONS_IND_D" "AUTEUR_D")
        (list "MODIFICATIONS_IND_C" "AUTEUR_C")
        (list "MODIFICATIONS_IND_B" "AUTEUR_B")
        (list "MODIFICATIONS_IND_A" "AUTEUR_A")
      )
    )
    (setq ignore (list "MODIFICATIONS_IND_A" "AUTEUR_A"))
    (setq date (cons "DATE" (substr (get-date) 1 10)))
    (setq format
      (list
        (append
          (mapcar 'car param)
          (list '(lambda (x) (substr (car x) (strlen (car x)))))
        )
        " > ["
        (list "PHASE" "Visibilité1" "Visibility1" 'cdr)
        "] "
        'layout
        " ("
        (list "FORMAT" 'cdr)
        " | "
        (append (mapcar 'cadr param) (list 'cdr))
        ")"
      )
    )
    (setq lst
      (loop-a-list-properties
        (select-filter "BLC" '("*Cartouche*" "`*U*") (list "_X" (list (cons 410 (lst2str (layoutlist) ",")))) nil nil)
        (list 410 'EffectiveName)
        (quote (list (cons (cdr (assoc 5 (entget name))) (append (get-att-list name) (get-dyn-list name)))))
        'append
        T
      )
    )
    (null (sssetfirst))
    (setq lst
      (mapcar
        '(lambda (x) (cons x (cdr (assoc x lst))))
        (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
      )
    )
    (setq lst (vl-remove-if '(lambda (x) (null (cdr x))) lst))
    (setq lng (apply 'max (mapcar '(lambda (x) (strlen (car x))) lst)))
    (setq tmp
      (vl-remove
        nil
        (apply
          'append
          (mapcar
            '(lambda (x / layout tmp)
              (setq
                layout (car x)
                tmp (cdr x)
              )
              (apply
                'append
                (mapcar
                  '(lambda (x / block tmp)
                    (setq
                      block (car x)
                      tmp (cdr x)
                    )
                    (apply
                      'append
                      (mapcar
                        '(lambda (x / hdl blst tmp)
                          (setq
                            hdl (car x)
                            blst (cdr x)
                            tmp (FormAssoc format blst 31)
                          )
                          (if tmp (list (cons tmp hdl)))
                        )
                        tmp
                      )
                    )
                  )
                  tmp
                )
              )
            )
            lst
          )
        )
      )
    )
    (setq laylist
      (ListBox
        (LgT
          "MODCART: Layout tab(s) selection"
          "MODCART: Sélection des présentation(s)"
          nil
        )
        (LgT
          "Please, select one or more layout tab(s) to be modified :"
          "Veuillez sélectionner la ou les présentation(s) à modifier :"
          nil
        )
        (mapcar 'car tmp)
        (cond
          ( (= "Model" (getvar "CTAB")) (mapcar 'car tmp))
          ( (car (nth (1- (cdr (assoc (getvar "CTAB") (get-layouts-pos)))) tmp)))
        )
        2
        nil
      )
    )
    (setq lst (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))))
    (princ
      (strcat
        (LgT
          "\nLayout tab(s) list = "
          "\nListe des présentation(s) = "
          nil
        )
        (lst2str
          (mapcar
            '(lambda (s) (vl-string-right-trim " " (substr s 1 (vl-string-search (cond (sep) ("")) s))))
            laylist
          )
          ", "
        )
      )
    )
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nSelect an option"
          "\nSélectionnez une option"
          nil
        )
        (list "Reset +1")
        " : "
        "+1"
        (LgT
          (strcat
            "\nMODCART : Available methods"
            "\nDefault value:     +1"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | For the selected layout tab(s), deletes every value defined in the  |"
            "\n  |    Reset    | attributes \"MODIFICATIONS_IND_[B-F]\" and \"AUTEUR_[B-F]\" and keep    |"
            "\n  |             | the index A as \"INITIAL DESIGN\" (if english) or \"CREATION DU PLAN\"  |"
            "\n  |             | (if french)                                                         |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | For the selected layout tab(s), adds a new value in the first empty |"
            "\n  |             | attribute \"MODIFICATIONS_IND_[B-F]\" and \"AUTEUR_[B-F]\" or roll      |"
            "\n  |      +1     | down all these attributes's value to insert the new value on top of |"
            "\n  |             | them without deleting the last modification's name (WARNING:        |"
            "\n  |             | Depending on the version of the title block, the letter will not    |"
            "\n  |             | corresponds to the real version of the layout !)                    |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          (strcat
            "\nMODCART : Méthodes disponibles"
            "\nValeur par défaut: +1"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | Pour les présentation(s) sélectionnée(s), supprime toutes les       |"
            "\n  |    Reset    | valeurs définies dans les attributs \"MODIFICATIONS_IND_[B-F]\" et    |"
            "\n  |             | \"AUTEUR_[B-F]\" en conservant l'indice A sur \"INITIAL DESIGN\" (si    |"
            "\n  |             | anglais) ou \"CREATION DU PLAN\" (si français)                        |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |             | Pour les présentation(s) sélectionnée(s), ajoute une nouvelle       |"
            "\n  |             | valeur dans le premier attribut \"MODIFICATIONS_IND_[B-F]\" et        |"
            "\n  |             | \"AUTEUR_[B-F]\" vide ou effectue un roulement vers le bas pour       |"
            "\n  |      +1     | toutes les valeurs de ces attributs afin d'insérer la nouvelle en   |"
            "\n  |             | haut de la liste sans supprimer le nom de la dernière modification  |"
            "\n  |             | (ATTENTION: Selon la version du cartouche, la lettre peut ne pas    |"
            "\n  |             | correspondre à la version réelle de la présentation !)              |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          nil
        )
      )
    )
    (cond
      ( (= mode "Reset")
        (mapcar
          '(lambda (x / hdl name blst att ind)
            (setq
              x (assoc x tmp)
              hdl (cdr x)
              name (handent hdl)
              blst (cdr (assoc hdl lst))
              att
                (set-att-list
                  name
                  (append
                    (list date)
                    (mapcar
                      '(lambda (x) (cons (car x) ""))
                      (vl-remove-if-not
                        '(lambda (x) (and (not (member (car x) ignore)) (member (car x) (apply 'append param))))
                        blst
                      )
                    )
                  )
                )
              blst
                (vl-sort
                  (vl-remove-if-not '(lambda (x) (and (member (car x) (apply 'append param)) (/= (cdr x) ""))) (get-att-list name))
                  '(lambda (e1 e2) (> (car e1) (car e2)))
                )
              ind (chr (apply 'max (mapcar '(lambda (x) (ascii (substr (car x) (strlen (car x))))) blst)))
            )
            (princ
              (strcat
                (LgT
                  "\n  •> The layout tab \""
                  "\n  •> La présentation \""
                  nil
                )
                (car x)
                (LgT
                  "\" is now defined on modification index → "
                  "\" est désormais définie sur l'indice de modification → "
                  nil
                )
                ind
                " ("
                (lst2str (mapcar 'cdr (vl-remove-if-not '(lambda (x) (wcmatch (car x) (strcat "*" ind))) blst)) " - ")
                ")"
              )
            )
           )
          laylist
        )
      )
      ( (= mode "+1")
        (while
          (and
            (princ
              (LgT
                "\nFor information (max. characters): A4/A3 (Portrait) → 29, A4/A3/A2 (Landscape) → 42, A1/A0/A0+ (Landscape) → 125"
                "\nPour information (max. caractères): A4/A3 (Portrait) → 29, A4/A3/A2 (Paysage) → 42, A1/A0/A0+ (Paysage) → 125"
                nil
              )
            )
            (=
              ""
              (setq str-mod
                (strcase
                  (getstring
                    T
                    (LgT
                      "\nSpecify the new text string to add : "
                      "\nSpécifiez le nouveau texte à ajouter : "
                      nil
                    )
                  )
                )
              )
            )
          )
          (alert-princ
            (LgT
              "\nThe text value can't be empty..."
              "\nLe texte ne peut pas être vide..."
              nil
            )
          )
        )
        (while
          (=
            ""
            (setq str-aut
              (strcase
                (getstring
                  (LgT
                    "\nSpecify the author's name of this modification : "
                    "\nSpécifiez le nom de l'auteur de cette modification : "
                    nil
                  )
                )
              )
            )
          )
          (alert-princ
            (LgT
              "\nThe author value can't be empty..."
              "\nL'auteur ne peut pas être vide..."
              nil
            )
          )
        )
        (mapcar
          '(lambda (x / hdl name blst att ind)
            (setq
              x (assoc x tmp)
              hdl (cdr x)
              name (handent hdl)
              blst (cdr (assoc hdl lst))
              att (mapcar '(lambda (a) (mapcar '(lambda (b) (assoc b blst)) a)) (vl-remove-if '(lambda (x) (member (car x) ignore)) (reverse param)))
            )
            (while (and att (not (= "" (cdaar att))))
              (setq att (cdr att))
            )
            (if (setq att (mapcar '(lambda (a s) (cons (car a) s)) (car att) (list str-mod str-aut)))
              (setq
                att (set-att-list name (append (list date) att))
                ind (chr (apply 'max (mapcar '(lambda (x) (ascii (substr (car x) (strlen (car x))))) (cdr att))))
              )
              (setq
                att (mapcar '(lambda (a) (mapcar '(lambda (b) (assoc b blst)) a)) (reverse param))
                blst (append (cdr att) (list (mapcar 'cons (car param) (list str-mod str-aut))))
                att (mapcar '(lambda (o n) (mapcar '(lambda (a b) (cons (car a) (cdr b))) o n)) att blst)
                att (set-att-list name (append (list date) (apply 'append att)))
                ind (strcat (substr (caar param) (strlen (caar param))) (LgT " (rolled down)" " (déroulement)" nil))
              )
            )
            (princ
              (strcat
                (LgT
                  "\n  •> The layout tab \""
                  "\n  •> La présentation \""
                  nil
                )
                (car x)
                (LgT
                  "\" is now defined on modification index → "
                  "\" est désormais définie sur l'indice de modification → "
                  nil
                )
                ind
              )
            )
           )
          laylist
        )
        (princ
          (strcat
            (LgT
              "\nThe listed layout tab(s) (see above) are now defined with the modification text \""
              "\nLa liste des présentation(s) (voir ci-dessus) sont désormais définies avec le texte de modification \""
              nil
            )
            str-mod
            "\" (= " (itoa (strlen str-mod)) "c)"
            (LgT
              " wrote by "
              " écrit par "
              nil
            )
            str-aut
            (LgT
              " on "
              " le "
              nil
            )
            (cdr date)
          )
        )
      )
    )
    
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:NAMECART" "21/06/2024" "Luna" "6.1.7" "\"PsUcart\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] NAMECART []-----------------------[]                                          ;
;--- Date of creation       > 25/05/2019                                                                                                            ;
;--- Last modification date > 21/06/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 6.1.7                                                                                                                 ;
;--- Class                  > "PsUcart"                                                                                                             ;

(defun c:NAMECART ( / *error* get-cart-name set-cart-name i Phase laylist s-lst d-lst mode-s mode-n layout ll lst)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun get-cart-name (layout format / jsel name att-list lst vis)
    (sssetfirst nil nil)
    (cond
      ( (null (setq jsel (select-filter "BLC" '("*Cartouche*" "`*U*") (list "_X" (list (cons 410 layout) (cons 66 1))) nil nil)))
        (alert-princ
          (LgT
            (strcat
              "An error occurred during the cartridge selection..."
              "\nNo blocks were found on the layout tab \""
              layout
              "\" whose name begins with \"Cartouche\"..."
            )
            (strcat
              "Une erreur est survenue lors de la sélection du cartouche..."
              "\nAucun bloc n'a été trouvé sur la présentation \""
              layout
              "\" dont le nom commence par \"Cartouche\"..."
            )
            nil
          )
        )
      )
      ( (not (equal 1 (sslength jsel)))
        (alert-princ
          (LgT
            (strcat
              "An error occurred during the cartridge selection..."
              "\nThe layout tab \""
              layout
              "\" currently has more than one block whose name begins with \"Cartouche\"..."
            )
            (strcat
              "Une erreur est survenue lors de la sélection du cartouche..."
              "\nLa présentation \""
              layout
              "\" possède actuellement plus d'un bloc dont le nom commence par \"Cartouche\"..."
            )
            nil
          )
        )
      )
      ( (null (setq name (ssname jsel 0))))
      ( (null
          (and
            (setq vis (car (vl-member-if '(lambda (x) (wcmatch x (strcat "*" Phase "*"))) (get-dyn-AllowedValues name (get-dyn-VisibilityName name)))))
            (set-dyn-VisibilityValue name vis)
          )
        )
        (alert-princ
          (LgT
            (strcat
              "\nAn error occurred during the cartridge selection..."
              "\nThe cartridge block reference doesn't have the visibility value \""
              (cond (vis) (Phase))
              "\" on the layout tab \""
              layout
              "\"..."
            )
            (strcat
              "\nUne erreur est survenue lors de la sélection du cartouche..."
              "\nLa référence de bloc cartouche ne possède pas de visbilité \""
              (cond (vis) (Phase))
              "\" sur la présentation \""
              layout
              "\"..."
            )
            nil
          )
        )
      )
      ( (null (setq att-list (get-att-list name)))
        (alert-princ
          (LgT
            (strcat
              "\nAn error occurred during the cartridge selection..."
              "\nThe cartridge block reference has no attributes on the layout tab \""
              layout
              "\"..."
            )
            (strcat
              "\nUne erreur est survenue lors de la sélection du cartouche..."
              "\nLa référence de bloc cartouche n'a pas d'attributs sur la présentation \""
              layout
              "\"..."
            )
            nil
          )
        )
      )
      ( (member
          nil
          (setq lst
            (mapcar
              '(lambda (x / a v)
                (setq a x)
                (cond 
                  ( (not (listp x)) x)
                  ( (while
                      (and
                        (car a)
                        (= 'STR (type (car a)))
                        (or
                          (null (setq v (cdr (assoc (car a) att-list))))
                          (= "" v)
                        )
                        (setq a (cdr a))
                      )
                      (setq v nil)
                    )
                  )
                  ( (and v (listp (last x))) (apply (last x) (list v)))
                  (v)
                )
               )
              format
            )
          )
        )
        (alert-princ
          (LgT
            (strcat
              "\nAn error occurred during the cartridge selection..."
              "\nThe following attributes were not found on the cartridge of the layout tab \""
              layout
              "\"...\n  - "
              (lst2str
                (mapcar
                  'caar
                  (vl-remove-if
                    '(lambda (x) (cdr x))
                    (mapcar 'cons format lst)
                  )
                )
                "\n  - "
              )
            )
            (strcat
              "\nUne erreur est survenue lors de la sélection du cartouche..."
              "\nLes attributs suivant n'ont pas été trouvés dans le cartouche sur la présentation \""
              layout
              "\"...\n  - "
              (lst2str
                (mapcar
                  'caar
                  (vl-remove-if
                    '(lambda (x) (cdr x))
                    (mapcar 'cons format lst)
                  )
                )
                "\n  - "
              )
            )
            nil
          )
        )
        (setq lst nil)
      )
      ( (setq lst (apply 'strcat lst)))
    )
    lst
  )
  
  (defun set-cart-name (layout / str)
    (setq str
      (get-cart-name
        layout
        (cond
          ((= mode-n "Simplified") s-lst)
          ((= mode-n "Detailed") d-lst)
        )
      )
    )
    (if (and str (setq str (set-layout-name (set-layout-name layout (strcat "$temp" (itoa (setq i (1+ i))) "$")) (strcase str))))
      (princ
        (LgT
          (strcat
            "\nThe layout tab \""
            layout
            "\" has been successfully renamed and now has the name \""
            str
            "\"..."
          )
          (strcat
            "\nLa présentation \""
            layout
            "\" a été renommée avec succès et possède désormais le nom \""
            str
            "\"..."
          )
          nil
        )
      )
      (princ
        (LgT
          (strcat
            "\nAn error occurred on the layout tab \""
            layout
            "\" (see above for more details) and couldn't be renamed..."
          )
          (strcat
            "\nUne erreur est survenue sur la présentation \""
            layout
            "\" (voir ci-dessus pour plus de détails) et n'a pas pu être renommée..."
          )
          nil
        )
      )
    )
  )

  (sssetfirst nil nil)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq
    i 100
    Phase (substr (getvar "DWGNAME") (+ 2 (vl-string-position (ascii "_") (getvar "DWGNAME"))) 1)
    laylist (layoutlist)
    s-lst
      (list
        (list "N°DESSIN" "N°_DESSIN" '(lambda (s) (substr s 1 4)))
        "-"
        '("PLANCHE")
      )
    d-lst
      (list
        '("CODE" "CODEPROJET")
        "_"
        Phase
        "_"
        (list "N°DESSIN" "N°_DESSIN" '(lambda (s) (substr s 1 4)))
        "-"
        '("PLANCHE")
        "_"
        (list "TITRE_2" '(lambda (s) (vl-string-translate "/,;\\:?*" ".....  " s)))
      )
  )
  (cond
    ((= Phase "S") (setq Phase "ESQ"))
    ((= Phase "A") (setq Phase "APS"))
    ((= Phase "E") (setq Phase "APD"))
    ((= Phase "P") (setq Phase "PRO"))
    ((= Phase "C") (setq Phase "DCE"))
    ((= Phase "X") (setq Phase "EXE"))
    ((= Phase "D") (setq Phase "DOE"))
    ((setq Phase "Masquer"))
  )
  (setq mode-s
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nWhich layout tab(s) would you like to rename"
        "\nQuelle(s) présentation(s) souhaitez-vous renommer"
        nil
      )
      (list
        (LgT
          "Current Selection All _Current Selection All"
          "Active Sélection Toutes _Current Selection All"
          nil
        )
      )
      " ? "
      "Current"
      (LgT
        (strcat
          "\nNAMECART : Selection mode for layout tabs"
          "\nDefault value:     Current"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Selects only the current presentation, i.e. the layout tab you are  |"
          "\n  |   Current   | currently working on. If you are currently on the \"OBJECT\" tab, the |"
          "\n  |             | last active layout will be considered for the rest of the program   |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Opens a multiple choice selection dialog box in order to specify    |"
          "\n  |  Selection  | the list of layout tabs you want to rename among all layout tabs of |"
          "\n  |             | the drawing. The pre-selected value at the opening of the dialog    |"
          "\n  |             | box corresponds to the current layout tab (except for MODEL space)  |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |     All     | All the layout tabs of the current drawing are selected to be       |"
          "\n  |             | renamed (except the layouts named \"TOOLKIT\")                        |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nNAMECART : Mode de sélection des présentations"
          "\nValeur par défaut: Active"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Sélectionne uniquement la présentation courante, c'est-à-dire la    |"
          "\n  |    Active   | présentation sur laquelle vous êtes actuellement. Si vous êtes      |"
          "\n  |             | actuellement sur l'onglet \"OBJET\", la dernière présentation active  |"
          "\n  |             | sera considérée pour la suite du programme                          |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Ouvre une boîte de dialogue de sélection à choix multiple dans le   |"
          "\n  |             | but de spécifier la liste des présentations que vous souhaitez      |"
          "\n  |  Sélection  | renommer parmi l'ensemble des présentations du dessin. La valeur    |"
          "\n  |             | pré-sélectionnée à l'ouverture correspond à la présentation         |"
          "\n  |             | courante (à l'exception de l'espace OBJET)                          |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Toutes les présentations du dessin en cours sont sélectionnées pour |"
          "\n  |    Toutes   | être renommées (à l'exception des présentations nommées \"TOOLKIT\"   |"
          "\n  |             | et/ou \"TRAVAIL\")                                                    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (setq mode-n
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nSpecify the writing format that will be used to rename the layout tab(s)"
        "\nSpécifiez le format d'écriture qui sera utilisé pour renommer la/les présentation(s)"
        nil
      )
      (list
        (LgT
          "Simplified Detailed _Simplified Detailed"
          "Simplifiée Détaillée _Simplified Detailed"
          nil
        )
      )
      " : "
      "Simplified"
      (LgT
        (strcat
          "\nNAMECART : Writing format"
          "\nDefault value:     Simplified"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Uses the format ####-## with :                                      |"
          "\n  |             | ####     -> corresponds to the index of the plan (attribute of the  |"
          "\n  |             |             cartridge named \"N°_DESSIN\")                            |"
          "\n  |  Simplified | ##       -> corresponds to the sheet number (attribute of the       |"
          "\n  |             |             cartridge named \"PLANCHE\")                              |"
          "\n  |             | The main interest of this format is the short length of the layout  |"
          "\n  |             | name, allowing to display more layout tabs at the bottom            |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Uses the format Cxxxx_[SAECXD]_####-##_... with :                   |"
          "\n  |             | Cxxxx    -> corresponds to the project code (attribute of the       |"
          "\n  |             |             cartridge named \"CODE\")                                 |"
          "\n  |             | [SAECXD] -> corresponds to the letter representing the phase design |"
          "\n  |             |             (retrieved from the .dwg name after the first \"_\")      |"
          "\n  |             | ####     -> corresponds to the index of the plan (attribute of the  |"
          "\n  |   Detailed  |             cartridge named \"N°_DESSIN\")                            |"
          "\n  |             | ##       -> corresponds to the sheet number (attribute of the       |"
          "\n  |             |             cartridge named \"PLANCHE\")                              |"
          "\n  |             | ...      -> corresponds to the title of the plan (attribute of the  |"
          "\n  |             |             cartridge named \"TITRE_2\")                              |"
          "\n  |             | The main interest of this format is that it corresponds to the      |"
          "\n  |             | format used by PDF files, thus simplifying PDF printing             |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nNAMECART : Format d'écriture"
          "\nValeur par défaut: Simplifiée"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Utilise le format ####-## avec :                                    |"
          "\n  |             | ####     -> correspond à l'indice du plan (attribut du cartouche    |"
          "\n  |             |             nommé \"N°_DESSIN\")                                      |"
          "\n  |  Simplifiée | ##       -> correspond au numéro de la planche (attribut du         |"
          "\n  |             |             cartouche nommé \"PLANCHE\")                              |"
          "\n  |             | Le principal intérêt de ce format est la faible longueur du nom,    |"
          "\n  |             | permettant ainsi d'afficher davantage de présentation en bas        |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Utilise le format Cxxxx_[SAECXD]_####-##_... avec :                 |"
          "\n  |             | Cxxxx    -> correspond au code projet (attribut du cartouche nommé  |"
          "\n  |             |             \"CODE\")                                                 |"
          "\n  |             | [SAECXD] -> correspond à la lettre représentant la phase de design  |"
          "\n  |             |             (récupérée dans le nom du .dwg après le premier \"_\")    |"
          "\n  |             | ####     -> correspond à l'indice du plan (attribut du cartouche    |"
          "\n  |  Détaillée  |             nommé \"N°_DESSIN\")                                      |"
          "\n  |             | ##       -> correspond au numéro de la planche (attribut du         |"
          "\n  |             |             cartouche nommé \"PLANCHE\")                              |"
          "\n  |             | ...      -> correspond au titre du plan (attribut du cartouche      |"
          "\n  |             |             nommé \"TITRE_2\")                                        |"
          "\n  |             | Le principal intérêt de ce format est qu'il correspond au format    |"
          "\n  |             | utilisé par les fichiers PDF, simplifiant ainsi l'impression PDF    |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (cond
    ( (= mode-s "Current")
      (setvar "TILEMODE" 0)
      (setq layout (getvar "CTAB"))
      (set-cart-name layout)
    )
    ( (member mode-s '("Selection" "All"))
      (setq layout (getvar "CTAB"))
      (cond
        ((= mode-s "Selection")
          (setq ll
            (ListBox
              (LgT
                "NAMECART: Layout tab(s) selection"
                "NAMECART: Sélection des présentation(s)"
                nil
              )
              (LgT
                "Please, select one or more layout tab(s) to be renamed :"
                "Veuillez sélectionner la ou les présentation(s) à renommer :"
                nil
              )
              (vl-remove-if
                '(lambda (x) (member (strcase x) '("MODEL" "TOOLKIT" "TRAVAIL")))
                (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
              )
              layout
              2
              nil
            )
          )
        )
        ((= mode-s "All")
          (setq ll
            (vl-remove-if
              '(lambda (x) (member (strcase x) '("MODEL" "TOOLKIT" "TRAVAIL")))
              (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
            )
          )
        )
      )
      (foreach layout ll
        (set-cart-name layout)
      )
    )
  )
  (sssetfirst nil nil)
  (if (setq lst (vl-remove-if '(lambda (x) (= (car x) (cdr x))) (mapcar 'cons laylist (set-layouts-pos '<))))
    (princ
      (LgT
        (strcat
          "\nThe following layout tab(s) have been moved after the sorting functionality:"
          (apply
            'strcat
            (mapcar
              '(lambda (x)
                (strcat
                  "\n   The layout tab \""
                  (car x)
                  "\" has been replaced by \""
                  (cdr x)
                  "\" at the position "
                  (itoa (vla-get-taborder (vla-item (vla-get-layouts (vla-get-ActiveDocument (vlax-get-acad-object))) (cdr x))))
                  "..."
                )
               )
              lst
            )
          )
        )
        (strcat
          "\nLa ou les présentation(s) suivantes ont été déplacées après la fonctionnalité de tri :"
          (apply
            'strcat
            (mapcar
              '(lambda (x)
                (strcat
                  "\n   La présentation \""
                  (car x)
                  "\" a été remplacée par \""
                  (cdr x)
                  "\" à la position "
                  (itoa (vla-get-taborder (vla-item (vla-get-layouts (vla-get-ActiveDocument (vlax-get-acad-object))) (cdr x))))
                  "..."
                )
               )
              lst
            )
          )
        )
        nil
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [PsViewp] VIEWPORT MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:COPYCHSPACE" "23/09/2022" "Luna" "1.0.0" "\"PsViewp\"")                     ;--•  Add the command's informations to $lst$ variable  ;

;                                        []-----------------------[] COPYCHSPACE []-----------------------[]                                        ;
;--- Date of creation       > 23/09/2022                                                                                                            ;
;--- Last modification date > 23/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "PsViewp"                                                                                                             ;

(defun c:COPYCHSPACE (/ jsel)
  (if (and (= 0 (getvar "TILEMODE")) (setq jsel (ssget)))
    (progn
      (command "_COPY" jsel "" "" "0.0,0.0,0.0")
      (command "_CHSPACE" jsel "")
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:FNTAERIENNE" "04/07/2022" "Luna" "2.0.1" "\"PsViewp\"")                     ;--•  Add the command's informations to $lst$ variable  ;

;                                        []-----------------------[] FNTAERIENNE []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.1                                                                                                                 ;
;--- Class                  > "PsViewp"                                                                                                             ;

(defun c:FNTAERIENNE ( / *error* CMDECHO NOMUTT lst jsel i name )
  (defun *error* (msg)
    (SetVarList (list (list "CMDECHO" nil CMDECHO) (list "NOMUTT" nil NOMUTT)))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("CMDECHO" CMDECHO 0) ("NOMUTT" NOMUTT nil)))
  (setq lst '("UBS-900-*AERIENNE*" "UBS-100-CHAMP*PV*"))
  (if (= "Model" (getvar "CTAB"))
    (setvar
      "CTAB"
      (ListBox
        (LgT
          "FNTAERIENNE: Current layout tab selection"
          "FNTAERIENNE: Sélection de la présentation courante"
          nil
        )
        (LgT
          "The command FNTAERIENNE can't work from Model space, please select a layout tab :"
          "La commande FNTAERIENNE ne peut pas fonctionner depuis l'espace Objet, sélectionner une présentation :"
          nil
        )
        (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
        nil
        0
        nil
      )
    )
  )
  (command "_PSPACE")
  (cond
    ( (null (setq lst (flt_tbl "LAYER" (lst2str lst ",") nil)))
      (princ (LgT "No layer(s) found." "Aucun calque(s) trouvé(s)." nil))
    )
    ( (null
        (setq jsel
          (MuteSel
            (LgT
              "\nPlease select one or several viewport(s) : "
              "\nSélectionner une ou des fenêtre(s) de présentation : "
              nil
            )
            (quote (ssget '((0 . "VIEWPORT"))))
          )
        )
      )
      (princ (LgT "No object(s) found." "Aucun objet(s) trouvé(s)." nil))
    )
    ( T
      (setvar "NOMUTT" 1)
      (repeat (setq i (sslength jsel))
        (setq name (ssname jsel (setq i (1- i))))
        (command "_-VPORTS" "_ON" name "")
        (command "_MSPACE")
        (command "_VPLAYER" "_Freeze" "*" "_Current" "_Thaw" (lst2str lst ",") "_Current" "")
        (command "_PSPACE")
      )
      (setvar "NOMUTT" NOMUTT)
      (princ
        (strcat
          (LgT
            "\nThe following layers has been set for "
            "\nLes calques suivants ont été définis pour "
            nil
          )
          (itoa (sslength jsel))
          (LgT
            " viewport(s) on \""
            " fenêtre(s) de présentation sur \""
            nil
          )
          (getvar "CTAB")
          "\" :"
          "\n  • "
          (lst2str lst "\n  • ")
        )
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setvar "CMDECHO" CMDECHO)
  (princ)
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [PsLaytb] LAYOUT-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:LAYOUTCOPY" "19/12/2023" "Luna" "1.0.3" "\"PsLaytb\"")                      ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] LAYOUTCOPY []-----------------------[]                                        ;
;--- Date of creation       > 29/09/2022                                                                                                            ;
;--- Last modification date > 19/12/2023                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.3                                                                                                                 ;
;--- Class                  > "PsLaytb"                                                                                                             ;

(defun c:LAYOUTCOPY ( / layout layouts tbo tabs)
  (if (= "Model" (setq layout (getvar "CTAB")))
    (princ
      (LgT
        "\nThe Model tab can't be copied..."
        "\nL'onglet Objet ne peut pas être copié..."
        nil
      )
    )
    (progn
      (setq
        layouts (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object)))
        tbo (vla-get-TabOrder (vla-item layouts layout))
      )
      (command-s "_-LAYOUT" "_Copy" "" "")
      (vlax-for l layouts (setq tabs (cons (cons (vla-get-TabOrder l) (vla-get-Name l)) tabs)))
      (setq layout (cdr (assoc tbo tabs)))
      (setvar "CTAB" layout)
      (princ
        (strcat
          (LgT "\nThe new layout created \"" "\nLa nouvelle présentation crée \"" nil)
          layout
          (LgT "\" is now the current layout." "\" est désormais la présentation courante." nil)
        )
      )
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:QUICKLAYOUTSWITCH" "22/09/2022" "Luna" "3.0.1" "\"PsLaytb\"")               ;--•  Add the command's informations to $lst$ variable  ;

;                                     []-----------------------[] QuickLayoutSwitch []-----------------------[]                                     ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 22/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.1                                                                                                                 ;
;--- Class                  > "PsLaytb"                                                                                                             ;

(defun c:QuickLayoutSwitch ( / layout)
  (setq layout
    (ListBox
      (LgT
        "QUICKLAYOUTSWITCH: Layout tab selection"
        "QUICKLAYOUTSWITCH: Sélection de la présentation"
        nil
      )
      (LgT
        "Please, select the layout tab you want to set to current:"
        "Veuillez sélectionner la présentation que vous souhaitez rendre courante :"
        nil
      )
      (append '("Model") (layoutlist))
      (cond ((vlax-ldata-get "URBASOLAR" "QLS_Settings")) ((getvar "CTAB")))
      0
      nil
    )
  )
  (if layout
    (progn
      (vlax-ldata-put "URBASOLAR" "QLS_Settings" (getvar "CTAB"))
      (setvar "CTAB" layout)
    )
  )
  (princ)
)



;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [SdBlock] BLOCK LIST-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:FIXBADATTBLOCKS" "30/11/2018" "BeekeeCZ" "1.0.0" "\"SdBlock\"")             ;--•  Add the command's informations to $lst$ variable  ;

;                                      []-----------------------[] FixBadAttBlocks []-----------------------[]                                      ;
;--- Date of creation       > 30/11/2018                                                                                                            ;
;--- Last modification date > 30/11/2018                                                                                                            ;
;--- Author                 > BeeKeeCZ                                                                                                              ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "SdBlock"                                                                                                             ;

(defun c:FixBadAttBlocks ( / gettags bkc bln doc lst tmp )
  (defun gettags ( def / rtn )
    (vlax-for obj def
      (if (= "AcDbAttributeDefinition" (vla-get-objectname obj))
        (setq rtn (cons (strcase (vla-get-tagstring obj)) rtn))
      )
    )
    rtn
  )

  (vl-load-com)
  (setq
    doc (vla-get-activedocument (vlax-get-acad-object))
    bkc (vla-get-blocks doc)
  )
  (vlax-for blk bkc
    (if (= :vlax-false (vla-get-isxref blk))
      (vlax-for obj blk
        (if
          (and
            (= "AcDbBlockReference" (vla-get-objectname obj))
            (= :vlax-true (vla-get-hasattributes obj))
            (or
              (setq tmp (assoc (setq bln (vla-get-name obj)) lst))
              (and
                (setq tmp (cons bln (gettags (vla-item bkc bln))))
                (setq lst (cons tmp lst))
              )
            )
          )
          (foreach att (vlax-invoke obj 'getattributes)
            (or
              (member (strcase (vla-get-tagstring att)) (cdr tmp))
              (and
                (vlax-write-enabled-p att)
                (vla-delete att)
              )
            )
          )
        )
      )
    )
  )
  (vla-regen doc acallviewports)
  (princ)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [SdLayer] LAYER LIST-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [SdXdata] EXTENDED DATA MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;


;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtFiles] FILE-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:ADM-ACTIVATE" "06/10/2022" "Luna" "2.1.0" "\"UtFiles\"")                    ;--•  Add the command's informations to $lst$ variable  ;

;                                        []-----------------------[] ADM-Activate []-----------------------[]                                       ;
;--- Date of creation       > 15/09/2022                                                                                                            ;
;--- Last modification date > 06/10/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.1.0                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:ADM-Activate ( / tmp dwg docs doc )
  (and
    (setq docs (mapcar 'car (vla-collection->list (vlax-get-acad-object) 'Documents 1)))
    (if (not (setq tmp (car (vl-remove (getvar "DWGNAME") docs))))
      (not
        (princ
          (LgT
            "\nThere's only one drawing in the current AutoCAD application..."
            "\nIl n'y a qu'un seul dessin dans l'application en cours d'AutoCAD..."
            nil
          )
        )
      )
      tmp
    )
    (setq dwg
      (cond
        ( (setq dwg (getenv "ADM_Drawing"))
          (if (vl-position (strcase dwg) (mapcar 'strcase docs))
            dwg
            tmp
          )
        )
        ( (setenv "ADM_Drawing" tmp))
      )
    )
    (setq doc
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease enter the drawing name to activate or"
          "\nEntrer le nom du dessin à activer ou"
          nil
        )
        (list 128 (LgT "Name _Name" "Nommer _Name" nil))
        " : "
        dwg
        nil
      )
    )
    (cond
      ( (= "Name" doc)
        (setq doc
          (ListBox
            (LgT "ADM-Activate: Drawing selection" "ADM-Activate: Sélection du dessin" nil)
            (LgT "Please select a drawing within the list :" "Sélectionner un dessin dans la liste :" nil)
            docs
            dwg
            0
            nil
          )
        )
      )
      ( (not (wcmatch (strcase doc) "*.DWG")) (setq doc (strcat doc ".DWG")))
      (doc)
    )
    (setq dwg (getvar "DWGNAME"))
    (not
      (if (equal (strcase dwg) (strcase doc))
        (princ
          (LgT
            (strcat "\nThe drawing named \"" dwg "\" is already active...")
            (strcat "\nLe dessin nommé \"" dwg "\" est déjà actif...")
            nil
          )
        )
      )
    )
    (if (setq dwg (vl-position (strcase doc) (mapcar 'strcase docs)))
      (setq doc (nth dwg docs))
      (not
        (princ
          (LgT
            (strcat "\nThe drawing named \"" doc "\" wasn't found in opened drawing's list...")
            (strcat "\nLe dessin nommé \"" doc "\" n'a pas été trouvé dans la liste des dessins ouverts...")
            nil
          )
        )
      )
    )
    (princ doc)
    (setenv "ADM_Drawing" (getvar "DWGNAME"))
    (ApplyDocsMethods (list doc) (list (cons 'Activate nil)))
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:ADM-CLOSE" "19/09/2022" "Luna" "1.0.1" "\"UtFiles\"")                       ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] ADM-Close []-----------------------[]                                        ;
;--- Date of creation       > 15/09/2022                                                                                                            ;
;--- Last modification date > 19/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:ADM-Close ( / dwg docs doc mode )
  (and
    (setq dwg (getvar "DWGNAME"))
    (setq docs (mapcar 'car (vla-collection->list (vlax-get-acad-object) 'Documents 1)))
    (setq doc
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease enter the drawing name(s) to close or"
          "\nEntrer le nom du/des dessin(s) à fermer ou"
          nil
        )
        (list 128 (LgT "allButActive List _allButActive List" "tousSaufCourant Liste _allButActive List" nil))
        " : "
        (car (vl-remove dwg docs))
        nil
      )
    )
    (cond
      ( (= "allButActive" doc) (setq doc T))
      ( (= "List" doc)
        (setq doc
          (ListBox
            (LgT "ADM-Close: Drawing(s) selection" "ADM-Close: Sélection du/des dessin(s)" nil)
            (LgT "Please select drawing(s) within the list :" "Sélectionner un/des dessin(s) dans la liste :" nil)
            (vl-remove dwg docs)
            dwg
            2
            nil
          )
        )
      )
      ( (setq doc (mapcar '(lambda (x) (if (wcmatch (strcase x) "*.DWG") x (strcat x ".DWG"))) (str2lst doc ","))))
    )
    (if (listp doc)
      (setq doc (vl-remove-if-not '(lambda (x) (member (strcase x) (mapcar 'strcase doc))) docs))
      T
    )
    (setq doc (if (and (listp doc) (member dwg doc)) (vl-remove dwg doc) doc))
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT "\nDo you want to save the changes?" "\nVoulez-vous enregistrer les modifications ?" nil)
        (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
        " "
        "Yes"
        nil
      )
    )
    (cond
      ( (= "Yes" mode) (setq mode :vlax-true))
      ( (= "No" mode) (setq mode :vlax-false))
    )
    (setq doc (ApplyDocsMethods doc (list (cons 'Close mode))))
    (princ
      (strcat
        "\n " (itoa (length (cdr doc))) " / " (itoa (car doc))
        (LgT
          (strcat " drawings has been closed successfully and " (if (= mode :vlax-true) "saved" "not saved") ":")
          (strcat " dessin(s) a/ont été fermé(s) avec succès et " (if (= mode :vlax-true) "enregistré(s)" "pas enregistré(s)") " :")
          nil
        )
        "\n  • "
        (lst2str (mapcar 'car (cdr doc)) "\n  • ")
        (if (= mode :vlax-true)
          (strcat
            "\n"
            (LgT "(for the new drawings saved, check here : \"" "(pour les nouveaux dessins enregistrés, vérifier ici : \"" nil)
            (strcat "C:\\Users\\" (getenv "USERNAME") "\\Documents")
            "\")"
          )
          ""
        )
      )
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:ADM-SAVE" "22/09/2022" "Luna" "1.0.1" "\"UtFiles\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] ADM-Save []-----------------------[]                                         ;
;--- Date of creation       > 15/09/2022                                                                                                            ;
;--- Last modification date > 22/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:ADM-Save ( / dwg docs doc )
  (and
    (setq dwg (getvar "DWGNAME"))
    (setq docs (mapcar 'car (vla-collection->list (vlax-get-acad-object) 'Documents 1)))
    (setq doc
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nPlease enter the drawing name(s) to save or"
          "\nEntrer le nom du/des dessin(s) à enregistrer ou"
          nil
        )
        (list 128 (LgT "All allButActive List _All allButActive List" "Tous tousSaufCourant Liste _All allButActive List" nil))
        " : "
        dwg
        nil
      )
    )
    (cond
      ( (= "All" doc) (not (setq doc nil)))
      ( (= "allButActive" doc) (setq doc T))
      ( (= "List" doc)
        (setq doc
          (ListBox
            (LgT "ADM-Save: Drawing(s) selection" "ADM-Save: Sélection du/des dessin(s)" nil)
            (LgT "Please select drawing(s) within the list :" "Sélectionner un/des dessin(s) dans la liste :" nil)
            docs
            dwg
            2
            nil
          )
        )
      )
      ( (setq doc (mapcar '(lambda (x) (if (wcmatch (strcase x) "*.DWG") x (strcat x ".DWG"))) (str2lst doc ","))))
    )
    (if (and doc (listp doc))
      (setq doc (vl-remove-if-not '(lambda (x) (member (strcase x) (mapcar 'strcase doc))) docs))
      T
    )
    (cdr (setq doc (ApplyDocsMethods doc (list (cons 'Save nil)))))
    (princ
      (strcat
        "\n " (itoa (length (cdr doc))) " / " (itoa (car doc))
        (LgT
          " drawings has been saved successfully :"
          " dessin(s) a/ont été enregistré(s) avec succès :"
          nil
        )
        "\n  • "
        (lst2str (mapcar 'car (cdr doc)) "\n  • ")
        "\n\n"
        (LgT "(for the new drawings saved, check here : \"" "(pour les nouveaux dessins enregistrés, vérifier ici : \"" nil)
        (strcat "C:\\Users\\" (getenv "USERNAME") "\\Documents")
        "\")"
      )
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:HANDLEPREVIEW" "18/07/2022" "Luna" "1.3.0" "\"UtFiles\"")                   ;--•  Add the command's informations to $lst$ variable  ;

;                                       []-----------------------[] HANDLEPREVIEW []-----------------------[]                                       ;
;--- Date of creation       > 06/07/2022                                                                                                            ;
;--- Last modification date > 18/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.3.0                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:HANDLEPREVIEW (/ *error* infolist doc spc sep jsel vctr vsiz CMDECHO mode filename dwg hdl lst line obj e s param option old break tmp i l)
  (defun *error* (msg)
    (setvar "CMDECHO" 0)
    (command-s "_ZOOM" "_C" vctr vsiz)
    (setvar "CMDECHO" CMDECHO)
    (sssetfirst)
    (princ msg)
  )
  (defun infolist (fun foo hlst / csv)
    (setq csv (vl-remove-if-not '(lambda (x) (member (nth pos x) hlst)) lst))
    (mapcar
      '(lambda (a b) (eval foo))
      (car csv)
      (apply 'mapcar (cons fun csv))
    )
  )

  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    spc (vla-get-Block (vla-get-ActiveLayout doc))
    sep ", "
    jsel (ssadd)
  )
  (SetVarList '(("VIEWCTR" vctr nil) ("VIEWSIZE" vsiz nil) ("CMDECHO" CMDECHO nil)))
  (sssetfirst)
  (setq mode
    (getkdh
      (quote (getkword msg))
      (LgT
        "\nEnter the handle(s) manually or"
        "\nEntrer le(s) handle(s) manuellement ou"
        nil
      )
      (list 128 (LgT "File _File" "Fichier _File" nil))
      " : "
      nil
      (LgT
        (strcat
          "\nHANDLEPREVIEW : Handle's listing method"
          "\nDefault value:     <none>"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |     File    | Opens a dialog box to search for a .csv file and read it. Only .csv |"
          "\n  |             | files with a column named Handle are allowed for this command       |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | You can also enter directly the name of the handles you're actually |"
          "\n  |    Write    | searching for as an answer. If you want to select multiple objects  |"
          "\n  |             | you can separate each handle's name with a comma (,) but no spaces  |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        (strcat
          "\nHANDLEPREVIEW : Méthode pour lister les handles"
          "\nValeur par défaut: <aucune>"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Ouvre une boîte de dialogue pour rechercher un fichier .csv et le   |"
          "\n  |   Fichier   | lire. Seuls les fichiers .csv avec une colonne nommée Handle sont   |"
          "\n  |             | autorisés pour cette command                                        |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n  |             | Vous pouvez également entrer directement le nom des handles que     |"
          "\n  |   Ecriture  | vous cherchez comme réponse. Si vous souhaitez sélectionner         |"
          "\n  |             | plusieurs objets, vous pouvez séparer chaque nom d'handle par une   |"
          "\n  |             | virgule (,) mais sans espaces                                       |"
          "\n  +-------------+---------------------------------------------------------------------+"
          "\n"
        )
        nil
      )
    )
  )
  (cond
    ( (= "File" mode)
      (cond
        ( (not
            (setq filename
              (getfiled
                (LgT
                  "Selection of .csv import file"
                  "Sélection du fichier .csv d'import"
                  nil
                )
                (strcat (getvar "DWGPREFIX") (setq dwg (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))))
                "csv"
                128
              )
            )
          )
          (princ (LgT "\nNo .csv file selected..." "\nAucun fichier .csv sélectionné..." nil))
        )
        ( (not (wcmatch (strcase filename) (strcat "*" (strcase dwg) "*")))
          (princ
            (strcat
              (LgT
                "\nThe .csv file \""
                "\nLe fichier .csv \""
                nil
              )
              (substr filename (+ 2 (vl-string-position (ascii "\\") filename 0 T)))
              (LgT
                "\" is not from the current drawing \""
                "\" n'est pas issu du dessin courant \""
                nil
              )
              dwg
              "\"..."
            )
          )
        )
        ( (not (setq lst (read-file filename)))
          (princ
            (strcat
              (LgT
                "\nThe .csv file \""
                "\nLe fichier .csv \""
                nil
              )
              filename
              (LgT
                "\" is empty..."
                "\" est vide..."
                nil
              )
            )
          )
        )
        ( (and
            (not
              (while
                (and
                  (setq line (car lst))
                  (setq line (str2lst line ";"))
                  (null (setq pos (vl-position (strcase "Handle") (mapcar 'strcase line))))
                )
                (setq lst (cdr lst))
              )
            )
            (null pos)
          )
          (princ
            (LgT
              "\nThe .csv file doesn't have a column named \"Handle\"..."
              "\nLe fichier .csv n'a pas de colonne nommée \"Handle\"..."
              nil
            )
          )
        )
        ( (not
            (setq lst
              (mapcar
                '(lambda (x / s)
                  (setq s (str2lst x ";"))
                  (lst2str
                    (subst
                      (strcase (vl-string-trim (apply 'strcat (Wildcard-trim ".")) (nth pos s)))
                      (nth pos s)
                      s
                    )
                    sep
                  )
                 )
                (cdr lst)
              )
            )
          )
          (princ
            (LgT
              "\nThe .csv file doesn't have any value in \"Handle\" column..."
              "\nLe fichier .csv n'a pas une seule valeur dans la colonne \"Handle\"..."
              nil
            )
          )
        )
        ( (setq hdl
            (ListBox
              (LgT
                "HANDLEPREVIEW: Handle(s)'s selection"
                "HANDLEPREVIEW: Sélection des handle(s)"
                nil
              )
              (LgT
                "Select only the handle(s) you want to check on drawing :"
                "Sélectionnez seulement les handle(s) que vous souhaitez vérifier dans le dessin :"
                nil
              )
              lst
              lst
              2
              30
            )
          )
          (setq hdl (mapcar '(lambda (x) (strcase (nth pos (str2lst x sep)))) hdl))
        )
      )
    )
    ( (and mode (not (= "" mode)))
      (setq hdl
        (str2lst
          (strcase (vl-string-translate (apply 'strcat (Wildcard-trim ".")) (mAtom "," (length (Wildcard-trim ".")) 'strcat) mode))
          ","
        )
      )
    )
  )
  (if lst (setq lst (mapcar '(lambda (l) (str2lst l sep)) lst)))
  (foreach h (reverse hdl)
    (if
      (and
        (if (not (vl-catch-all-error-p (setq obj (vl-catch-all-apply 'vla-HandleToObject (list doc h))))) T (not (setq e (cons h e))))
        (if
          (and
            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-ObjectIdToObject (list doc (vla-get-OwnerId obj)))))
            (equal (vla-ObjectIdToObject doc (vla-get-OwnerId obj)) spc)
          )
          T
          (not (setq s (cons h s)))
        )
      )
      (setq
        obj (vlax-vla-object->ename obj)
        tmp (cons h tmp)
        jsel (ssadd obj jsel) 
      )
    )
  )
  (if e
    (princ
      (strcat
        (LgT
          "\nWARNING: Some specified handle(s) wasn't found in the current drawing :"
          "\nATTENTION: Certain(s) handle(s) sépcifié(s) n'ont pas été trouvé(s) sur le dessin courant :"
          nil
        )
        "\n"
        (lst2str e ", ")
      )
    )
  )
  (if s
    (princ
      (strcat
        (LgT
          "\nWARNING: Some specified handle(s) wasn't found in the current layout but exists in the current drawing"
          "\nATTENTION: Certain(s) handle(s) sépcifié(s) n'ont pas été trouvé(s) sur la présentation courante mais existe(nt) dans le dessin courant"
          nil
        )
        "\n"
        (lst2str s ", ")
      )
    )
  )
  (if (< 0 (sslength jsel))
    (progn
      (sssetfirst)
      (sssetfirst nil (ZoomObjects jsel))
      (setq
        i -1
        l (length tmp)
        param
          (list
            (cons "Previous" "Précédent")
            (cons "Next" "Suivant")
            (cons "All" "Tous")
            (cons "Handle" "Handle")
            (cons "eXit" "Quitter")
          )
        option
          (LgT
            (lst2str (list (lst2str (mapcar 'car param) " ") (lst2str (mapcar 'car param) " ")) " _")
            (lst2str (list (lst2str (mapcar 'cdr param) " ") (lst2str (mapcar 'car param) " ")) " _")
            nil
          )
        old "eXit"
      )
      (while
        (and
          (not break)
          (if lst
            (princ
              (strcat
                "\n"
                (lst2str
                  (mapcar
                    '(lambda (e v) (strcat e " = " v))
                    line
                    (infolist
                      '=
                      '(if b a "*VARIES*")
                      (if (minusp i) tmp (list (nth i tmp)))
                    )
                  )
                  ", "
                )
              )
            )
            (cond
              ( (= 1 (sslength jsel)) (princ (strcat "\nHandle = " (car tmp))))
              ( (minusp i) (princ "\nHandle = *VARIES*"))
              ( (princ (strcat "\nHandle = " (nth i tmp))))
            )
          )
          (setq mode old mode
            (getkdh
              (quote (getkword msg))
              (strcat
                "\n n°"
                (itoa (if (minusp i) l (1+ i)))
                " / "
                (itoa l)
                (LgT
                  " | Zoom on"
                  " | Zoom sur"
                  nil
                )
              )
              (list 128 option)
              " : "
              (LgT mode (cdr (assoc mode param)) nil)
              (LgT
                (strcat
                  "\nHANDLEPREVIEW : Selection preview (zoom)"
                  "\nDefault value:     <" (LgT mode (cdr (assoc mode param)) nil) ">"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |   Previous  | Zooms in on the object before the current object in selection set   |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     Next    | Zooms in on the object after the current object in selection set    |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     All     | Zooms in on all objects in the selection set                        |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |    Handle   | Opens a dialog box to select the wanted handle within the list and  |"
                  "\n  |             | zoom in on it                                                       |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     eXit    | Ends the command and keeps selected the preview's object(s)         |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |    Write    | You can also type one handle's name as answer (not case-sensitive)  |"
                  "\n  |             | or its position in the list (number) to zoom in on it directly      |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n"
                )
                (strcat
                  "\nHANDLEPREVIEW : Aperçu de la sélection (zoom)"
                  "\nValeur par défaut: <" (LgT mode (cdr (assoc mode param)) nil) ">"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |  Précédent  | Effectue un zoom sur l'objet précédant l'objet actuel contenu dans  |"
                  "\n  |             | le jeu de sélection                                                 |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |   Suivant   | Effectue un zoom sur l'objet suivant l'objet actuel contenu dans le |"
                  "\n  |             | jeu de sélection                                                    |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |     Tous    | Effectue un zoom sur l'ensemble des objets contenus dans le jeu de  |"
                  "\n  |             | sélection                                                           |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |    Handle   | Ouvre une boîte de dialogue pour sélectionner le handle désiré dans |"
                  "\n  |             | la liste et effectue un zoom sur l'objet correspondant              |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |   Quitter   | Termine la commande et garde sélectionné le(s) objet(s) de l'aperçu |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n  |             | Vous pouvez également taper le nom du handle comme réponse (non     |"
                  "\n  |   Ecriture  | sensible à la casse) ou sa position (numéro) dans la liste pour     |"
                  "\n  |             | zoomer directement dessus                                           |"
                  "\n  +-------------+---------------------------------------------------------------------+"
                  "\n"
                )
                nil
              )
            )
          )
          (null (sssetfirst))
        )
        (cond
          ( (= "eXit" mode)
            (setq break T)
            (setvar "CMDECHO" 0)
            (command-s "_ZOOM" "_C" vctr vsiz)
            (setvar "CMDECHO" CMDECHO)
          )
          ( (= "Next" mode) (setq old mode i (rem (1+ i) l)))
          ( (= "Previous" mode) (setq old mode i (if (minusp (1- i)) (1- l) (1- i))))
          ( (= "All" mode) (setq old mode i -1))
          ( (= "Handle" mode)
            (setq
              old mode
              i
                (ListBox
                  (LgT
                    "HANDLEPREVIEW: Handle's selection"
                    "HANDLEPREVIEW: Sélection du handle"
                    nil
                  )
                  (LgT
                    "Select the handle you want to check on drawing :"
                    "Sélectionnez le handle que vous souhaitez vérifier dans le dessin :"
                    nil
                  )
                  (if lst (mapcar '(lambda (x) (lst2str x " ; ")) (cdr lst)) tmp)
                  nil
                  0
                  nil
                )
              i
                (if lst
                  (vl-position (str2lst i " ; ") (cdr lst))
                  (vl-position (strcase i) tmp)
                )
            )
          )
          ( mode
            (cond
              ( (distof mode)
                (if (< 0 (atoi mode) (1+ l)) (setq i (1- (atoi mode))))
              )
              ( (member (strcase mode) tmp) (setq i (vl-position (strcase mode) tmp)))
            )
          )
        )
        (if (minusp i)
          (sssetfirst nil (ZoomObjects jsel))
          (sssetfirst nil (ZoomObjects (ssadd (handent (nth i tmp)))))
        )
      )
      (setvar "CMDECHO" 0)
      (command-s "_ZOOM" "_C" vctr vsiz)
      (setvar "CMDECHO" CMDECHO)
      (sssetfirst)
      (if (minusp i)
        (sssetfirst nil jsel)
        (sssetfirst nil (ssadd (handent (nth i tmp))))
      )
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:RPCUSTOM" "26/05/2022" "Didier" "1.0.1" "\"UtFiles\"")                      ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] RPCustom []-----------------------[]                                         ;
;--- Date of creation       > 11/03/2005                                                                                                            ;
;--- Last modification date > 26/05/2022                                                                                                            ;
;--- Author                 > Didier                                                                                                                ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:RPCustom ( / o h c)
  (mapcar 'set '(o h c) (mapcar 'getvar '("CMDECHO" "VIEWSIZE" "VIEWCTR")))
  (setvar "CMDECHO" 0)
  (command "_PLAN" "")
  (command "_ZOOM" "C" c h)
  (setvar "CMDECHO" o)
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:RADPURGE" "04/07/2022" "Luna" "1.1.2" "\"UtFiles\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] RADPURGE []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.2                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:RADPURGE (/ *error* doc $ layout layer)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    $ (vla-add (vla-get-layers doc) "$tmp$")
  )
  (setvar "CLAYER" "$tmp$")
  (setvar "CTAB" "Model")
  (vlax-for layout (vla-get-layouts doc)
    (if (/= (vla-get-name layout) "Model")
      (vla-delete layout)
    )
  )
  (vlax-for layer (vla-get-layers doc)
    (if (not (wcmatch (vla-get-name layer) "*|*,$tmp$"))
      (progn
        (vlax-put layer 'LayerOn -1)
        (vlax-put layer  'Freeze 0)
        (vlax-put layer 'Lock 0)
      )
    )
  )
  (setvar "CLAYER" "0")
  (setq $ (vla-delete $))
  (command "_-PURGE" "_All" "*" "_No")
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:VP-RADPURGE" "04/07/2022" "Luna" "2.1.1" "\"UtFiles\"")                     ;--•  Add the command's informations to $lst$ variable  ;

;                                        []-----------------------[] VP-RADPURGE []-----------------------[]                                        ;
;--- Date of creation       > 23/12/2019                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.1.1                                                                                                                 ;
;--- Class                  > "UtFiles"                                                                                                             ;

(defun c:VP-RADPURGE (/ *error* VP-get-layer-list doc CMDECHO NOMUTT CLAYER $ jsel name layer-list t0 i)
  (defun *error* (msg)
    (if layer-list (command-s "_UNDO" "_Back"))
    (setvar "CMDECHO" CMDECHO)
    (setvar "NOMUTT" NOMUTT)
    (if $
      (progn
        (setvar "CLAYER" CLAYER)
        (vla-delete $)
      )
    )
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun VP-get-layer-list (name flag / VP-get-frozen-layers lst)
    (defun VP-get-frozen-layers (vp / typ val)
      (vla-getXdata vp "ACAD" 'typ 'val)
      (cddr (reverse (cdr (member "{" (cdr (member "{" (mapcar 'vlax-variant-value (vlax-safearray->list val))))))))
    )
    (setq lst (VP-get-frozen-layers (vlax-ename->vla-object name)))
    (if flag
      (vl-remove-if '(lambda (x) (member x lst)) (mapcar 'car (vla-collection->list nil 'layers 1)))
      lst
    )
  )
  
  (sssetfirst nil nil)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("CMDECHO" CMDECHO 0) ("NOMUTT" NOMUTT nil)))
  (command-s "_UNDO" "_Mark")
  (setq
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    $ (vla-add (vla-get-layers doc) "$tmp$")
  )
  (SetVarList '(("CLAYER" CLAYER "$tmp$")))
  (if (= "Model" (getvar "CTAB"))
    (setvar
      "CTAB"
      (ListBox
        (LgT
          "VP-RADPURGE: Current layout tab selection"
          "VP-RADPURGE: Sélection de la présentation courante"
          nil
        )
        (LgT
          "The command VP-RADPURGE can't work from Model space, please select a layout tab :"
          "La commande VP-RADPURGE ne peut pas fonctionner depuis l'espace Objet, sélectionner une présentation :"
          nil
        )
        (vl-remove-if '(lambda (l) (member (strcase l) '("TOOLKIT" "TRAVAIL"))) (layoutlist))
        nil
        0
        nil
      )
    )
  )
  (command "_PSPACE")
  (while
    (progn
      (princ
        (LgT
          "\nPlease, select a single viewport :"
          "\nVeuillez sélectionner une seule fenêtre de présentation :"
          nil
        )
      )
      (setvar "NOMUTT" 1)
      (setq jsel (ssget "_+.:E:S" '((0 . "VIEWPORT"))))
      (setvar "NOMUTT" NOMUTT)
      (null jsel)
    )
  )
  (setq
    name (ssname jsel 0)
    layer-list (VP-get-layer-list name nil)
  )
  (if
    (setq layer-list
      (ListBox
        (LgT
          "VP-RADPURGE : Layer(s) selection"
          "VP-RADPURGE : Sélection des calques"
          nil
        )
        (LgT
          "Please, select layer(s) to be deleted :"
          "Sélectionner le(s) calque(s) à supprimer :"
          nil
        )
        (vl-sort (vl-remove-if '(lambda (l) (wcmatch l "*|*,$tmp$,0")) (mapcar 'car (vla-collection->list nil 'layers 0))) '<)
        layer-list
        2
        50
      )
    )
    (progn
      (setq t0 (getvar "MILLISECS"))
      (vlax-for layer (vla-get-layers doc)
        (if (not (wcmatch (vla-get-Name layer) "*|*,$tmp$"))
          (progn
            (vlax-put layer 'LayerOn -1)
            (vlax-put layer 'Freeze 0)
            (vlax-put layer 'Lock 0)
          )
        )
      )
      (SetVarList '(("CLAYER" nil "0") ("CECOLOR" nil "256") ("CELTYPE" nil "ByLayer") ("CTAB" nil "Model") ("NOMUTT" nil 1)))
      (setq $ (vla-delete $))
      (command "_-LAYDEL")
      (foreach layer layer-list
        (command "_Name" layer)
      )
      (command "" "_Yes")
      (setvar "NOMUTT" NOMUTT)
      (setq i 0)
      (repeat (1- (length (setq layer-list (divlist (vl-sort layer-list '<) (fixdiv (length layer-list) 50)))))
        (alert
          (strcat
            (LgT
              "See below, the list of layers that have been purged from the drawing :"
              "Ci-dessous la liste des calques ayant été purgé de ce dessin :"
              nil
            )
            "\n  - "
            (lst2str (nth i layer-list) "\n  - ")
            "\n"
            "\n( page "
            (itoa (setq i (1+ i)))
            " / "
            (itoa (length layer-list))
            " )"
          )
        )
      )
      (alert
        (strcat
          (LgT
            "See below, the list of layers that have been purged from the drawing :"
            "Ci-dessous la liste des calques ayant été purgé de ce dessin :"
            nil
          )
          "\n  - "
          (lst2str (nth i layer-list) "\n  - ")
          "\n"
          (LgT
            "\nThe command took "
            "\nLa commande a pris "
            nil
          )
          (rtos (* (- (getvar "MILLISECS") t0) 0.001) 2 1)
          (LgT
            " seconds to delete "
            " secondes pour supprimer "
            nil
          )
          (itoa (length (apply 'append layer-list)))
          (LgT
            " layer(s)..."
            " calque(s)..."
            nil
          )
          "\n"
          "\n( page "
          (itoa (setq i (1+ i)))
          " / "
          (itoa (length layer-list))
          " )"
        )
      )
      (if
        (=
          "No"
          (getkdh
            (quote (getkword msg))
            (LgT
              "\nDoes the drawing make sense to you"
              "\nLe dessin vous semble-t-il cohérent"
              nil
            )
            '(1 "Oui Non _Yes No")
            " ?"
            nil
            (LgT
              (strcat
                "\nVP-RADPURGE : User confirmation after the purge"
                "\nDefault value:     <none>"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | You are agree with the actual state of the drawing. The program     |"
                "\n  |             | will delete all the layout tabs, purge unreferenced objet(s) and    |"
                "\n  |     Yes     | display the list of deleted layer(s)                                |"
                "\n  |             | You still have the possibility to undo the command by using the     |"
                "\n  |             | 'Back' option of UNDO command to go back to the previous 'Mark'     |"
                "\n  |             | set at the begining of VP-RADPURGE                                  |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | You are not agree with the actual state of the drawing. The program |"
                "\n  |     No      | will go back to the previous 'Mark' (command UNDO) to reset the     |"
                "\n  |             | drawing to its state at the begining of VP-RADPURGE                 |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              (strcat
                "\nVP-RADPURGE : Confirmation utilisateur après la purge"
                "\nValeur par défaut: <aucune>"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Vous êtes d'accord avec l'état actuel du dessin. Le programme va    |"
                "\n  |             | supprimer tous les onglets de présentation, purger les objet(s)     |"
                "\n  |     Oui     | non référencés et afficher la liste des calque(s) supprimé(s)       |"
                "\n  |             | Vous avez toujours la possibilité d'annuler la commande en          |"
                "\n  |             | utilisant l'option 'Retour' de la commande ANNULER pour retourner   |"
                "\n  |             | à la 'Marque' précédente définie au début de VP-RADPURGE            |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Vous n'êtes pas d'accord avec l'état actuel du dessin. Le programme |"
                "\n  |     Non     | va retourner à la 'Marque' (commande ANNULER) précédente pour       |"
                "\n  |             | redéfinir le dessin à son état tel qu'au début de VP-RADPURGE       |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              nil
            )
          )
        )
        (progn
          (command-s "_UNDO" "_Back")
          (setq layer-list nil)
          (princ
            (LgT
              "\nThe command VP-RADPURGE has been undo..."
              "\nLa commande VP-RADPURGE a été annulée..."
              nil
            )
          )
        )
        (progn
          (setvar "NOMUTT" 1)
          (vlax-for layout (vla-get-layouts doc)
            (if (/= "Model" (vla-get-Name layout))
              (vla-delete layout)
            )
          )
          (command "_-PURGE" "_All" "*" "_No")
          (setvar "NOMUTT" NOMUTT)
          (princ
            (strcat
              (LgT
                "See below, the list of "
                "Ci-dessous la liste des "
                nil
              )
              (itoa (length (apply 'append layer-list)))
              (LgT
                " layers that have been purged from the drawing :"
                " calques ayant été purgé de ce dessin :"
                nil
              )
              "\n  - "
              (lst2str (apply 'append layer-list) "\n  - ")
            )
          )
        )
      )
    )
    (*error* "")
  )
  (setvar "CMDECHO" CMDECHO)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtGeodt] GEOMETRIC DATA ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:ALTPOINT" "19/09/2022" "Luna" "3.1.0" "\"UtGeodt\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] ALTPOINT []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 19/09/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.1.0                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

(defun c:ALTPOINT
  ( /
    *error* AlP-Settings-DCL AlP-get-MATALT AlP-princ-param
    DIMZIN LUPREC param break mode bloc-list layer-list jsel i name n mod-sel pt att-list ALT MAT lst filename file
  )
  (defun *error* (msg)
    (if file (close file))
    (setvar "DIMZIN" DIMZIN)
    (setvar "LUPREC" LUPREC)
    (sssetfirst)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun AlP-Settings-DCL (/ *error* AlP-CheckAttributes AlP-Check% AlP-Init AlP-Accept filename file DCL_ID bl al)
    (defun *error* (msg)
      (if file (close file))
      (if DCL_ID (unload_dialog DCL_ID))
      (if filename (vl-file-delete filename))
      (princ msg)
    )
    (defun AlP-CheckAttributes (str)
      (cond
        ( (vl-string-search str " ")
          (alert
            (LgT
              "The attribute(s) can't contained space (\" \")..."
              "Les attribut(s) ne peuvent pas contenir d'espace (\" \")..."
              nil
            )
          )
        )
        ( (/= str (lst2str (vl-remove nil (mapcar '(lambda (x) (if (member x (mapcar 'car al)) x)) (str2lst str ","))) ","))
          (alert
            (LgT
              "One or more attribute(s) doesn't correspond to the list of available attributes (use \",\" to separate each attribute name)..."
              "Un ou plusieurs attribut(s) ne corresponde(nt) pas à la liste des attribut(s) disponible(s) (utiliser \",\" pour séparer chaque nom d'attribut)..."
              nil
            )
          )
        )
      )
    )
    (defun AlP-Check% (str)
      (cond
        ( (null (setq str (distof str)))
          (alert
            (LgT
              "Only numbers are allowed..."
              "Uniquement les nombres sont autorisés..."
              nil
            )
          )
        )
        ( (minusp str)
          (alert
            (LgT
              "Only positive numbers are allowed..."
              "Uniquement les nombres positifs sont autorisés..."
              nil
            )
          )
        )
        ( (= "1" (get_tile "NBV")) str)
        ( (< 50 str)
          (alert
            (LgT
              "The value must be between 0 and 50..."
              "La valeur doit être comprise entre 0 et 50..."
              nil
            )
          )
        )
        (str)
      )
    )
    (defun AlP-Init (tile value flag / f)
      (setq f (null (cdr (assoc "LST" param))))
      (and
        (if flag (not f) f)
        (mode_tile tile value)
      )
    )
    (defun AlP-Accept (/ f)
      (defun f (str val)
        (if (= "1" (get_tile str)) val)
      )
      (list
        (cons "ALT" (get_tile "ALT"))
        (cons "MAT" (f "Toggle_MAT" (get_tile "MAT")))
        (cons "SEL" (f "SEL" T))
        (cons "CHK" (f "CHK" T))
        (cons "NBV" (f "NBV" T))
        (cons "%" (if (f "NBV" T) (atoi (get_tile "%")) (/ (atof (get_tile "%")) 100.0)))
        (assoc "LST" param)
      )
    )
    (setq
      filename (vl-filename-mktemp "ALTPOINT_SettingsBox.dcl")
      file (open filename "W")
    )
    (mapcar
      '(lambda (l) (write-line l file))
      (list
        "ALTPOINT_SettingsBox:dialog {"
        (strcat "  label = \"" (LgT "ALTPOINT : Command settings" "ALTPOINT : Paramètres de commande" nil) "\" ;")
        "  :row {"
        "    :column {"
        "      :boxed_column {"
        (strcat "        label = \"" (LgT "Attributes settings" "Paramètres des attributs" nil) "\" ;")
        "        :text {"
        "          label = \"Altitude: \" ;"
        "          alignment = \"right\" ;"
        "        }"
        "        :edit_box {"
        "          key = \"ALT\" ;"
        "          alignment = \"left\" ;"
        "        }"
        "        spacer ;"
        "        spacer ;"
        "        :toggle {"
        "          label = \"Matricule: \" ;"
        "          key = \"Toggle_MAT\" ;"
        "          alignment = \"right\" ;"
        "          value = \"1\" ;"
        "        }"
        "        :edit_box {"
        "          key = \"MAT\" ;"
        "          alignment = \"left\" ;"
        "        }"
        "      }"
        "      :boxed_column {"
        (strcat "        label = \"" (LgT "Selection settings" "Paramètres de sélection" nil) "\" ;")
        "        :toggle {"
        (strcat "          label = \"" (LgT "Select all block references from Model" "Sélectionner toutes les références de bloc en Objet" nil) "\" ;")
        "          key = \"SEL\" ;"
        "        }"
        "        :toggle {"
        (strcat "          label = \"" (LgT "Only controls values, without modifying properties" "Contrôle uniquement les valeurs, sans modifier les propriétés" nil) "\" ;")
        "          key = \"CHK\" ;"
        "        }"
        "        :radio_row {"
        "          :radio_button {"
        (strcat "            label = \"" (LgT "Percentage" "Pourcentage" nil) "\" ;")
        "            key = \"PCT\" ;"
        "          }"
        "          :radio_button {"
        (strcat "            label = \"" (LgT "Number of value(s)" "Nombre de valeur(s)" nil) "\" ;")
        "            key = \"NBV\" ;"
        "          }"
        "        }"
        "        :text{"
        "          key = \"TXT\" ;"
        "        }"
        "        :edit_box {"
        "          key = \"%\" ;"
        "        }"
        "      }"
        "    }"
        "    :boxed_column {"
        "      width = 42 ;"
        (strcat "      label = \"" (LgT "Block(s) informations" "Informations sur le(s) bloc(s)" nil) "\" ;")
        "      :list_box {"
        (strcat "        label = \"" (LgT "Block(s) list :" "Liste des bloc(s) :" nil) "\" ;")
        "        key = \"LST\" ;"
        "        height = 13 ;"
        "      }"
        "      :list_box {"
        (strcat "        label = \"" (LgT "Attribute(s) list :" "Liste des attribut(s) :" nil) "\" ;")
        "        key = \"ATT\" ;"
        "        height = 13 ;"
        "      }"
        "    }"
        "  }"
        "  spacer ;"
        "  ok_cancel ;"
        "}"
      )
    )
    (setq file (close file))
    (setq DCL_ID (load_dialog filename))
    (if (not (new_dialog "ALTPOINT_SettingsBox" DCL_ID))
      (exit)
    )
    (set_tile "ALT" (cond ((cdr (assoc "ALT" param))) ("")))
    (AlP-Init "ALT" 1 nil)
    (set_tile "Toggle_MAT" (if (cdr (assoc "MAT" param)) "1" "0"))
    (set_tile "MAT" (cond ((cdr (assoc "MAT" param))) ("")))
    (AlP-Init "MAT" 1 nil)
    (set_tile "SEL" (if (cdr (assoc "SEL" param)) "1" "0"))
    (set_tile "CHK" (if (cdr (assoc "CHK" param)) "1" "0"))
    (set_tile "NBV" (if (cdr (assoc "NBV" param)) "1" "0"))
    (set_tile "PCT" (if (cdr (assoc "NBV" param)) "0" "1"))
    (set_tile
      "TXT"
      (if (cdr (assoc "NBV" param))
        (LgT "Number of lower and higher top values (u)" "Nombre de valeurs inférieures et supérieures (u)" nil)
        (LgT "Lower and higher top percentage value (%)" "Valeur du pourcentage des valeurs inférieures et supérieures (%)" nil)
      )
    )
    (set_tile "%" (if (cdr (assoc "NBV" param)) (itoa (fix (cdr (assoc "%" param)))) (rtos (* (cdr (assoc "%" param)) 100) 2 1)))
    (setq bl
      (mapcar
        '(lambda (b / o a)
          (setq o (tblobjname "BLOCK" b))
          (setq o (entnext o))
          (while o
            (if (= "ATTDEF" (cdr (assoc 0 (entget o))))
              (setq a (cons (cdr (assoc 2 (entget o))) a))
            )
            (setq o (entnext o))
          )
          (cons b (reverse a))
         )
        (cdr (assoc "LST" param))
      )
    )
    (start_list "LST")
    (mapcar
      '(lambda (x)
        (add_list (strcat (car x) " [" (itoa (length (cdr x))) (LgT " attribute(s)" " attribut(s)" nil) "]"))
        (mapcar '(lambda (a) (add_list (strcat "  → " a))) (cdr x))
        (add_list "")
       )
      bl
    )
    (end_list)
    (setq al
      (mapcar
        '(lambda (a) (cons a (vl-remove nil (mapcar '(lambda (b) (if (member a b) (car b))) bl))))
        (remove-duplicates (apply 'append (mapcar 'cdr bl)))
      )
    )
    (start_list "ATT")
    (mapcar
      '(lambda (x)
        (add_list (strcat (car x) " [" (itoa (length (cdr x))) " / " (itoa (length bl)) (LgT " block(s)" " bloc(s)" nil) "]"))
        (mapcar '(lambda (b) (add_list (strcat "  → " b))) (cdr x))
        (add_list "")
       )
      al
    )
    (end_list)
    (action_tile "ALT" "(AlP-CheckAttributes $value)")
    (action_tile "MAT" "(AlP-CheckAttributes $value)")
    (action_tile "Toggle_MAT" "(AlP-Init \"MAT\" (- 1 (atoi $value)) T)")
    (action_tile "PCT" "(set_tile \"TXT\" (LgT \"Lower and higher top percentage value (%)\" \"Valeur du pourcentage des valeurs inférieures et supérieures (%)\" nil))")
    (action_tile "NBV" "(set_tile \"TXT\" (LgT \"Number of lower and higher top values (u)\" \"Nombre de valeurs inférieures et supérieures (u)\" nil))")
    (action_tile "%" "(AlP-Check% $value)")
    (action_tile "accept" "(setq param (AlP-Accept)) (done_dialog)")
    (action_tile "cancel" "(done_dialog)")
    (start_dialog)
    (unload_dialog DCL_ID)
    (setenv
      "ALTPOINT_Settings"
      (lst2str (list (apply '+ (mapcar '(lambda (b s) (if (cdr (assoc s param)) b 0)) '(1 2 4) '("SEL" "CHK" "NBV"))) (cdr (assoc "%" param))) ",")
    )
    (vl-file-delete filename)
    param
  )
  (defun AlP-get-MATALT (val lst)
    (cond
      ( (null val) nil)
      ( (assoc (car val) lst))
      ( (AlP-get-MATALT (cdr val) lst))
    )
  )
  (defun AlP-princ-param ()
    (princ
      (strcat
        (LgT
          "\nCurrent settings: Select all = "
          "\nParamètres courant: Sélectionner tout = "
          nil
        )
        (if (cdr (assoc "SEL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil))
        (LgT
          "  Block(s) modification = "
          "  Modification des bloc(s) = "
          nil
        )
        (if (cdr (assoc "CHK" param)) (LgT "No" "Non" nil) (LgT "Yes" "Oui" nil))
        (if (cdr (assoc "NBV" param)) (LgT "  Number of values = " "  Nombre de valeurs = " nil) (LgT "  Percentage = " "  Pourcentage = " nil))
        (if (cdr (assoc "NBV" param))
          (strcat (itoa (fix (cdr (assoc "%" param)))) "u")
          (strcat (rtos (* 100.0 (cdr (assoc "%" param))) 2 1) "%")
        )
        "\nAltitude = " (cond ((cdr (assoc "ALT" param))) (""))
        "  Matricule = " (cond ((cdr (assoc "MAT" param))) ((LgT "<none>" "<aucun>" nil)))
        (LgT "  Layer(s) = " "  Calque(s) = " nil) (cond ((lst2str layer-list ", ")) ("*"))
        (LgT
          "\nBlock(s) list = "
          "\nListe des bloc(s) = "
          nil
        )
        (cond ((lst2str (cdr (assoc "LST" param)) ", ")) (""))
      )
    )
    nil
  )

  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("DIMZIN" DIMZIN 0) ("LUPREC" LUPREC 2)))
  (cond
    ( (getenv "ALTPOINT_Settings") (setq param (str2lst (getenv "ALTPOINT_Settings") ",")))
    ( (setq param (str2lst (setenv "ALTPOINT_Settings" (lst2str (list (apply '+ '(1 2 4)) 24) ",")) ",")))
  )
  (setq param
    (list
      (cons "ALT" "ALT")
      (cons "MAT" "MAT")
      (cons "SEL" (bit 1 (atoi (car param))))
      (cons "CHK" (bit 2 (atoi (car param))))
      (cons "NBV" (bit 4 (atoi (car param))))
      (cons "%" (atof (cadr param)))
      (cons "LST" '())
    )
  )
  (while
    (and
      (null break)
      (null (AlP-princ-param))
      (setq mode
        (getkdh
          (quote (entsel msg))
          (LgT
            "\nSelect a block or"
            "\nSélectionner un bloc ou"
            nil
          )
          (list
            (LgT
              "TCpoint List Settings eXit _TCpoint List Settings eXit"
              "TCpoint Liste Paramètres Quitter _TCpoint List Settings eXit"
              nil
            )
          )
          " : "
          "eXit"
          (LgT
            (strcat
              "\nALTPOINT : Selection mode for blocks"
              "\nDefault value:     eXit"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Select    | Select manually a block with attributes corresponding to the        |"
              "\n  |             | defined settings. You can select one or more blocks                 |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   TCpoint   | Selects only blocks named \"TCPOINT\", usually used for topographic   |"
              "\n  |             | datas, with 2 attributes named \"ALT\" and \"MAT\"                      |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    List     | Opens a dialog box with the list of all block's name and let you    |"
              "\n  |             | select one or more blocks within the list                           |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Opens a dialog box to set the settings for the command as the       |"
              "\n  |  Settings   | name of the attribute for the altitude and the matricule, if you    |"
              "\n  |             | want to select all occurences or select them manually, and if you   |"
              "\n  |             | want to check the topographic points or correct them                |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            (strcat
              "\nALTPOINT : Mode de sélection des blocs"
              "\nValeur par défaut: Quitter"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Vous sélectionnez manuellement un bloc possédant des attributs      |"
              "\n  |  Sélection  | correspondant aux paramètres définis. Vous pouvez sélectionner un   |"
              "\n  |             | ou plusieurs blocs                                                  |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Sélectionne uniquement les blocs nommés \"TCPOINT\", habituellement   |"
              "\n  |   TCpoint   | utilisés pour les données topographiques, possédant 2 attributs     |"
              "\n  |             | nommés \"ALT\" et \"MAT\" (remplace la liste de bloc existante)         |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    Liste    | Ouvre une boîte de dialogue avec la liste de l'ensemble des noms de |"
              "\n  |             | blocs et vous permet de sélectionner un ou plusieurs blocs          |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Ouvre une boîte de dialogue pour définir les paramètres de la       |"
              "\n  |             | commande tels que le nom des attributs pour l'altitude et le        |"
              "\n  | Paramètres  | matricule, si vous souhaitez sélectionner toutes les références de  |"
              "\n  |             | blocs ou bien les sélectionner manuellement, et si vous désirez     |"
              "\n  |             | simplement vérifier les points topographiques ou bien les corriger  |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            nil
          )
        )
      )
    )
    (cond
      ( (= "TCpoint" mode)
        (setq
          break T
          bloc-list '("TCPOINT*")
          param (subst (cons "LST" bloc-list) (assoc "LST" param) param)
        )
      )
      ( (= "List" mode)
        (setq
          bloc-list
            (ListBox
              (LgT
                "ALTPOINT: Block(s) selection"
                "ALTPOINT: Sélection des bloc(s)"
                nil
              )
              (LgT
                "Please, select one or more block(s) :"
                "Veuillez sélectionner un ou plusieurs bloc(s) :"
                nil
              )
              (vl-sort (vl-remove-if '(lambda (x) (wcmatch x "`**")) (mapcar 'car (vla-collection->list nil 'blocks 1))) '<)
              nil
              2
              nil
            )
          param (subst (cons "LST" bloc-list) (assoc "LST" param) param)
        )
      )
      ( (and
          (listp mode)
          (setq mode (vlax-ename->vla-object (car mode)))
          (= "AcDbBlockReference" (vlax-get mode 'ObjectName))
          (vlax-property-available-p mode 'EffectiveName)
          (= -1 (vlax-get mode 'HasAttributes))
        )
        (if (not (member (vlax-get mode 'EffectiveName) bloc-list))
          (setq
            bloc-list (cons (vlax-get mode 'EffectiveName) bloc-list)
            param (subst (cons "LST" bloc-list) (assoc "LST" param) param)
          )
        )
        (if (not (member (vlax-get mode 'Layer) layer-list))
          (setq layer-list (cons (vlax-get mode 'Layer) layer-list))
        )
      )
      ( (= "Settings" mode)
        (setq param (AlP-Settings-DCL))
      )
      ( (= "eXit" mode) (setq break T))
      ( T (princ (LgT "\nInvalid selection..." "\nSélection invalide..." nil)))
    )
  )
  (if
    (and
      param
      (setq bloc-list (cdr (assoc "LST" param)))
      (setq jsel
        (apply
          'ssget
          (vl-remove
            nil
            (list
              (if (cdr (assoc "SEL" param)) "_X")
              (list
                '(0 . "INSERT")
                '(66 . 1)
                '(410 . "Model")
                (cons 2 (lst2str bloc-list ","))
                (if layer-list (cons 8 (lst2str layer-list ",")) '(8 . "*"))
              )
            )
          )
        )
      )
    )
    (progn
      (repeat (setq n 0 mod-sel (ssadd) i (sslength jsel))
        (and
          (setq name (ssname jsel (setq i (1- i))))
          (setq pt (cdr (assoc 10 (entget name))))
          (setq att-list (get-att-list name))
          (setq
            MAT (if (cdr (assoc "MAT" param)) (AlP-get-MATALT (str2lst (cdr (assoc "MAT" param)) ",") att-list))
            ALT (cdr (AlP-get-MATALT (str2lst (cdr (assoc "ALT" param)) ",") att-list))
          )
          (setq lst
            (cons
              (cons
                (cond ((cdr MAT)) (""))
                (list
                  (atof ALT)
                  pt
                  (vlax-get (vlax-ename->vla-object name) 'EffectiveName)
                  (cond ((car MAT)) (""))
                  (cdr (assoc 5 (entget name)))
                )
              )
              lst
            )
          )
          (if
            (or
              (null (distof ALT))
              (not (equal (last pt) (distof ALT) 1E-3))
              (zerop (distof ALT))
            )
            (ssadd name mod-sel)
            T
          )
          (null (cdr (assoc "CHK" param)))
          (not (null (distof ALT)))
          (entmod
            (subst
              (cons 10 (list (car pt) (cadr pt) (distof ALT)))
              (assoc 10 (entget name))
              (entget name)
            )
          )
          (setq n (1+ n))
        )
      )
      (setq
        lst (vl-sort lst '(lambda (a1 a2) (< (cadr a1) (cadr a2))))
        i
          (if (cdr (assoc "NBV" param))
            (if (< (* 2 (cdr (assoc "%" param))) (length lst))
              (fix (cdr (assoc "%" param)))
              (/ (length lst) 2)
            )
            (atoi (rtos (* (length lst) (cdr (assoc "%" param))) 2 0))
          )
      )
      (sssetfirst)
      (if (null (cdr (assoc "CHK" param)))
        (princ
          (strcat
            (LgT
              "\nA total of "
              "\nUn total de "
              nil
            )
            (itoa n)
            " / "
            (itoa (sslength jsel))
            (LgT
              (strcat
                " block(s) have been redefined successfully."
                "\nHere's an overview of the results :"
                "\n  - Maximum altitude = "
              )
              (strcat
                " bloc(s) ont été redéfinis avec succès."
                "\nVoici un aperçu des résultats :"
                "\n  - Altitude maximale = "
              )
              nil
            )
            (rtos (cadr (last lst)) 2 2)
            "m"
            (if (cdr (assoc "MAT" param))
              (strcat
                " (MAT: "
                (lst2str (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cadr x) (cadr (last lst)))) lst)) ", ")
                ")"
              )
              ""
            )
            (LgT
              "\n  - Minimum altitude = "
              "\n  - Altitude minimale = "
              nil
            )
            (rtos (cadar lst) 2 2)
            "m"
            (if (cdr (assoc "MAT" param))
              (strcat
                " (MAT: "
                (lst2str (mapcar 'car (vl-remove-if-not '(lambda (x) (= (cadr x) (cadar lst))) lst)) ", ")
                ")"
              )
              ""
            )
            (LgT
              "\n  - Average altitude = "
              "\n  - Altitude moyenne  = "
              nil
            )
            (rtos (/ (apply '+ (mapcar 'cadr lst)) (float (length lst))) 2 2)
            "m"
            (if (not (zerop (cdr (assoc "%" param))))
              (strcat
                (if (cdr (assoc "NBV" param))
                  (LgT
                    (strcat "\nList of " (itoa i) " values in the lower top ")
                    (strcat "\nListe des " (itoa i) " valeurs inférieures")
                    nil
                  )
                  (strcat
                    (LgT
                      "\nList of values in the lower top "
                      "\nListe des valeurs inférieures à "
                      nil
                    )
                    (rtos (* 100 (cdr (assoc "%" param))) 2 1)
                    "% (" (itoa i) "u)"
                  )
                )
                " :"
                (apply
                  'strcat
                  (mapcar
                    '(lambda (x / c)
                      (setq c (vl-position x (sublist lst 1 i)))
                      (strcat
                        (if (= 0 (rem c 3)) "\n|  " ", ")
                        (rtos (cadr x) 2 2)
                        "m"
                        (if (cdr (assoc "MAT" param))
                          (strcat
                            " (" (cadddr (cdr x)) ": "
                            (car x)
                            ")"
                          )
                          ""
                        )
                      )
                    )
                    (sublist lst 1 i)
                  )
                )
                "\n|"
                (if (cdr (assoc "NBV" param))
                  (LgT
                    (strcat "\nList of " (itoa i) " values in the higher top ")
                    (strcat "\nListe des " (itoa i) " valeurs supérieures")
                    nil
                  )
                  (strcat
                    (LgT
                      "\nList of values in the higher top "
                      "\nListe des valeurs supérieures à "
                      nil
                    )
                    (rtos (* 100 (cdr (assoc "%" param))) 2 1)
                    "% (" (itoa i) "u)"
                  )
                )
                " :"
                (apply
                  'strcat
                  (mapcar
                    '(lambda (x / c)
                      (setq c (vl-position x (sublist lst (- (length lst) i) nil)))
                      (strcat
                        (if (= 0 (rem c 3)) "\n|  " ", ")
                        (rtos (cadr x) 2 2)
                        "m"
                        (if (cdr (assoc "MAT" param))
                          (strcat
                            " (" (cadddr (cdr x)) ": "
                            (car x)
                            ")"
                          )
                          ""
                        )
                      )
                    )
                    (sublist lst (- (length lst) i) nil)
                  )
                )
              )
            )
          )
        )
        (and
          (setq filename
            (getfiled
              (LgT
                "Selection of .csv export file"
                "Sélection du fichier .csv d'export"
                nil
              )
              (strcat (getvar "DWGPREFIX") (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "_ALTPOINT Export")
              "csv"
              1
            )
          )
          (cond
            ( (setq file (Excel:IsOpenedWorkBook filename))
              (vla-Close (cdr file) :vlax-false)
              (princ
                (LgT
                  (strcat "\nThe file " (car file) " was opened. It will be closed and overwritten...")
                  (strcat "\nLe fichier " (car file) " était ouvert. Il sera fermé et réécrit...")
                  nil
                )
              )
            )
            ( (not file))
          )
          (setq file (open filename "W"))
          (write-line "Handle;Block;Attribute;Matricule;X;Y;Z;Altitude;Delta" file)
          (mapcar
            '(lambda (x / h b e m p x y z a)
              (setq
                m (car x)
                h (cdr x)
                a (car h)
                p (cadr h)
                b (caddr h)
                e (cadddr h)
                h (last h)
                x (car p)
                y (cadr p)
                z (last p)
              )
              (write-line
                (strcat
                  "'"
                  h
                  ";"
                  b
                  ";"
                  e
                  ";"
                  m
                  ";"
                  (vl-string-translate "." "," (rtos x 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos y 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos z 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos a 2 2))
                  ";"
                  (vl-string-translate "." "," (rtos (- a z) 2 2))
                )
                file
              )
             )
            lst
          )
          (not (setq file (close file)))
          (not (alert (LgT "The file has been created successfully !" "Le fichier a été créé avec succès !" nil)))
          (princ
            (strcat
              (LgT
                "\nFile name: "
                "\nNom du fichier: "
                nil
              )
              filename
            )
          )
          (=
            "Yes"
            (getkdh
              (quote (getkword msg))
              (LgT
                "\nWould you like to open the export file ?"
                "\nSouhaitez-vous ouvrir le fichier d'export ?"
                nil
              )
              (list (LgT "Yes No _Yes No" "Oui Non _Yes No" nil))
              " "
              "Yes"
              nil
            )
          )
          (startapp "C:\\Program Files\\Microsoft Office\\root\\Office16\\Excel.exe" (strcat "\"" filename "\""))
        )
      )
      (sssetfirst nil mod-sel)
    )
  )
  (SetVarList '(("DIMZIN" nil DIMZIN) ("LUPREC" nil LUPREC)))
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:LONGCUMUL" "04/07/2022" "Luna" "3.0.2" "\"UtGeodt\"")                       ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] LONGCUMUL []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.2                                                                                                                 ;
;--- Class                  > "UtGeodt"                                                                                                             ;

(defun c:LONGCUMUL
  ( /
    *error* LgC-bit LgC-princ-param LgC-Results-DCL
    param type-list bit bitlist DIMZIN LUPREC u break mode msg lst typ lay jsel name tmp CSV coeff add i
  )
  (defun *error* (msg)
    (setvar "DIMZIN" DIMZIN)
    (setvar "LUPREC" LUPREC)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun LgC-bit (b a)
    (= b (logand a b))
  )
  (defun LgC-princ-param ()
    (princ
      (strcat
        (LgT
          "\nCurrent settings: Select all = "
          "\nParamètres courant: Sélectionner tout = "
          nil
        )
        (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil))
        (LgT
          "\nLayer(s) list = "
          "\nListe des calque(s) = "
          nil
        )
        (cond ((lst2str (cdr (assoc "LAYERS" param)) ", ")) ("*"))
        (LgT
          "\nObject type(s) list = "
          "\nListe des type(s) d'objet = "
          nil
        )
        (cond ((lst2str (mapcar 'car (cdr (assoc "OBJECT" param))) ", ")) ((lst2str (mapcar 'car type-list) ", ")))
      )
    )
    nil
  )
  (defun LgC-Results-DCL (/ *error* LgC-EditCheck LgC-Export LgC-Settings-set LgC-Results filename file DCL_ID)
    (defun *error* (msg)
      (if file (close file))
      (if filename (vl-file-delete filename))
      (princ msg)
    )
    (defun LgC-EditCheck (str flag)
      (cond
        ( (null (setq str (distof str)))
          (alert
            (LgT
              "Only numbers are allowed..."
              "Uniquement les nombres sont autorisés..."
              nil
            )
          )
        )
        ( (and flag (minusp str))
          (alert
            (LgT
              "Only positive numbers are allowed..."
              "Uniquement les nombres positifs sont autorisés..."
              nil
            )
          )
        )
        ( (and flag (zerop str))
          (alert
            (LgT
              "The value can't be set to zero..."
              "La valeur ne peut pas être définie à zéro..."
              nil
            )
          )
        )
        (str)
      )
    )
    (defun LgC-Export (/ *error* filename file cc ca c i)
      (defun *error* (msg)
        (if file (close file))
        (princ msg)
      )
      (if
        (and
          (setq
            filename
              (getfiled
                (LgT
                  "Selection of .csv export file"
                  "Sélection du fichier .csv d'export"
                  nil
                )
                (strcat (getvar "DWGPREFIX") (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "_LONGCUMUL Export")
                "csv"
                1
              )
          )
          (setq file (open filename "W"))
          (setq
            cc "$G$1"
            ca "$I$1"
            c "D"
            i 2
          )
          (write-line
            (strcat
              ";;;;;Coeff :"
              ";"
              (vl-string-translate "." "," (rtos coeff 2 2))
              ";"
              (LgT "Add :" "Supp. :" nil)
              ";"
              (vl-string-translate "." "," (rtos add 2 2))
            )
            file
          )
          (write-line
            (LgT
              "Handle;Layer;Object Type;Length (AutoCAD);Length (Corrected)"
              "Handle;Calque;Type d'Objet;Longueur (AutoCAD);Longueur (Corrigée)"
              nil
            )
            file
          )
          (mapcar
            '(lambda (x / lay l)
              (setq
                lay (car x)
                l (cdr x)
              )
              (mapcar
                '(lambda (x / typ l)
                  (setq
                    typ (car x)
                    l (cdr x)
                  )
                  (mapcar
                    '(lambda (x / hdl lng)
                      (setq
                        lng (car x)
                        hdl (cadr x)
                      )
                      (write-line
                        (strcat
                          hdl
                          ";"
                          lay
                          ";"
                          (nth (vl-position typ (mapcar 'cdr type-list)) (mapcar 'car type-list))
                          ";"
                          (vl-string-translate "." "," (rtos lng 2 2))
                          ";"
                          "=" cc "*(" ca "+" c (itoa (setq i (1+ i))) ")"
                        )
                        file
                      )
                     )
                    (vl-sort l '(lambda (e1 e2) (< (car e1) (car e2))))
                  )
                 )
                l
              )
             )
            lst
          )
          (not (setq file (close file)))
          (not
            (alert
              (strcat
                (LgT
                  "The file has been created successfully !"
                  "Le fichier a été créé avec succès !"
                  nil
                )
                "\n  > "
                (substr filename (+ 2 (vl-string-position (ascii "\\") filename 0 T)))
              )
            )
          )
        )
        filename
      )
    )
    (defun LgC-Settings-set (tile b)
      (if (= b (logand bit b))
        (set_tile tile "1")
        (set_tile tile "0")
      )
    )
    (defun LgC-Results (bit / LgC-Dis LgC-Det_Lengths LgC-Det_Objects LgC-Det_Layers i)
      (defun LgC-Dis (str f)
        (setq i (+ i 3))
        (if f
          (add_list
            (strcat
              (space (expt i 2))
              "[]"
              (substr (mAtom (if (= i 3) "• " "- ") (- 42 (* 2 (+ i 2))) 'strcat) 2)
              "[]"
              (space (expt i 2))
            )
          )
        )
        (add_list (strcat (space (- i 3)) str))
        (if (LgC-bit (cdr (assoc "Dis_Length" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Total length      = "
                "Longueur totale = "
                nil
              )
              (ThsdSpace (rtos (apply '+ lng) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Count" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Number of objects = "
                "Nombre d'objets = "
                nil
              )
              (ThsdSpace (itoa (length lng)) (LgT "," " " nil))
              "u"
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Max" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Maximum length    = "
                "Longueur max.   = "
                nil
              )
              (ThsdSpace (rtos (apply 'max lng) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Min" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Minimum length    = "
                "Longueur min.   = "
                nil
              )
              (ThsdSpace (rtos (apply 'min lng) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Avg" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Average length    = "
                "Longueur moy.   = "
                nil
              )
              (ThsdSpace (rtos (/ (apply '+ lng) (float (length lng))) 2 2) (LgT "," " " nil))
              (units u)
            )
          )
        )
        (if (LgC-bit (cdr (assoc "Dis_Dev" bitlist)) bit)
          (add_list
            (strcat
              (space i)
              (LgT
                "Standard deviation= "
                "Ecart type      = "
                nil
              )
              (ThsdSpace
                (rtos
                  (sqrt (/ (apply '+ (mapcar '(lambda (x) (expt (- x (/ (apply '+ lng) (float (length lng)))) 2)) lng)) (float (length lng))))
                  2
                  2
                )
                (LgT "," " " nil)
              )
              (units u)
            )
          )
        )
      )
      (defun LgC-Det_Lengths (f n tmp)
        (if f
          (mapcar
            '(lambda (x / hdl lng)
              (setq
                i n
                lng (* coeff (+ add (car x)))
                hdl (cadr x)
              )
              (add_list (strcat (space i) " • \"" hdl "\" →  " (ThsdSpace (rtos lng 2 2) (LgT "," " " nil)) (units u)))
            )
            tmp
          )
        )
      )
      (defun LgC-Det_Objects (f n tmp)
        (if f
          (mapcar
            '(lambda (x / typ tmp lng)
              (setq
                i n
                typ (car x)
                tmp (cdr x)
                lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) tmp)
                typ (nth (vl-position typ (mapcar 'cdr type-list)) (mapcar 'car type-list))
              )
              (LgC-Dis (strcat (space i) "\"" typ "\" :") T)
              (LgC-Det_Lengths (LgC-bit (cdr (assoc "Det_Lengths" bitlist)) bit) i (vl-sort tmp '(lambda (e1 e2) (< (car e1) (car e2)))))
              (add_list "")
            )
            tmp
          )
        )
      )
      (defun LgC-Det_Layers (f n tmp)
        (if f
          (mapcar
            '(lambda (x / lay tmp lng)
              (setq
                i n
                lay (car x)
                tmp (cdr x)
                lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr tmp)))
              )
              (LgC-Dis (strcat (space i) (LgT "Layer \"" "Calque \"" nil) lay "\" :") T)
              (or
                (LgC-Det_Objects (LgC-bit (cdr (assoc "Det_Objects" bitlist)) bit) i (vl-sort tmp '(lambda (e1 e2) (< (car e1) (car e2)))))
                (LgC-Det_Lengths
                  (LgC-bit (cdr (assoc "Det_Lengths" bitlist)) bit)
                  i
                  (vl-sort (apply 'append (mapcar 'cdr tmp)) '(lambda (e1 e2) (< (car e1) (car e2))))
                )
              )
            )
            tmp
          )
        )
      )
      (start_list "Results")
      (cond
        ( (LgC-Det_Layers (LgC-bit (cdr (assoc "Det_Layers" bitlist)) bit) 0 lst))
        ( (LgC-Det_Objects
            (LgC-bit (cdr (assoc "Det_Objects" bitlist)) bit)
            0
            (setq
              tmp (apply 'append (mapcar 'cdr lst))
              tmp
                (vl-remove-if-not
                  '(lambda (b) (cdr b))
                  (mapcar
                    '(lambda (x) (cons (cdr x) (apply 'append (mapcar 'cdr (vl-remove-if-not '(lambda (a) (= (cdr x) (car a))) tmp)))))
                    type-list
                  )
                )
            )
          )
        )
        ( (LgC-Det_Lengths
            (LgC-bit (cdr (assoc "Det_Lengths" bitlist)) bit)
            0
            (vl-sort (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))) '(lambda (e1 e2) (< (car e1) (car e2))))
          )
        )
        ( T
          (setq
            i 0
            lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))))
          )
          (LgC-Dis "" nil)
        )
      )
      (end_list)
    )
    (setq
      filename (vl-filename-mktemp "LONGCUMUL_ResultsBox.dcl")
      file (open filename "W")
      coeff 1.0
      add 0.0
    )
    (mapcar
      '(lambda (l) (write-line l file))
      (list
        " LONGCUMUL_ResultsBox:dialog {"
        (strcat "   label = \"" (LgT "LONGCUMUL : Results" "LONGCUMUL : Résultats" nil) "\" ;")
        "   :row {"
        "     :boxed_column {"
        (strcat "       label = \"" (LgT "Settings" "Paramètres" nil) "\" ;")
        "       :toggle {"
        (strcat "         label = \"" (LgT "Details by LAYERS" "Détailler par CALQUES" nil) "\" ;")
        "         key = \"Det_Layers\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Details by OBJECT TYPES" "Détailler par TYPES D'OBJET" nil) "\" ;")
        "         key = \"Det_Objects\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Details by LENGTHS" "Détailler par LONGUEURS" nil) "\" ;")
        "         key = \"Det_Lengths\" ;"
        "       }"
        "       spacer ;"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays NUMBER of objects" "Afficher le NOMBRE d'objets" nil) "\" ;")
        "         key = \"Dis_Count\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays CUMULATED length" "Afficher la longueur CUMULÉE" nil) "\" ;")
        "         key = \"Dis_Length\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays MAXIMUM length" "Afficher la longueur MAXIMALE" nil) "\" ;")
        "         key = \"Dis_Max\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays MINIMUM length" "Afficher la longueur MINIMALE" nil) "\" ;")
        "         key = \"Dis_Min\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays AVERAGE length" "Afficher la longueur MOYENNE" nil) "\" ;")
        "         key = \"Dis_Avg\" ;"
        "       }"
        "       :toggle {"
        (strcat "         label = \"" (LgT "Displays STANDARD DEVIATION" "Afficher l'ÉCART TYPE" nil) "\" ;")
        "         key = \"Dis_Dev\" ;"
        "       }"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "       :edit_box {"
        "         label = \"Coefficient (x) =\" ;"
        "         key = \"Coeff\" ;"
        (strcat "         value = \"" (rtos coeff 2 2) "\" ;")
        "         alignment = \"left\" ;"
        "         width = 39 ;"
        "         edit_width = 16 ;"
        "         fixed_width = true ;"
        "       }"
        "       :edit_box {"
        (strcat "         label = \"" (LgT "Add length (+)  = " "Longueur ajoutée (+) =" nil) "\" ;")
        "         key = \"Add\" ;"
        (strcat "         value = \"" (rtos add 2 2) "\" ;")
        "         alignment = \"left\" ;"
        "         width = 39 ;"
        "         edit_width = 16 ;"
        "         fixed_width = true ;"
        "       }"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "       spacer ;"
        "     }"
        "     :boxed_column {"
        (strcat "       label = \"" (LgT "Results" "Résultats" nil) "\" ;")
        "       :list_box {"
        "         key = \"Results\" ;"
        "         multiple_select = true ;"
        "         height = 51 ;"
        "         width = 100 ;"
        "       }"
        "       :row {"
        "         spacer ;"
        "         :button {"
        "           label = \"Export .csv\" ;"
        "           key = \"Export\" ;"
        "           width = 12 ;"
        "           alignment = \"centered\" ;"
        "           is_default = true ;"
        "         }"
        "         spacer ;"
        "       }"
        "     }"
        "   }"
        "   spacer ;"
        "   ok_only ;"
        " }"
      )
    )
    (setq file (close file))
    (setq DCL_ID (load_dialog filename))
    (if (not (new_dialog "LONGCUMUL_ResultsBox" DCL_ID))
      (exit)
    )
    (foreach x bitlist
      (LgC-Settings-set (car x) (cdr x))
    )
    (action_tile "Det_Layers" "(setq bit (SwapBit bit (cdr (assoc \"Det_Layers\" bitlist)))) (LgC-Results bit)")
    (action_tile "Det_Objects" "(setq bit (SwapBit bit (cdr (assoc \"Det_Objects\" bitlist)))) (LgC-Results bit)")
    (action_tile "Det_Lengths" "(setq bit (SwapBit bit (cdr (assoc \"Det_Lengths\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Count" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Count\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Length" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Length\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Max" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Max\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Min" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Min\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Avg" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Avg\" bitlist)))) (LgC-Results bit)")
    (action_tile "Dis_Dev" "(setq bit (SwapBit bit (cdr (assoc \"Dis_Dev\" bitlist)))) (LgC-Results bit)")
    (LgC-Results bit)
    (action_tile "Coeff" "(set_tile \"Coeff\" (rtos (setq coeff (cond ((LgC-EditCheck $value T)) (coeff))) 2 2)) (LgC-Results bit)")
    (action_tile "Add" "(set_tile \"Add\" (rtos (setq add (cond ((LgC-EditCheck $value nil)) (add))) 2 2)) (LgC-Results bit)")
    (action_tile "Export" "(setq CSV (vl-remove nil (append (list (LgC-Export)) CSV)))")
    (action_tile "accept" "(setenv \"LONGCUMUL_Settings\" (itoa bit)) (done_dialog)")
    (start_dialog)
    (unload_dialog DCL_ID)
    (setq filename (not (vl-file-delete filename)))
    (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr (apply 'append (mapcar 'cdr lst)))))
  )
  
  (setq
    param
      (list
        (cons "LAYERS" '())
        (cons "OBJECT" '())
        (cons "SELALL" nil)
      )
    type-list
      (list
        (cons (LgT "Arc" "Arc" nil) "ARC")
        (cons (LgT "Circle" "Cercle" nil) "CIRCLE")
        (cons (LgT "Ellipse" "Ellipse" nil) "ELLIPSE")
        (cons (LgT "Line" "Ligne" nil) "LINE")
        (cons (LgT "Polyline" "Polyligne" nil) "LWPOLYLINE")
        (cons (LgT "3D Polyline" "Polyligne 3D" nil) "POLYLINE")
        (cons (LgT "Spline" "Spline" nil) "SPLINE")
      )
    bit (atoi (cond ((getenv "LONGCUMUL_Settings")) ((setenv "LONGCUMUL_Settings" "251"))))
    bitlist
      (list
        (cons "Det_Layers"  1)
        (cons "Det_Objects" 2)
        (cons "Det_Lengths" 4)
        (cons "Dis_Count"   8)
        (cons "Dis_Length"  16)
        (cons "Dis_Max"     32)
        (cons "Dis_Min"     64)
        (cons "Dis_Avg"     128)
        (cons "Dis_Dev"     256)
      )
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("DIMZIN" DIMZIN 0) ("LUPREC" LUPREC 2) ("INSUNITS" u nil)))
  (while
    (and
      (null break)
      (null (LgC-princ-param))
      (setq mode
        (getkdh
          (quote (entsel msg))
          (LgT
            "\nSelect an object or"
            "\nSélectionner un objet ou"
            nil
          )
          (list
            (LgT
              "Layers Objects All Undo eXit _Layers Objects All Undo eXit"
              "Calques Objets Tous annUler Quitter _Layers Objects All Undo eXit"
              nil
            )
          )
          " : "
          "eXit"
          (LgT
            (strcat
              "\nLONGCUMUL : Filter definition for selection"
              "\nDefault value:     eXit"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Select    | Select manually a linear object to add its layer property and       |"
              "\n  |             | object type property as filter. You can select one or more objects  |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Layers    | Opens a dialog box with the list of all layers name and let you     |"
              "\n  |             | select one or more layers within the list                           |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Objects   | Opens a dialog box with the list of all object types and let you    |"
              "\n  |             | select one or more object types within the list                     |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |     All     | Asks you to know if you want to select all objects with these       |"
              "\n  |             | properties (ignoring pre-selection) or if you have to select them   |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    Undo     | Deletes the last selected layer from the list. If you want to reset |"
              "\n  |             | the object type's list, use the Objects option                      |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    eXit     | Puts an end to the filter's definition and goes to the next step    |"
              "\n  |             | to select objects only in Model space                               |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            (strcat
              "\nLONGCUMUL : Définition du filtre pour la sélection"
              "\nValeur par défaut: Quitter"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |             | Sélectionnez manuellement un objet linéaire pour ajouter ses        |"
              "\n  |  Sélection  | propriétés de calque et type d'objet au filtre. Vous pouvez         |"
              "\n  |             | sélectionner un ou plusieurs objets                                 |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Calques   | Ouvre une boîte de dialogue avec la liste de l'ensemble des calques |"
              "\n  |             | et vous permet de sélectionner un ou plusieurs calques              |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Objets    | Ouvre une boîte de dialogue avec la liste de l'ensemble des types   |"
              "\n  |             | d'objet et vous permet de sélectionner un ou plusieurs types        |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |    Tous     | Demande si vous souhaitez sélectionner tous les objets avec ces     |"
              "\n  |             | propriétés (ignorant la présélection) ou si vous les sélectionnez   |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   annUler   | Supprime le dernier calque sélectionné de la liste. Si vous désirez |"
              "\n  |             | supprimer les types d'objet de la liste, utilisez l'option Objets   |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n  |   Quitter   | Met fin à la définition du filtre de sélection et passe à l'étape   |"
              "\n  |             | suivante pour effectuer la sélection dans l'espace Objet uniquement |"
              "\n  +-------------+---------------------------------------------------------------------+"
              "\n"
            )
            nil
          )
        )
      )
    )
    (cond
      ( (= "Layers" mode)
        (setq
          lst
            (ListBox
              (LgT
                "LONGCUMUL: Layer(s) selection"
                "LONGCUMUL: Sélection des calque(s)"
                nil
              )
              (LgT
                "Please, select one or more layer(s) :"
                "Veuillez sélectionner un ou plusieurs calque(s) :"
                nil
              )
              (vl-sort (vl-remove-if '(lambda (x) (wcmatch x "*|*")) (mapcar 'car (vla-collection->list nil 'layers 1))) '<)
              (cond ((cdr (assoc "LAYERS" param))) ((getvar "CLAYER")))
              2
              nil
            )
          param
            (subst
              (cons
                "LAYERS"
                (append
                  (cdr (assoc "LAYERS" param))
                  (vl-remove-if '(lambda (x) (member x (cdr (assoc "LAYERS" param)))) lst)
                )
              )
              (assoc "LAYERS" param)
              param
            )
        )
      )
      ( (= "Objects" mode)
        (setq
          lst
            (ListBox
              (LgT
                "LONGCUMUL: Object type(s) selection"
                "LONGCUMUL: Sélection des type(s) d'objet"
                nil
              )
              (LgT
                "Please, select one or more object type(s) :"
                "Veuillez sélectionner un ou plusieurs type(s) d'objet :"
                nil
              )
              (mapcar 'car type-list)
              (mapcar 'car (cdr (assoc "OBJECT" param)))
              2
              nil
            )
          param (subst (cons "OBJECT" (if lst (mapcar '(lambda (x) (assoc x type-list)) lst) type-list)) (assoc "OBJECT" param) param)
        )
      )
      ( (= "All" mode)
        (setq mode
          (getkdh
            (quote (getkword msg))
            (LgT
              "\nDo you want to select all objects within the current tab or to select them manually (+ pre-selection)"
              "\nSouhaitez-vous sélectionner tous les objets présents sur l'onglet courant ou les sélectionner manuellement (+ présélection)"
              nil
            )
            (list
              (LgT
                "Yes No _Yes No"
                "Oui Non _Yes No"
                nil
              )
            )
            " ? "
            (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil))
            (LgT
              (strcat
                "\nLONGCUMUL : Selection mode for objects"
                "\nDefault value:     Current value (" (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil)) ")"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Entire database. If you specify this selection method and do not    |"
                "\n  |     Yes     | provide filters, it will ignore the pre-selection and select all    |"
                "\n  |             | entities in the database, including entities on layers that are     |"
                "\n  |             | off, frozen, and out the visible screen                             |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | If you didn't pre-select any entity before LONGCUMUL (PICKFIRST=1), |"
                "\n  |     No      | asks you to select objects manually (filters ON). Otherwise, uses   |"
                "\n  |             | the current selection set and apply the filters on it               |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              (strcat
                "\nLONGCUMUL : Mode de sélection des objets"
                "\nValeur par défaut: Valeur courante (" (if (cdr (assoc "SELALL" param)) (LgT "Yes" "Oui" nil) (LgT "No" "Non" nil)) ")"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Toute la base de données. Si vous spécifiez cette méthode de        |"
                "\n  |     Oui     | sélection sans fournir de filtre, elle ignorera la présélection et  |"
                "\n  |             | sélectionnera toutes les entités de la base de données, y compris   |"
                "\n  |             | les entités des calques inactifs, gelés, et hors écran visible      |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n  |             | Si vous n'avez pas présélectionné d'entité avant LONGCUMUL          |"
                "\n  |     Non     | (PICKFIRST=1), demande de sélectionner les objets manuellement      |"
                "\n  |             | (filtres actifs). Sinon, utilise le jeu de sélection actuel et      |"
                "\n  |             | applique les filtres sur celui-ci                                   |"
                "\n  +-------------+---------------------------------------------------------------------+"
                "\n"
              )
              nil
            )
          )
        )
        (setq param (subst (cons "SELALL" (cond ((= "Yes" mode) T) ((= "No" mode) nil))) (assoc "SELALL" param) param))
      )
      ( (= "Undo" mode)
        (setq param
          (subst (cons "LAYERS" (reverse (cdr (reverse (cdr (assoc "LAYERS" param)))))) (assoc "LAYERS" param) param)
        )
      )
      ( (and
          (listp mode)
          (setq mode (entget (car mode)))
          (setq typ (cdr (assoc 0 mode)))
          (setq typ (vl-position typ (mapcar 'cdr type-list)))
          (setq typ (nth typ type-list))
          (setq lay (cdr (assoc 8 mode)))
        )
        (if (not (member typ (cdr (assoc "OBJECT" param))))
          (setq param (subst (cons "OBJECT" (append (cdr (assoc "OBJECT" param)) (list typ))) (assoc "OBJECT" param) param))
        )
        (if (not (member lay (cdr (assoc "LAYERS" param))))
          (setq param (subst (cons "LAYERS" (append (cdr (assoc "LAYERS" param)) (list lay))) (assoc "LAYERS" param) param))
        )
      )
      ( (= "eXit" mode)
        (setq break T)
        (if (null (cdr (assoc "OBJECT" param))) (setq param (subst (cons "OBJECT" type-list) (assoc "OBJECT" param) param)))
      )
      ( T (princ (LgT "\nInvalid selection..." "\nSélection invalide..." nil)))
    )
  )
  (setq
    lst nil
    jsel
      (apply
        'ssget
        (vl-remove
          nil
          (list
            (if (cdr (assoc "SELALL" param)) "_X")
            (vl-remove
              nil
              (list
                (if (setq lst (mapcar 'cdr (cdr (assoc "OBJECT" param)))) (cons 0 (lst2str lst ",")))
                (if (setq lst (cdr (assoc "LAYERS" param))) (cons 8 (lst2str lst ",")))
                (cons 410 "Model")
              )
            )
          )
        )
      )
    lst
      (loop-a-list-properties
        jsel
        (list 8 0)
        (quote
          (list
            (list
              (vlax-curve-getDistAtParam (vlax-ename->vla-object name) (vlax-curve-getEndParam (vlax-ename->vla-object name)))
              (cdr (assoc 5 (entget name)))
            )
          )
        )
        'append
        T
      )
  )
  (if
    (setq lst
      (vl-sort
        (mapcar
          '(lambda (x / lay tmp)
            (setq
              lay (car x)
              tmp (cdr x)
            )
            (cons lay (vl-sort tmp '(lambda (e1 e2) (< (car e1) (car e2)))))
           )
          lst
        )
        '(lambda (e1 e2) (< (car e1) (car e2)))
      )
    )
    (progn
      (setq tmp (LgC-Results-DCL))
      (princ
        (strcat
          (LgT
            "\nObject types's list : "
            "\nListe des types d'objet : "
            nil
          )
          (lst2str
            (mapcar
              'car
              (vl-remove-if-not
                '(lambda (x) (member (cdr x) (mapcar 'car (apply 'append (mapcar 'cdr lst)))))
                type-list
              )
            )
            ", "
          )
          "\n­"
          "\n   []+-+-+-+-+-+-+-+-+-+[]\n -> "
          (lst2str
            (mapcar
              '(lambda (x / lay tmp lng)
                (setq
                  lay (car x)
                  tmp (cdr x)
                  lng (mapcar '(lambda (l) (* coeff (+ add (car l)))) (apply 'append (mapcar 'cdr tmp)))
                )
                (strcat
                  (LgT
                    "Layer \""
                    "Calque \""
                    nil
                  )
                  lay
                  "\" :"
                  (if (LgC-bit (cdr (assoc "Dis_Length" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Total length      = "
                        "Longueur totale = "
                        nil
                      )
                      (ThsdSpace (rtos (apply '+ lng) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Count" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Number of objects = "
                        "Nombre d'objets = "
                        nil
                      )
                      (ThsdSpace (itoa (length lng)) (LgT "," " " nil)) "u"
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Max" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Maximum length    = "
                        "Longueur max.   = "
                        nil
                      )
                      (ThsdSpace (rtos (apply 'max lng) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Min" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Minimum length    = "
                        "Longueur min.   = "
                        nil
                      )
                      (ThsdSpace (rtos (apply 'min lng) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Avg" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Average length    = "
                        "Longueur moy.   = "
                        nil
                      )
                      (ThsdSpace (rtos (/ (apply '+ lng) (float (length lng))) 2 2) (LgT "," " " nil)) (units u)
                    )
                    ""
                  )
                  (if (LgC-bit (cdr (assoc "Dis_Dev" bitlist)) bit)
                    (strcat
                      "\n    • "
                      (LgT
                        "Standard deviation= "
                        "Ecart type      = "
                        nil
                      )
                      (ThsdSpace
                        (rtos
                          (sqrt (/ (apply '+ (mapcar '(lambda (x) (expt (- x (/ (apply '+ lng) (float (length lng)))) 2)) lng)) (float (length lng))))
                          2
                          2
                        )
                        (LgT "," " " nil)
                      )
                      (units u)
                    )
                    ""
                  )
                )
               )
              lst
            )
            "\n   []+-+-+-+-+-+-+-+-+-+[]\n -> "
          )
          "\n­"
          "\nTOTAL :"
          (LgT
            "\n  - Total length         = "
            "\n  - Longueur totale      = "
            nil
          )
          (ThsdSpace (rtos (apply '+ tmp) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Number of objects    = "
            "\n  - Nombre d'objets      = "
            nil
          )
          (ThsdSpace (itoa (length tmp)) (LgT "," " " nil)) "u"
          (LgT
            "\n  - Maximum length       = "
            "\n  - Longueur max.        = "
            nil
          )
          (ThsdSpace (rtos (apply 'max tmp) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Minimum length       = "
            "\n  - Longueur min.        = "
            nil
          )
          (ThsdSpace (rtos (apply 'min tmp) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Average length       = "
            "\n  - Longueur moy.        = "
            nil
          )
          (ThsdSpace (rtos (/ (apply '+ tmp) (float (length tmp))) 2 2) (LgT "," " " nil)) (units u)
          (LgT
            "\n  - Standard deviation   = "
            "\n  - Ecart type           = "
            nil
          )
          (ThsdSpace
            (rtos (sqrt (/ (apply '+ (mapcar '(lambda (x) (expt (- x (/ (apply '+ tmp) (float (length tmp)))) 2)) tmp)) (float (length tmp)))) 2 2)
            (LgT "," " " nil)
          )
          (units u)
          "\n  - Coefficient (x)      = " (ThsdSpace (rtos coeff 2 2) (LgT "," " " nil))
          (LgT
            "\n  - Add length (+)       = "
            "\n  - Longueur ajoutée (+) = "
            nil
          )
          (ThsdSpace (rtos add 2 2) (LgT "," " " nil)) (units u)
          "\n­"
        )
      )
      (setq i 0)
      (mapcar
        '(lambda (f)
          (princ
            (strcat
              (LgT
                "\nFile name n°"
                "\nNom du fichier n°"
                nil
              )
              (itoa (setq i (1+ i)))
              " : "
              f
            )
          )
        )
        CSV
      )
      (textscr)
    )
    (princ
      (LgT
        "\nNo object selected with the current filter..."
        "\nAucun objet sélectionné avec le filtre actuel..."
        nil
      )
    )
  )
  (SetVarList '(("DIMZIN" nil DIMZIN) ("LUPREC" nil LUPREC)))
  (sssetfirst nil jsel)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)


;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtObjet] OBJECT-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:CPH" "23/07/2024" "Luna" "2.0.0" "\"UtObjet\"")                             ;--•  Add the command's informations to $lst$ variable  ;

;                                            []-----------------------[] CPH []-----------------------[]                                            ;
;--- Date of creation       > 13/06/2024                                                                                                            ;
;--- Last modification date > 23/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

(defun c:CPH (/ *error* color->str htmlfile name obj entlist handle layer object color lst)
  (defun *error* (msg)
    (mapcar '(lambda (x) (vla-put-Visible x :vlax-true)) lst)
    (vlax-release-object htmlfile)
    (princ msg)
    (princ)
  )
  (defun color->str (dxf / c)
    (cond
      ( (cdr (assoc 430 dxf)))
      ( (setq c (cdr (assoc 420 dxf))) (lst2str (LM:True->RGB c) ", "))
      ( (setq c (cdr (assoc 62 dxf))) (itoa c))
      ( (strcat (color->str (entget (tblobjname "LAYER" (cdr (assoc 8 dxf))))) (LgT " (ByLayer)" " (DuCalque)" nil)))
    )
  )

  (setq htmlfile (vlax-get-or-create-object "htmlfile"))
  (while (setq name (car (entsel)))
    (setq
      obj (vlax-ename->vla-object name)
      entlist (entget name)
      handle (cdr (assoc 5 entlist))
      layer (cdr (assoc 8 entlist))
      object (cdr (assoc 0 entlist))
      color (color->str entlist)
      lst (cons obj lst)
    )
    (vlax-invoke
      (vlax-get
        (vlax-get htmlfile 'ParentWindow)
        'ClipBoardData
      )
      'SetData
      "Text"
      handle
    )
    (princ
      (LgT
        (strcat "\nThe handle \"" handle "\" has been copied into the clipboard...")
        (strcat "\nLe handle \"" handle "\" a été copié dans le presse-papier...")
        nil
      )
    )
    (vla-put-Visible obj :vlax-false)
  )
  (cond
    ( (null lst)
      (vlax-invoke
        (vlax-get
          (vlax-get htmlfile 'ParentWindow)
          'ClipBoardData
        )
        'SetData
        "Text"
        ""
      )
      (vlax-release-object htmlfile)
      (princ (LgT "\nNo object selected...\n" "\nAucun objet sélectionné...\n" nil))
      nil
    )
    ( (= 1 (length lst))
      (vlax-release-object htmlfile)
      (vla-put-Visible obj :vlax-true)
      (princ
        (LgT
          (strcat
            "\nProperties : "
            "\n  • Layer    = " layer
            "\n  • Object   = " object
            "\n  • Color    = " color
            (if (= "LWPOLYLINE" object)
              (strcat
                "\n  • Length   = "
                (vl-string-translate "." "," (rtos (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 2 2))
                (units (getvar "INSUNITS"))
              )
              ""
            )
            "\n"
          )
          (strcat
            "\nPropriétés : "
            "\n  • Calque   = " layer
            "\n  • Objet    = " object
            "\n  • Couleur  = " color
            (if (= "LWPOLYLINE" object)
              (strcat
                "\n  • Longueur = "
                (vl-string-translate "." "," (rtos (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 2 2))
                (units (getvar "INSUNITS"))
              )
              ""
            )
            "\n"
          )
          nil
        )
      )
      handle
    )
    ( (< 1 (length lst))
      (vlax-release-object htmlfile)
      (mapcar '(lambda (x) (vla-put-Visible x :vlax-true)) lst)
      (princ (LgT "\nRestoring visibility for all objects...\n" "\nRestauration de la visibilité des objets...\n" nil))
      lst
    )
  )
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:GETLAYER" "08/07/2022" "Luna" "3.0.0" "\"UtObjet\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] GETLAYER []-----------------------[]                                         ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 08/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

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

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:LCTOFC" "22/12/2022" "Luna" "1.0.1" "\"UtObjet\"")                          ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] LCtoFC []-----------------------[]                                           ;
;--- Date of creation       > 10/12/2021                                                                                                            ;
;--- Last modification date > 22/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

(defun c:LCtoFC (/ color# ent-s ent-t layer color62 color420 color430 jsel i)
  (defun color# (color key name)
    (cond
      ( (and
          color
          (assoc key (entget name))
        )
        (entmod (subst color (assoc key (entget name)) (entget name)))
      )
      ( (and
          color
          (not (assoc key (entget name)))
        )
        (entmod (append (entget name) (list color)))
      )
      ( (and
          (not color)
          (assoc key (entget name))
        )
        (entmod (vl-remove (assoc key (entget name)) (entget name)))
      )
    )
  )

  (while
    (and
      (setq ent-s (car (entsel (LgT "\nSelect source object : " "\nSélectionner l'objet source : " nil))))
      (if (not (assoc 62 (entget ent-s)))
        (setq
          layer (entget (tblobjname "LAYER" (cdr (assoc 8 (entget ent-s)))))
          color420 (assoc 420 layer)
          color430 (assoc 430 layer)
          color62 (assoc 62 layer)
        )
        (setq
          color420 (assoc 420 (entget ent-s))
          color430 (assoc 430 (entget ent-s))
          color62 (assoc 62 (entget ent-s))
        )
      )
    )
    (if
      (progn
        (princ (LgT "\nSelect target object(s) : " "\nSélectionner le(s) objet(s) cible(s) : " nil))
        (setq jsel (ssget))
      )
      (repeat (setq i (sslength jsel))
        (setq ent-t (ssname jsel (setq i (1- i))))
        (color# color62 62 ent-t)
        (color# color420 420 ent-t)
        (color# color430 430 ent-t)
      )
    )
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:OPENDEEPL" "04/07/2022" "Luna" "1.0.1" "\"UtObjet\"")                       ;--•  Add the command's informations to $lst$ variable  ;

;                                         []-----------------------[] OpenDeepL []-----------------------[]                                         ;
;--- Date of creation       > 25/01/2022                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

(defun c:OpenDeepL (/ *error* URLencode ACAD-string _GetClipBoardText mode name str)
  (defun *error* (msg)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (defun URLencode (URL / dec->hex f xpt)
    (defun dec->hex (n)
      (cond
        ( (< 15 n)
          (strcat (dec->hex (lsh n -4)) (dec->hex (rem n 16)))
        )
        ( (< n 10)
          (itoa n)
        )
        (T
          (chr (+ n 55))
        )
      )
    )
    (defun f (l / d)
      (if l
        (strcat
          (cond
            ( (or
                (< 47 (car l) 58)     ; [0-9]
                (< 64 (car l) 91)     ; [A-Z]
                (< 96 (car l) 123)    ; [a-z]
                (member (car l) xpt)  ; non-encoded special characters
              )
              (chr (car l))
            )
            ( (< (car l) 128)
              (strcat "%" (if (= 1 (strlen (setq d (dec->hex (car l))))) (strcat "0" d) d))
            )
            ( (< (car l) 192)
              (strcat "%C2%" (if (= 1 (strlen (setq d (dec->hex (car l))))) (strcat "0" d) d))
            )
            ( (< (car l) 256)
              (strcat "%C3%" (if (= 1 (strlen (setq d (dec->hex (- (car l) 64))))) (strcat "0" d) d))
            )
          )
          (f (cdr l))
        )
        ""
      )
    )
    (setq
      xpt
        (vl-string->list "-_.!~*'()")  ; JScript
        ;;(vl-string->list "-._~")       ; the Internet Society
        ;;(vl-string->list "-_.!*()")    ; .NET  
    )
    (f (vl-string->list URL))
  )
  (defun ACAD-string (str new old / len pos)
    (setq len (- (strlen (vl-prin1-to-string old)) 2))
    (while (setq pos (vl-string-search old str))
      (setq
        str
          (strcat
            (substr str 1 pos)
            new
            (substr str (+ pos len))
          )
      )
    )
    str
  )
  (defun _GetClipBoardText( / h r)
    (setq r
      (vlax-invoke
        (vlax-get
          (vlax-get
            (setq h (vlax-create-object "htmlfile"))
            'ParentWindow
          )
          'ClipBoardData
        )
        'GetData
        "Text"
      )
    )
    (vlax-release-object h)
    r
  )
  
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (initget "ObjectSelection Text")
  (cond
    ( (= "Text" (setq mode (getkword "\nSpecifie what you want to translate [ObjectSelection/Text] <ObjectSelection> : ")))
      (setq str (getstring T "\nEnter the text : "))
    )
    ( (null
        (while
          (not
            (and
              (setq name (nentsel "\nSelect an object with a text : "))
              (setq name (car name))
              (assoc 1 (entget name))
              (wcmatch (cdr (assoc 1 (entget name))) "*")
            )
          )
        )
      )
      (setq
        mode "ObjectSelection"
        str (cdr (assoc 1 (entget name)))
      )
    )
  )
  (showhtmlmodalwindow
    (strcat
      "https://www.deepl.com/translator#fr/en/"
      (progn
        (setq str (ACAD-string str "\n" "\\P"))
        (URLencode str)
      )
    )
  )
  (if name
    (entmod (subst (cons 1 (_GetClipBoardText)) (assoc 1 (entget name)) (entget name)))
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:PLINE3DTO2D" "19/07/2022" "Luna" "2.0.0" "\"UtObjet\"")                     ;--•  Add the command's informations to $lst$ variable  ;

;                                        []-----------------------[] PLINE3Dto2D []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 19/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

(defun c:PLINE3Dto2D (/ PL32-Properties doc param mode default jsel mode-a mode-d i n name plst zlst alt ent)
  (defun PL32-Properties (met)
    (null (vlax-put ent met (vlax-get name met)))
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq param
    (list
      (cons
        0
        (list
          (cons "miNimum" "miNimum")
          (cons "maXimum" "maXimum")
          (cons "Average" "moYenne")
          (cons "Zero" "Zéro")
        )
      )
      (cons
        1
        (list
          (cons "Yes" "Oui")
          (cons "No" "Non")
        )
      )
    )
  )
  (setq default
    (cond
      ((getenv "PLINE3Dto2D_Settings"))
      ((setenv "PLINE3Dto2D_Settings" (LgT "Zero, Yes" "Zéro, Oui" nil)))
    )
  )
  (and
    (setq default (str2lst default ", "))
    (or
      (setq jsel (ssget "_I" '((0 . "POLYLINE"))))
      (setq jsel
        (MuteSel
          (LgT
            "\nPlease select 3D polylines :"
            "\nSélectionner des polylignes 3D :"
            nil
          )
          (quote (ssget '((0 . "POLYLINE"))))
        )
      )
    )
    (setq mode-a
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nDo you want to maintain an average altitude for each polyline ?"
          "\nSouhaitez-vous conserver une altitude moyenne pour chaque polyligne ?"
          nil
        )
        (list
          (LgT
            (lst2str
              (list
                (lst2str (mapcar 'car (cdr (assoc 0 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 0 param))) " ")
              )
              " _"
            )
            (lst2str
              (list
                (lst2str (mapcar 'cdr (cdr (assoc 0 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 0 param))) " ")
              )
              " _"
            )
            nil
          )
        )
        " : "
        (car default)
        (LgT
          (strcat
            "\nPLINE3Dto2D : Polyline's elevation conversion"
            "\nDefault value:     <" (car default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   miNimum   | For each selected 3D polylines, apply the minimum Z coordinate as   |"
            "\n  |             | elevation value for the new substitute polyline object              |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   maXimum   | For each selected 3D polylines, apply the maximum Z coordinate as   |"
            "\n  |             | elevation value for the new substitute polyline object              |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   Average   | For each selected 3D polylines, calculate the average Z coordinate  |"
            "\n  |             | and set it as elevation value for the new substitute polyline       |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Zero    | For all selected 3D polylines, set the elevation value at 0.0 for   |"
            "\n  |             | all the new substitute polylines                                    |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          (strcat
            "\nPLINE3Dto2D : Conversion d'élévation des polylignes"
            "\nValeur par défaut: <" (car default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   miNimum   | Pour chaque polyligne 3D sélectionnée, utilise la coordonnée Z      |"
            "\n  |             | minimale comme valeur d'élévation pour la nouvelle polyligne        |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   maXimum   | Pour chaque polyligne 3D sélectionnée, utilise la coordonnée Z      |"
            "\n  |             | maximale comme valeur d'élévation pour la nouvelle polyligne        |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |   moYenne   | Pour chaque polyligne 3D sélectionnée, calcule une coordonnée Z     |"
            "\n  |             | moyenne comme valeur d'élévation pour la nouvelle polyligne         |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Zéro    | Pour toutes les polylignes 3D sélectionnées, défini l'élévation à   |"
            "\n  |             | 0.0 pour toutes les nouvelles polylignes remplaçantes               |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          nil
        )
      )
    )
    (setq mode-d
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nErase source objects ?"
          "\nEffacer les objets source ?"
          nil
        )
        (list
          (LgT
            (lst2str
              (list
                (lst2str (mapcar 'car (cdr (assoc 1 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 1 param))) " ")
              )
              " _"
            )
            (lst2str
              (list
                (lst2str (mapcar 'cdr (cdr (assoc 1 param))) " ")
                (lst2str (mapcar 'car (cdr (assoc 1 param))) " ")
              )
              " _"
            )
            nil
          )
        )
        " : "
        (cadr default)
        (LgT
          (strcat
            "\nPLINE3Dto2D : Source objects"
            "\nDefault value:     <" (cadr default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Yes     | Erase all the selected 3D polylines, only 2D polylines remain       |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     No      | The original objects are not erased, so both polylines remain       |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          (strcat
            "\nPLINE3Dto2D : Objets source"
            "\nValeur par défaut: <" (cadr default) ">"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Oui     | Efface toutes les polylignes 3D, seules les polylignes 2D existent  |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n  |     Non     | Les objets originaux ne sont pas effacés, les polylignes 2D et 3D   |"
            "\n  |             | sont conservées dans le dessin                                      |"
            "\n  +-------------+---------------------------------------------------------------------+"
            "\n"
          )
          nil
        )
      )
    )
    (setenv
      "PLINE3Dto2D_Settings"
      (lst2str
        (list
          (LgT mode-a (cdr (assoc mode-a (cdr (assoc 0 param)))) nil)
          (LgT mode-d (cdr (assoc mode-d (cdr (assoc 1 param)))) nil)
        )
        ", "
      )
    )
    (repeat (setq n 0 i (sslength jsel))
      (and
        (setq name (ssname jsel (setq i (1- i))))
        (setq plst (get-pt-list name))
        (setq name (vlax-ename->VLA-Object name))
        (if (= -1 (vlax-get name 'Closed)) (setq plst (reverse (cdr (reverse plst)))) T)
        (setq zlst (mapcar 'caddr plst))
        (setq alt
          (cond
            ( (= mode-a "miNimum") (apply 'min zlst))
            ( (= mode-a "maXimum") (apply 'max zlst))
            ( (= mode-a "Average") (/ (apply '+ zlst) (float (length plst))))
            ( (= mode-a "Zero") 0.0)
          )
        )
        (setq ent
          (vlax-invoke
            (vla-get-ModelSpace doc)
            'addLightWeightPolyline
            (apply
              'append
              (mapcar
                '(lambda (p) (list (car p) (cadr p)))
                plst
              )
            )
          )
        )
        (null (vla-put-Elevation ent alt))
        (PL32-Properties 'Closed)
        (PL32-Properties 'EntityTransparency)
        (PL32-Properties 'Layer)
        (PL32-Properties 'LineType)
        (PL32-Properties 'LineTypeScale)
        (PL32-Properties 'TrueColor)
        (ssdel (vlax-VLA-Object->ename name) jsel)
        (ssadd (vlax-VLA-Object->ename ent) jsel)
        (null (if (= mode-d "Yes") (vla-delete name)))
        (setq n (1+ n))
      )
    )
    (null (vla-EndUndoMark doc))
    (princ
      (strcat
        (LgT
          "\nA total of "
          "\nUn total de "
          nil
        )
        (itoa n)
        " / " 
        (itoa (sslength jsel))
        (LgT
          " 3D polylines has been successfully transformed."
          " polylignes 3D ont été transformées avec succès."
          nil
        )
      )
    )
    (null (sssetfirst))
    (sssetfirst nil jsel)
  )
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:POLYADDPOINT" "15/07/2024" "Luna" "2.0.2" "\"UtObjet\"")                    ;--•  Add the command's informations to $lst$ variable  ;

;                                        []-----------------------[] POLYADDPOINT []-----------------------[]                                       ;
;--- Date of creation       > 04/01/2022                                                                                                            ;
;--- Last modification date > 15/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.2                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

(defun c:POLYADDPOINT ( / *error* temp-circles-make temp-circles-delete temp-circles-update doc mode name i pt jsel n name pts)
  (defun *error* (msg)
    (temp-circles-delete)
    (setvar "CLAYER" layer)
    (command-s "_-LAYDEL" "_Name" tmp "" "_Yes")
    (vla-EndUndoMark doc)
    (princ msg)
  )
  (defun temp-circles-make (pt-list radius rgb / layer color obj lst)
    (setq
      color (vla-get-TrueColor (vla-Item (vla-get-layers doc) "0"))
      lst (vlax-ldata-get "URBASOLAR" "temp-obj")
    )
    (apply 'vla-setRGB (cons color rgb))
    (mapcar
      '(lambda (p)
        (setq obj (vla-AddCircle (vla-get-ModelSpace doc) (vlax-3D-point p) radius))
        (vla-put-TrueColor obj color)
        (vla-put-Linetype obj "Continuous")
        (setq lst (cons obj lst))
        (vlax-ldata-put "URBASOLAR" "temp-obj" lst)
      )
      pt-list
    )
    lst
  )
  (defun temp-circles-delete (/ lst)
    (setq lst (vlax-ldata-get "URBASOLAR" "temp-obj"))
    (mapcar '(lambda (x) (vl-catch-all-apply 'vla-delete (list x))) lst)
    (vlax-ldata-delete "URBASOLAR" "temp-obj")
  )
  (defun temp-circles-update (name i)
    (cond
      ( (= i -1)
        (temp-circles-make (list (vlax-curve-getStartPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
      )
      ( (null i)
        (temp-circles-make (list (vlax-curve-getEndPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
      )
      ( T
        (temp-circles-make
          (list (vlax-curve-getPointAtParam name i) (vlax-curve-getPointAtParam name (1- i)))
          (/ (getvar "VIEWSIZE") 100.)
          (CrosshairColor->RGB)
        )
      )
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq tmp "$temp$")
  (layer-get-or-create tmp (CrosshairColor->RGB) "Continuous" 0 0)
  (if (= tmp (setq layer (getvar "CLAYER")))
    (setq layer "0")
  )
  (setvar "CLAYER" tmp)
  (while (not (setq jsel (ssget "_:L" '((0 . "LWPOLYLINE")))))
    (princ (LgT "\nNo object selected..." "\nAucun objet sélectionné..." nil))
  )
  (cond
    ( (= 1 (sslength jsel))
      (setq
        name (ssname jsel 0)
        mode
          (getkdh
            (quote (getpoint msg))
            (LgT
              "\nSpecify the previous vertex or"
              "\nSpécifiez le sommet précédent ou"
              nil
            )
            (list 4 (LgT "Start End _Start End" "Début Fin _Start End" nil))
            " : "
            (LgT "End" "Fin" nil)
            nil
          )
      )
      (cond
        ( (listp mode)
          (setq pt (trans mode 1 0))
          (setq i (1+ (LM:round (vlax-curve-getparamatpoint name (vlax-curve-getclosestpointto name pt)))))
          (temp-circles-make (list (vlax-curve-getpointatparam name i)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
          (temp-circles-make (list (vlax-curve-getpointatparam name (1- i))) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
        )
        ( (= mode "Start")
          (setq i -1)
          (temp-circles-make (list (vlax-curve-getpointatparam name 0)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
        )
        ( (= mode "End")
          (setq i nil)
          (temp-circles-make (list (vlax-curve-getpointatparam name (vlax-curve-getendparam name))) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
          (if (bit 1 (cdr (assoc 70 (entget name))))
            (temp-circles-make (list (vlax-curve-getpointatparam name 1)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
          )
        )
      )
      (vla-EndUndoMark doc)
      (while
        (and
          (not (= pt "Close"))
          (setq pt
            (getkdh
              (quote (getpoint pt msg))
              (LgT "\nSpecify a new point" "\nSpécifiez un nouveau point" nil)
              (if (and (= mode "End") (not (bit 1 (cdr (assoc 70 (entget name))))))
                (list 4 (LgT "Close _Close" "Clore _Close" nil))
                nil
              )
              " : "
              nil
              nil
            )
          )
        )
        (vla-StartUndoMark doc)
        (if (= pt "Close")
          (entmod (subst (cons 70 (SwapBit 1 (cdr (assoc 70 (entget name))))) (assoc 70 (entget name)) (entget name)))
          (lwpoly-AddVertex name (trans pt 1 0) i)
        )
        (temp-circles-delete)
        (if (not (or (null i) (minusp i)))
          (setq i (1+ i))
        )
        (setvar "LASTPOINT" pt)
        (temp-circles-update name i)
        (vla-EndUndoMark doc)
      )
    )
    ( T
      (setq mode (getpoint (LgT "\nSpecify the previous vertex : " "\nSpécifiez le sommet précédent : " nil)))
      (setq mode (trans mode 1 0))
      (repeat (setq n (sslength jsel))
        (setq
          name (ssname jsel (setq n (1- n)))
          i (LM:round (vlax-curve-getParamAtPoint name (vlax-curve-getClosestPointTo name mode)))
        )
        (cond
          ( (equal i (vlax-curve-getEndParam name))
            (temp-circles-make (list (vlax-curve-getEndPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
            (setq i nil)
          )
          ( (equal i (vlax-curve-getStartParam name))
            (temp-circles-make (list (vlax-curve-getStartPoint name)) (/ (getvar "VIEWSIZE") 100.) (CrosshairColor->RGB))
            (setq i -1)
          )
          ( T
            (temp-circles-make
              (list (vlax-curve-getPointAtParam name i) (vlax-curve-getPointAtParam name (1+ i)))
              (/ (getvar "VIEWSIZE") 100.)
              (CrosshairColor->RGB)
            )
            (setq i (1+ i))
          )
        )
        (setq pts (cons (cons name i) pts))
      )
      (vla-EndUndoMark doc)
      (while (setq pt (getpoint (LgT "\nSpecify a new point : " "\nSpécifiez un nouveau point : " nil)))
        (vla-StartUndoMark doc)
        (setvar "LASTPOINT" pt)
        (setq pt (trans pt 1 0))
        (temp-circles-delete)
        (mapcar
          '(lambda (x / name i)
            (setq
              name (car x)
              i (cdr x)
            )
            (lwpoly-AddVertex name pt i)
            (if (not (or (null i) (minusp i)))
              (setq pts (subst (cons name (setq i (1+ i))) (assoc name pts) pts))
            )
            (temp-circles-update name i)
            x
           )
          pts
        )
        (vla-EndUndoMark doc)
      )
    )
  )
  (temp-circles-delete)
  (setvar "CLAYER" layer)
  (command "_-LAYDEL" "_Name" tmp "" "_Yes")
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:POLYDELPOINT" "15/07/2024" "Luna" "4.1.1" "\"UtObjet\"")                    ;--•  Add the command's informations to $lst$ variable  ;

;                                       []-----------------------[] POLYDELPOINT []-----------------------[]                                        ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 15/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 4.1.1                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

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

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:PVROWNUMBERING" "04/07/2024" "Luna" "1.0.0" "\"UtObjet\"")                  ;--•  Add the command's informations to $lst$ variable  ;

;                                       []-----------------------[] PVROWNUMBERING []-----------------------[]                                      ;
;--- Date of creation       > 04/07/2024                                                                                                            ;
;--- Last modification date > 04/07/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

(defun c:PVROWNUMBERING ( / val c jsel i name n lst mx mn)
  (setq jsel (ssget '((0 . "TEXT,MTEXT") (8 . "PVcase Row Numbering"))))
  (setq val
    (getkdh
      (quote (getint msg))
      (LgT
        "\nLast row's number"
        "\nNuméro de la dernière rangée"
        nil
      )
      (list 2)
      " : "
      (vlax-ldata-get "URBASOLAR" "PVROWNUMBERING_Start" 1)
      (LgT
        (strcat
          "\nPVROWNUMBERING : Last row's number"
          "\nCorresponds to the additional value that will be added on each selected text value."
          "\nFor example, if the last numbered row is 17 and the selected PVarea texts start at 1, specifying 17 means each row will have "
          "their value added to 17 (row n°1 → 1+17 = 18), making it easier to make sure each zone will follow the previous one."
        )
        (strcat
          "\nPVROWNUMBERING : Numéro de la dernière rangée"
          "\nCorrespond à la valeur qui sera ajoutée pour chaque texte sélectionné."
          "\nPar exemple, si la dernière rangée numérotée est 17 et les textes sélectionnés commencent à 1, alors chaque rangée se verra "
          "additionné de la valeur 17 (rangée n°1 → 1+17 = 18), simplifiant la continuité de numérotation des rangées pour l'ensemble du projet."
        )
        nil
      )
    )
  )
  (repeat (setq c (sslength jsel) i c)
    (setq name (ssname jsel (setq i (1- i))))
    (entmod
      (subst
        (cons 1 (itoa (setq n (+ val (atoi (cdr (assoc 1 (entget name))))))))
        (assoc 1 (entget name))
        (entget name)
      )
    )
    (setq lst (cons n lst))
  )
  (setq
    mx (apply 'max lst)
    mn (apply 'min lst)
  )
  (vlax-ldata-put "URBASOLAR" "PVROWNUMBERING_Start" mx)
  (princ
    (LgT
      (strcat
        "\n  • Amount of row = " (itoa (/ c 2)) " or " (itoa (1+ (- mx mn))) "u  (→ " (itoa i) " texts)"
        "\n  • First row     = " (itoa mn)
        "\n  • Last row      = " (itoa mx)
      )
      (strcat
        "\n  • Nombre de rangée = " (itoa (/ c 2)) " ou " (itoa (1+ (- mx mn))) "u  (→ " (itoa i) " textes)"
        "\n  • Première rangée  = " (itoa mn)
        "\n  • Dernière rangée  = " (itoa mx)
      )
      nil
    )
  )
  (princ)
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [UtVarsy] SYSTEME VARIABLE-HANDLING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:PSLTSCALECUSTOM" "04/07/2022" "Luna" "2.0.1" "\"UtVarsy\"")                 ;--•  Add the command's informations to $lst$ variable  ;

;                                      []-----------------------[] PSLTSCALECUSTOM []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.1                                                                                                                 ;
;--- Class                  > "UtVarsy"                                                                                                             ;

(defun c:PSLTSCALECUSTOM ( / *error* InitLayout nom value lst lng)
  (defun *error* (msg)
    (if InitLayout (setvar "CTAB" InitLayout))
    (if nom (setvar "NOMUTT" nom))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )

  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (SetVarList '(("CTAB" InitLayout nil)))
  (while
    (<
      1
      (setq value
        (getkdh
          (quote (getint msg))
          (LgT
            "\nEnter a new value for PSLTSCALE"
            "\nEntrez une nouvelle valeur pour PSLTSCALE"
            nil
          )
          (list 4 "0 1")
          ": "
          0
          '(help "" "PSLTSCALE")
        )
      )
    )
    (princ
      (LgT
        "\nRequires 0 or 1 only."
        "\nNécessite seulement 0 ou 1."
        nil
      )
    )
  )
  (SetVarList '(("NOMUTT" nom 1)))
  (setq lst
    (mapcar
      '(lambda (layout / lst psl)
        (setq
          lst (SetVarList (list (list "CTAB" nil layout) (list "PSLTSCALE" nil value)))
          psl (cadr lst)
        )
        (list layout (cadr psl) (last psl))
       )
      (ListBox
        (LgT
          "PSLTSCALE: Layout tab(s) selection"
          "PSLTSCALE: Sélection des présentation(s)"
          nil
        )
        (LgT
          "Please, select one or more layout tab(s) to be modified :"
          "Veuillez sélectionner la ou les présentation(s) à modifier :"
          nil
        )
        (mapcar 'car (vl-sort (get-layouts-pos) '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
        (layoutlist)
        2
        nil
      )
    )
  )
  (SetVarList (list (list "CTAB" nil InitLayout) (list "NOMUTT" nil nom)))
  (setq
    InitLayout nil
    nom nil
    lng (apply 'max (mapcar '(lambda (x) (strlen (car x))) lst))
  )
  (princ
    (strcat
      (LgT
        "\nThe PSLTSCALE system variable is now redefined as "
        "\nLa variable PSLTSCALE est désormais redéfinie à "
        nil
      )
      (itoa value)
      (LgT
        " for the following layout tab(s):"
        " pour les onglet(s) de présentations suivant :"
        nil
      )
      (apply
        'strcat
        (mapcar
          '(lambda (x) (strcat "\n •  " (car x) (space (- lng (strlen (car x)))) "   (" (itoa (cadr x)) " → " (itoa (last x)) ")"))
          lst
        )
      )
      (LgT
        "\nIn the list above, "
        "\nDans la liste ci-dessus, "
        nil
      )
      (itoa (length (vl-remove-if-not '(lambda (x) (= value (cadr x))) lst)))
      " / "
      (itoa (length lst))
      (LgT
        " layout tab(s) already had PSLTSCALE = "
        " onglet(s) de présentation avaient déjà PSLTSCALE = "
        nil
      )
      (itoa value)
      "."
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:TILEMODESWITCH" "07/06/2022" "Luna" "2.0.0" "\"UtVarsy\"")                  ;--•  Add the command's informations to $lst$ variable  ;

;                                       []-----------------------[] TileModeSwitch []-----------------------[]                                      ;
;--- Date of creation       > 27/05/2022                                                                                                            ;
;--- Last modification date > 07/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.0                                                                                                                 ;
;--- Class                  > "UtVarsy"                                                                                                             ;

(defun c:TileModeSwitch ( / )
  (BinaryVarSwap "TILEMODE")
  (princ)
)


;                                                   []-----------------------------------------[]                                                   ;

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BdBound] BOUNDING BOX MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:PREVIEWBOUNDINGBOX" "07/10/2022" "Luna" "2.0.1" "\"BbBound\"")              ;--•  Add the command's informations to $lst$ variable  ;

;                                    []-----------------------[] PreViewBoundingBox []-----------------------[]                                     ;
;--- Date of creation       > 04/07/2022                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.1                                                                                                                 ;
;--- Class                  > "BbBound"                                                                                                             ;

(defun c:PreViewBoundingBox (/ *error* box jsel ent mode)
  (defun *error* (msg)
    (if ent (vla-delete ent))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )

  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (and
    (or
      (setq jsel (ssget "_I"))
      (setq jsel (ssget))
    )
    (setq ent (PVBB-Draw jsel))
    (setq box (cdr ent))
    (setq ent (car ent))
    (sssetfirst nil jsel)
    (cond
      ( (= "AcDbPolyline" (vla-get-ObjectName ent))
        (vla-put-color ent 6)
        (princ
          (LgT
            (strcat
              "\nObject type : Polyline"
              "\nObject color: Magenta"
            )
            (strcat
              "\nType d'objet   : Polyligne"
              "\nCouleur d'objet: Magenta"
            )
            nil
          )
        )
      )
      ( (= "AcDb3dPolyline" (vla-get-ObjectName ent))
        (vla-put-color ent 1)
        (princ
          (LgT
            (strcat
              "\nObject type : 3D Polyline"
              "\nObject color: Red"
            )
            (strcat
              "\nType d'objet   : Polyligne 3D"
              "\nCouleur d'objet: Rouge"
            )
            nil
          )
        )
      )
      ( (= "AcDb3dSolid" (vla-get-ObjectName ent))
        (vla-put-color ent 4)
        (princ
          (LgT
            (strcat
              "\nObject type : 3D Solid"
              "\nObject color: Cyan"
            )
            (strcat
              "\nType d'objet   : Solide 3D"
              "\nCouleur d'objet: Cyan"
            )
            nil
          )
        )
      )
    )
    (setq mode
      (getkdh
        (quote (getkword msg))
        (LgT
          "\nErase created object ?"
          "\nEffacer l'objet créé ?"
          nil
        )
        (list
          (LgT
            "Yes No _Yes No"
            "Oui Non _Yes No"
            nil
          )
        )
        ": "
        "Yes"
        nil
      )
    )
  )
  (if (and ent (= "Yes" mode)) (vla-delete ent))
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
  box
)

;---◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ [BdSelct] SELECTION SET MANIPULATION ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘;

;                                                             --{ ◘◘◘◘◘ [   ] ◘◘◘◘◘ }--                                                             ;
($pLstAdd-U$ "C:MID_MOVE" "07/10/2022" "Luna" "2.0.2" "\"BbSelct\"")                        ;--•  Add the command's informations to $lst$ variable  ;

;                                          []-----------------------[] MID_MOVE []-----------------------[]                                         ;
;--- Date of creation       > 30/09/2021                                                                                                            ;
;--- Last modification date > 07/10/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.0.2                                                                                                                 ;
;--- Class                  > "BbSelct"                                                                                                             ;

(defun c:MID_MOVE (/ *error* osm cde jsel ent obj pt pt2)
  (defun *error* (msg)
    (if ent (vla-delete ent))
    (setvar "OSMODE" osm)
    (setvar "CMDECHO" cde)
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ msg)
  )
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if
    (and
      (setq osm (getvar "OSMODE"))
      (setq cde (getvar "CMDECHO"))
      (or
        (setq jsel (ssget "_I"))
        (setq jsel (ssget))
      )
      (setq ent (car (PVBB-Draw jsel)))
      (setq obj (vla-get-ObjectName ent))
      (null
        (cond
          ( (= "AcDbPolyline" obj) (vla-put-Color ent 6))
          ( (= "AcDb3dPolyline" obj) (vla-put-Color ent 1))
          ( (= "AcDb3dSolid" obj) (vla-put-Color ent 4))
        )
      )
      (null (vla-put-EntityTransparency ent 50))
      (setq pt (LM:ssBoundingBoxMidPt jsel))
      (ssadd (vlax-VLA-Object->ename ent) jsel)
      (setq pt2 (getpoint pt "\nSecond point : "))
    )
    (progn
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (command-s "_MOVE" jsel "" pt pt2)
      (setvar "OSMODE" osm)
      (setvar "CMDECHO" cde)
      (vla-delete ent)
    )
    (princ
      (LgT
        "\nNo object(s) selected..."
        "\nAucun objet(s) sélectionné(s)..."
        nil
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)



; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘ DISPLAY THE LIST OF PROGRAMS LOADED FOR EACH NEW DRAWING ◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
; |◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘◘| ;
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
    "\n   Time needed to execute the file \"UBS - LISP (Remote edition).lsp\" :"
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