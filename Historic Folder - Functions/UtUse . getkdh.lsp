
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                --{  getkdh  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                           []-----------------------[] getkdh []-----------------------[]                                          ;
;--- Date of creation       > 26/01/2022                                                                                                            ;
;--- Last modification date > 15/06/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 2.2.0                                                                                                                 ;
;--- Class                  > "UtUse"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Makes a user request using the getXXX functions that honor keywords (getint, getreal, getdist, getangle, getorient, getpoint, getcorner,        ;
;   getkword, entsel, nentsel and nentselp). You can set a default value if you want and display or not a message (= "?" as keyword) to help the    ;
;   users if needed.                                                                                                                                ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (getkdh) have 6 argument(s) :                                                                                                        ;
;   --•  fun                    > is the (quote) list of the getXXX functions (see above for the list of working functions) and have to be set like ;
;                               this -> (quote (getXXX [] msg [])), with :                                                                          ;
;                                       the (quote) function which is necessary for good working of the program                                     ;
;                                       the getXXX function corresponding to one of the authorized functions which work with (initget)              ;
;                                       the [] corresponding to the optional arguments of the specified getXXX function                             ;
;                                       and MSG which is also necessary to be define like this for good working of the program                      ;
;     (type fun) = 'LST                         | Ex. : (quote (getkword msg)), (quote (getpoint (0.0 0.0 0.0) msg)), (quote (nentselp msg pt)), ...;
;   --•  pfx                    > is the prefix of the [msg] for (getXXX) function, and will be prompt before the keyword list                      ;
;     (type pfx) = 'STR                         | Ex. : "\nQuel format d'écriture souhaitez vous", "\nWhich writing format do you want", nil, ...   ;
;   --•  arg                    > is the list of arguments for the initget function, like ([bit] [keywords]) (cf. (initget) help page)              ;
;     (type arg) = 'LST                         | Ex. : '(1 "Select "), '(), nil, '(5), '("annUler ? _Undo ?"),  ...                                ;
;   --•  sfx                    > is the suffix of the [msg] for (getXXX) function, and will be prompt after the keyword list and/or default value  ;
;     (type sfx) = 'STR                         | Ex. : "? ", ": ", nil, ...                                                                        ;
;   --•  dft                    > is the default value (if specified) used as a keyword, allowing the user to press ENTER to select it. The default ;
;                               as no needs to be one of the specified keyword, but if it does, it will returns the language-independent value (if  ;
;                               specified)                                                                                                          ;
;     (type dft) = ANY                          | Ex. : 1, "Simplified", '(0 . "INSERT"), "eXit", nil, ...                                          ;
;   --•  hlp                    > is the message you want to display in the command line historic to help user to understand the issue for each     ;
;                               keyword.                                                                                                            ;
;     (type hlp) = 'STR or 'LST                 | Ex. : "", "\nSimplified : ... \nDetailed", '(help "" "PSLTSCALE"), nil, ...                       ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "UtUse" ---> get                                           | v1.0.0 - 02/02/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtObjet" ---> C:GETLAYER                                  | v3.0.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (getkdh) returns the value specified by user. If the 'hlp' argument is specified and the user choose this value, the function will ;
;   display the help message and ask again for a keyword different of "?".                                                                          ;
;     Ex. : (getkdh (quote (getkword msg)) "\nWhich format do you want" '("A4 A3 A2 A0") " : " nil "\nHelp message...")                             ;
;           returns "A4", "A3", "A2" or "A0" if the user choose one of those or nil if the user press ENTER. If the user choose "?", the program    ;
;           will ask again the same question after displaying the help message. See below for the exemple of output in the command line :           ;
;     command: Which format do you want [A4/A3/A2/A0/?] : A1                                                                                        ;
;     command: Incorrect choice of option.                                                                                                          ;
;     command: Which format do you want [A4/A3/A2/A0/?] : ?                                                                                         ;
;     command: Help message...                                                                                                                      ;
;     command: Which format do you want [A4/A3/A2/A0/?] : A3                                                                                        ;
;     command: "A3"                                                                                                                                 ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.2.0   |   Extending the usability of 'hlp' to can be used as a string or quoted function aswell                                          | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.1.0   |   Extending the usability of 'dft' to be any type of value to match properly with all the getXXX function's value type           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   Changing the function name, deleting the use of (LgT) function internally and extending the usability with every getXXX        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Correcting the 'hlp' argument usability with (LgT)                                                                             | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

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