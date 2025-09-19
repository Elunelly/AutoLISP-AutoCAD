
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                              --{  OpenDeepL  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] OpenDeepL []-----------------------[]                                         ;
;--- Date of creation       > 25/01/2022                                                                                                            ;
;--- Last modification date > 04/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.1                                                                                                                 ;
;--- Class                  > "UtObjet"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   Allows you to translate a text through a translation page DeepL from a handwritten text or an object containing a text.                         ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        : Ask the user if he want to translate an handwritten text or a text property contained in an object (TEXT, MTEXT, MLEADER, ...)  ;
;   Step n°1.a    :   If the answer is "Text", let the user write a one line text (space enabled)                                                   ;
;   Step n°1.b    :   If the answer is "ObjectSelection", let the user select one entity that have a DXF code 1 (loop on the selection)             ;
; Step n°2        : Uses the function (URLencode) to get the translation between AutoCAD format and HTML format of the string                       ;
; Step n°3        : Opens a modal window of HTML onto the DeepL site and set the "text to translate" as default with the string                     ;
; Step n°4        : The user have to copy the translated text on his clipboard (CTRL+C) before the closing of the modal window                      ;
; Step n°5        : If the user selected an entity during the command, it will (entmod) the first DXF code 1 with the value in the clipboard        ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaStr" ---> ACAD-string                                   | v1.0.0 - 25/01/2022 (Luna)                                                    ;
;   --•  "BaErr" ---> *error*                                       | v1.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtDac" ---> URLencode                                     | v1.1.0 - 25/01/2022 ((gile) / Luna)                                           ;
;   --•  "UtDev" ---> _GetClipBoardText                             | v1.0.0 - 06/03/2008 (Patrick_35)                                              ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (OpenDeepL) returns firstly the HTML modal window for DeepL.com, which needs an intervention of the user and then return nothing.   ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.1   |   Add the functions (vla-StartUndoMark) and (vla-EndUndoMark) to group all the modifications of the program in a single CTRL+Z   | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:OpenDeepL (/ URLencode ACAD-string _GetClipBoardText mode name str)
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