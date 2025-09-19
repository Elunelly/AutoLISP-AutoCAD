
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                 --{  atol  }--                                                                | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] atol []-----------------------[]                                           ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 05/01/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts any LISP object into a list of single string, representing each character of the object. It uses the (vl-princ-to-string), (chr) and   ;
;   (vl-string->list) functions to do that.                                                                                                         ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (atol) have 1 argument(s) :                                                                                                          ;
;   --•  str                    > the element you want to decompose into a list of string                                                           ;
;     (type str) = '...                         | Ex. : "Texte", '(1 2 3), -12.535, ...                                                             ;
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
;   The function (atol) returns the representation of any LISP object as a string, decomposed in a list of single characters.                       ;
;     Ex. : (atol "Texte") returns ("T" "e" "x" "t" "e")                                                                                            ;
;           (atol '(1 2 3)) returns ("(" "1" " " "2" " " "3" ")")                                                                                   ;
;           (atol -12.53) returns ("-" "1" "2" "." "5" "3")                                                                                         ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Re-design of the function, simplification of writing and extension of the functionality for any LISP object                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun atol (str)
  (mapcar 'chr (vl-string->list (vl-princ-to-string str)))
)