
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  Wildcard-trim  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] Wildcard-trim []-----------------------[]                                       ;
;--- Date of creation       > 10/12/2021                                                                                                            ;
;--- Last modification date > 10/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   When used for functions (vl-string-*-trim), lists all the characters corresponding to the specified pattern in the form of a list from the 256  ;
;   characters present in the ASCII table.                                                                                                          ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Wildcard-trim) have 1 argument(s) :                                                                                                 ;
;   --•  w                      > correspond to the pattern that each character needs to match                                                      ;
;     (type w) = 'STR                           | Ex. : "#", "@", "~#", ".,#", ...                                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Wildcard-trim) returns a list of characters (1 single character per string) whose characteristics correspond to the wildcard      ;
;   specified in argument (see wcmatch for the Wild-Card Characters).                                                                               ;
;     Ex. : (Wildcard-trim "#") returns ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "²" "³" "¹")                                                       ;
;           (Wildcard-trim "@") returns ("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"    ;
;                                        "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"    ;
;                                        "ƒ" "Š" "Œ" "Ž" "š" "œ" "ž" "Ÿ" "ª" "µ" "º" "À" "Á" "Â" "Ã" "Ä" "Å" "Æ" "Ç" "È" "É" "Ê" "Ë" "Ì" "Í" "Î"    ;
;                                        "Ï" "Ð" "Ñ" "Ò" "Ó" "Ô" "Õ" "Ö" "Ø" "Ù" "Ú" "Û" "Ü" "Ý" "Þ" "ß" "à" "á" "â" "ã" "ä" "å" "æ" "ç" "è" "é"    ;
;                                        "ê" "ë" "ì" "í" "î" "ï" "ð" "ñ" "ò" "ó" "ô" "õ" "ö" "ø" "ù" "ú" "û" "ü" "ý" "þ" "ÿ")                       ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun wildcard-trim (w / i lst a)
  (repeat (setq i 256)
    (if (wcmatch (setq a (chr (setq i (1- i)))) w)
      (setq lst (cons a lst))
    )
  )
  lst
)