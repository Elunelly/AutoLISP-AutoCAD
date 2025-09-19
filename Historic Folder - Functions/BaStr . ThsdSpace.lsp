
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                              --{  ThsdSpace  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] ThsdSpace []-----------------------[]                                         ;
;--- Date of creation       > 29/12/2021                                                                                                            ;
;--- Last modification date > 29/12/2021                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaStr"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   For a string representing a positive or negative number (integer or real), adds a separator for thousands to simplify the reading of large      ;
;   numbers. Usually, the thousands separator is a comma "," for English speakers and a space " " for French speakers.                              ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (ThsdSpace) have 2 argument(s) :                                                                                                     ;
;   --•  str                    > represents the number in string format that needed to be modified                                                 ;
;     (type str) = 'STR                         | Ex. : "1000.15", "5134864649841.473468", "-42", ...                                               ;
;   --•  sep                    > represents the string used to separate each thousands, usually a comma "," (English) or a space " " (French)      ;
;     (type sep) = 'STR                         | Ex. : ",", " ", "/", ...                                                                          ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (ThsdSpace) returns the string formated with the specified separator for thousands, or nil if str is not representing a number.    ;
;     Ex. : (ThsdSpace "1000.15" " ") returns "1 000.15"                                                                                            ;
;           (ThsdSpace "5134864649841.473468" ",") returns "5,134,864,649,841.473468"                                                               ;
;           (ThsdSpace "-42" " ") returns "-42"                                                                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

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