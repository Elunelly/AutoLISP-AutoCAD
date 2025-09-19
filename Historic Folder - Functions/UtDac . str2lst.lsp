
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  str2lst  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] str2lst []-----------------------[]                                          ;
;--- Date of creation       > 15/04/2017                                                                                                            ;
;--- Last modification date > 15/04/2017                                                                                                            ;
;--- Author                 > (gile)                                                                                                                ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a string representing a list of items into list format by using a separator detection in the string. If the separator is not found,    ;
;   the list will only contains one element.                                                                                                        ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (str2lst) have 2 argument(s) :                                                                                                       ;
;   --•  str                    > corresponds to the string representing a list of elements you want to convert into a list                         ;
;     (type str) = 'STR                         | Ex. : "label1,label2,label3", "Scale Lineweight Length Width", ...                                ;
;   --•  sep                    > corresponds to the string separator that will be use to separate each element from the string into the list       ;
;     (type sep) = 'STR                         | Ex. : "", " ", ",", "/", ...                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "UtUse" ---> getkdh                                        | v2.1.0 - 02/02/2022 (Luna)                                                    ;
;   --•  "DtSel" ---> Select-Filter                                 | v3.2.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "DbLst" ---> ListBox                                       | v4.0.0 - 11/05/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (str2lst) returns a list with each element detected in the 'str' each time a separator is found in the string.                     ;
;     Ex. : (str2lst "label1,label2,label3" ",") returns '("label1" "label2" "label3")                                                              ;
;           (str2lst "Scale Lineweight Length Width" " ") returns '("Scale" "Lineweight" "Length" "Width")                                          ;
;           (str2lst "label1,label2,label3" "/") returns '("label1,label2,label3")                                                                  ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons
      (substr str 1 pos)
      (str2lst (substr str (+ (strlen sep) pos 1)) sep)
    )
    (list str)
  )
)