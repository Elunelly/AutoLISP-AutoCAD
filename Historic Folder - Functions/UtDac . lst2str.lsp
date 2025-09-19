
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  lst2str  }--                                                               | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                          []-----------------------[] lst2str []-----------------------[]                                          ;
;--- Date of creation       > 05/03/2021                                                                                                            ;
;--- Last modification date > 01/02/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtDac"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Converts a list of items into string format by adding a separator between each item in the list. It's just the inverse function of (str2lst).   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (lst2str) have 2 argument(s) :                                                                                                       ;
;   --•  lst                    > corresponds to the list of elements you want to convert into a string                                             ;
;     (type lst) = 'LST                         | Ex. : (layoutlist), '(0 1 2 3 4 5 6 7 8 9), ...                                                   ;
;   --•  sep                    > corresponds to the string separator you want to add between each item in the list                                 ;
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
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (lst2str) returns a string with each element of the 'lst' concatened by a string separator, nil if 'lst' is null.                  ;
;     Ex. : (lst2str (layoutlist) ",") returns "Model,Layout1,Layout2"                                                                              ;
;           (lst2str '("Ceci" "est" "un" "test" "!") " ") returns "Ceci est un test !"                                                              ;
;           (lst2str '("A" 0 "B" 1) " = ") returns "A = 0 = B = 1"                                                                                  ;
;           (lst2str nil " ") returns nil                                                                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Checks if 'lst' is not null and returns nil if false                                                                           | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

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