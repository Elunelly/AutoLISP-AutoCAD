
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                                 --{  LgT  }--                                                                 | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                            []-----------------------[] LgT []-----------------------[]                                            ;
;--- Date of creation       > 10/01/2022                                                                                                            ;
;--- Last modification date > 12/05/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.0                                                                                                                 ;
;--- Class                  > "UtWin"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Depending on the value of "LOCALE" system variable ("FR" or "EN", equals any other language), it will returns the french version of a string or ;
;   the english version. There's no possibilities to translate in a smart way any string, so you have to specify the french and english version each;
;   time you wanna use this function properly. You can force the prompting if you want.                                                             ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (LgT) have 3 argument(s) :                                                                                                           ;
;   --•  en                     > represents the english version of the string to prompt                                                            ;
;     (type en) = 'STR                          | Ex. : "Select object", ...                                                                        ;
;   --•  fr                     > represents the french version of the string to prompt                                                             ;
;     (type fr) = 'STR                          | Ex. : "Sélectionner un objet", ...                                                                ;
;   --•  flag                   > controls if it will check the value of "LOCALE" system variable, or force the english / french version            ;
;     (type flag) = 'INT                        | Ex. : 0 or anything (= if "LOCALE" equals "FR", prompt 'fr', prompt 'en' otherwise),              ;
;                                                       1 (= prompt 'en') or 2 (= prompt 'fr'), nil (= check the value of (getenv "FORCEDLANGUAGE"));
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;   --•  "PsViewp" ---> c:FNTAERIENNE                               | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsLaytb" ---> c:QuickLayoutSwitch                         | v3.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:HANDLEPREVIEW                             | v1.2.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtFiles" ---> c:VP-RADPURGE                               | v2.1.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:ALTPOINT                                  | v3.0.0 - 08/08/2022 (Luna)                                                    ;
;   --•  "UtGeodt" ---> c:LONGCUMUL                                 | v3.0.2 - 04/07/2022 (Luna)                                                    ;
;   --•  "UtObjet" ---> C:GETLAYER                                  | v3.0.0 - 08/07/2022 (Luna)                                                    ;
;   --•  "UtVarsy" ---> c:PSLTSCALECUSTOM                           | v2.0.1 - 04/07/2022 (Luna)                                                    ;
;   --•  "BbBound" ---> c:PreViewBoundingBox                        | v1.0.0 - 04/07/2022 (Luna)                                                    ;
;   --•  "BbSelct" ---> c:MID_MOVE                                  | v2.0.0 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (LgT) returns the french or english version of the string depending of the 'flag' value and the "LOCALE" value (if 'flag' = 0).    ;
;     Ex. : (LgT "Select object" "Sélection d'un objet" 0) returns "Sélection d'un objet" if "LOCALE" equals "FR", "Select object" otherwise        ;
;           (LgT "Select object" "Sélection d'un objet" 1) returns "Select object"                                                                  ;
;           (LgT "Select object" "Sélection d'un objet" 2) returns "Sélection d'un objet"                                                           ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Add the possibility of nil for 'flag' value, to use the environment variable "FORCEDLANGUAGE" value                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun LgT (en fr flag)
  (cond
    ( (null flag) (LgT en fr (if (getenv "FORCEDLANGUAGE") (atoi (getenv "FORCEDLANGUAGE")) 0)))
    ( (= flag 1) en)
    ( (= flag 2) fr)
    ( T (LgT en fr (if (= (getvar "LOCALE") "FR") 2 1)))
  )
)