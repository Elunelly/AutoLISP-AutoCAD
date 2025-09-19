
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                               --{  get-Date  }--                                                              | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                         []-----------------------[] get-Date []-----------------------[]                                          ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 19/06/2020                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "UtWin"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Retrieves the date and time at runtime in the form "DD/MM/YYYY - HH:MM:SS".                                                                     ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (get-Date) have 0 argument(s) :                                                                                                      ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (get-Date) returns a string in the form "DD/MM/YYYY - HH:MM:SS" where's "DD" represents the day, "MM" represents the month, "YYYY" ;
;   represents the year and "HH" represents the hour, "MM" represents the minute, "SS" represents the seconds.                                      ;
;     Ex. : (get-Date) returns "24/01/2022 - 12:35:21"                                                                                              ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun get-Date (/ r d h)

  (setq
    r (rtos (getvar "CDATE") 2 6)
    d (strcat (substr r 7 2) "/" (substr r 5 2) "/" (substr r 1 4))
    h (strcat (substr r 10 2) ":" (substr r 12 2) ":" (substr r 14 2))
    r (strcat d " - " h)
  )

)