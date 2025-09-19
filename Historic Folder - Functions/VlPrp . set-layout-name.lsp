
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  set-layout-name  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] set-layout-name []-----------------------[]                                      ;
;--- Date of creation       > 19/06/2020                                                                                                            ;
;--- Last modification date > 21/06/2024                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.1.1                                                                                                                 ;
;--- Class                  > "VlPrp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Allows you to rename layout tabs by specifying the name of the existing layout and the new name. If the new name specified already exists in    ;
;   layout list, add a " (2)" or higher increment to the new name to avoid any problems.                                                            ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (set-layout-name) have 2 argument(s) :                                                                                               ;
;   --•  str-old                > is the old name of the layout you want to rename, so it has to be a name of an existing layout                    ;
;     (type str-old) = 'STR                     | Ex. : "layout1", "1030-01", (getvar "CTAB"), ...                                                  ;
;   --•  str-new                > is the new name of the layout you want to rename, it can be any name, even an existing one                        ;
;     (type str-new) = 'STR                     | Ex. : "layout2", "3020-00", (getvar "CTAB"), ...                                                  ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "VlCol" ---> vla-collection->list                          | v2.0.0 - 19/08/2021 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (set-layout-name) returns the new name of the renamed layout or nil if the 'str-old' specified doesn't exist in the drawing.       ;
;     Ex. : (set-layout-name "layout1" "layout2") returns "layout2" if "layout2" doesn't exist, "layout2 (2)" if it exists or nil if "layout1" does ;
;           not exist.                                                                                                                              ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.1   |   Fixed the error Automation when blank spaces were left on the right and/or left of the str-new                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.1.0   |   Suppression of the message displayed if successful and renaming of variables and arguments                                     | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

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