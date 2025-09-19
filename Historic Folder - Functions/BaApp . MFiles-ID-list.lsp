
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                           --{  MFiles-ID-list  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] MFiles-ID-list []-----------------------[]                                       ;
;--- Date of creation       > 19/12/2022                                                                                                            ;
;--- Last modification date > 19/12/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "BaApp"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   Lists the M-Files properties (names and ID) needed for AutoCAD.                                                                                 ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (MFiles-ID-list) have 0 argument(s) :                                                                                                ;
;   --•  ...                    >                                                                                                                   ;
;     (type ...) = '...                         | Ex. :                                                                                             ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "BaApp" ---> MFiles-field-generator                        | v1.0.0 - 19/12/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (MFiles-ID-list) returns a list of dotted-pair for each MFiles property filled in manually                                         ;
;     Ex. : (MFiles-ID-list) returns ...                                                                                                            ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun MFiles-ID-list ()
  (list
    (cons "Projet(s)"                                       "PGFE106511CD7B4CBAAB72BA8DAAD04C46") ; #00
    (cons "Code"                                            "PG821E546988074DCC93B9004E8B1CDB19") ; #01
    (cons "Nom du Projet"                                   "PG657DD3AAE5D941E7903C91F9CDBE5089") ; #02
    (cons "Commune"                                         "PG8875B0DCFED340CDAEB409E12D7D5FE4") ; #03
    (cons "Département"                                     "PG10AD0DD70B244F79996DEA07602355EF") ; #04
    (cons "Phase de design"                                 "PG244CF5DABF044278AC113C1017C8D835") ; #05
    (cons "Trigramme phase"                                 "PG18CD3574A3044970AD945B135B5AE67E") ; #06
    (cons "Créateur"                                        "PG7E23981CEA5E45C59622D23CDC57317E") ; #07
    (cons "Triptique"                                       "PGF9CADF4B98E74C1993987A4D23E3164C") ; #08
    (cons "Dessinateur(s) Projeteur (s)"                    "PG79D94E3637014DE59F9279E3E3419FD0") ; #09
    (cons "Contributeur - Dessinateur(s) Projeteur(s)"      "PG778D2C441D6A47928C265E29644FABAD") ; #10
    (cons "Chargé(s) Affaire(s)"                            "PG18EA95888BAC4EACBDB693DC1C8644F7") ; #11
    (cons "Contributeur - Chargé(s) Affaire(s)"             "PGF322C50702FF43B28F38F4BBA4C9136F") ; #12
    (cons "Chef(s) de Projets Développement"                "PG57F89F35A3304B2EA352BF6BE87152F6") ; #13
    (cons "Contributeur - Chef(s) de Projets Développement" "PG4481AFAA29F24DE3B1E46781A2B30E48") ; #14
    (cons "Chargé(s) d'Etude(s)"                            "PGECA7DEE952A34FCBA5C8AD4256A59527") ; #15
    (cons "Contributeur - Chargé(s) d'Etude(s)"             "PG6A29938E245640AA8A84F95B20E96EC6") ; #16
    (cons "Chef(s) de Projets Construction"                 "PG8760825C00594719950D20DE3FF9C89E") ; #17
    (cons "Contributeur - Chef(s) de Projets Construction"  "PGDEBDD516C6794D59A95E88EABDC853DE") ; #18
    (cons "Chargé(s) de Travaux"                            "PGB0D5220D8A1E482492A1767D23539AE9") ; #19
    (cons "Contributeur - Chargé(s) de Travaux"             "PGFC25A392881E4D558B887BB91732B50B") ; #20
    (cons "Automaticien(s)"                                 "PGF1D0DFF36BA0427298A8D7EDBA3FF1BD") ; #21
    (cons "Contributeur - Automaticien(s)"                  "PG9BA0A300264D4A91897AC2048E3FC1AB") ; #22
  )
)