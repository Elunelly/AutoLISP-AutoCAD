
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                    HISTORICAL TRACKING FILE OF THE COMMAND                                                    | ;
; |                                                           --{  FixBadAttBlocks  }--                                                           | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                      []-----------------------[] FixBadAttBlocks []-----------------------[]                                      ;
;--- Date of creation       > 30/11/2018                                                                                                            ;
;--- Last modification date > 30/11/2018                                                                                                            ;
;--- Author                 > BeeKeeCZ                                                                                                              ;
;--- Version                > 1.0.0                                                                                                                 ;
;--- Class                  > "SdBlock"                                                                                                             ;

;--- Goal and utility of the command                                                                                                                ;
;   For all block of the 'Blocks collection (ActiveDocument only), correct the attributes issues where the attribute's definition has been deleted  ;
;   but the command ATTSYNC can't work on inserted block references due to the lack of attribute's definition in the block definition.              ;
;                                                                                                                                                   ;
;--- Explanation of how the command works step by step                                                                                              ;
; Step n°1        :                                                                                                                                 ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "XxXxx" ---> ...                                           | v#.#.# - ##/##/#### (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "VlObj" ---> gettags                                       | v1.0.0 - 30/11/2018 (BeekeeCZ)                                                ;
;                                                                                                                                                   ;
;--- Return on drawing                                                                                                                              ;
;   The command (FixBadAttBlocks) returns nothing                                                                                                   ;
;     Ex. : [...]                                                                                                                                   ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the command                                                                                                        | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun c:FixBadAttBlocks ( / gettags bkc bln doc lst tmp )
  (defun gettags ( def / rtn )
    (vlax-for obj def
      (if (= "AcDbAttributeDefinition" (vla-get-objectname obj))
        (setq rtn (cons (strcase (vla-get-tagstring obj)) rtn))
      )
    )
    rtn
  )

  (vl-load-com)
  (setq
    doc (vla-get-activedocument (vlax-get-acad-object))
    bkc (vla-get-blocks doc)
  )
  (vlax-for blk bkc
    (if (= :vlax-false (vla-get-isxref blk))
      (vlax-for obj blk
        (if
          (and
            (= "AcDbBlockReference" (vla-get-objectname obj))
            (= :vlax-true (vla-get-hasattributes obj))
            (or
              (setq tmp (assoc (setq bln (vla-get-name obj)) lst))
              (and
                (setq tmp (cons bln (gettags (vla-item bkc bln))))
                (setq lst (cons tmp lst))
              )
            )
          )
          (foreach att (vlax-invoke obj 'getattributes)
            (or
              (member (strcase (vla-get-tagstring att)) (cdr tmp))
              (and
                (vlax-write-enabled-p att)
                (vla-delete att)
              )
            )
          )
        )
      )
    )
  )
  (vla-regen doc acallviewports)
  (princ)
)