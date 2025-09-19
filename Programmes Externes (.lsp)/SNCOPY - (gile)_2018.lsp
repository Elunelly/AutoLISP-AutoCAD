;; Author : Gilles Chanteau
;; Date   : 10/09/2018
;; Version: 3.0.0.0
;; 
;;  I nead a lisp that should copy selected multiple nested blocks from master block.
;;  Suppose I have a master block from a Architect, that contains chairs and tables as blocks inside the master block.
;;  The lisp code should copy only the selected nested blocks to outside. Please have a look on attached dwg and image.
;;
;;  I got a lisp code that can copy nested elements from master block, The requirement is instead of copying nested elements the
;;  lisp should copy only selected nested blocks  from master block.

(defun c:SNCOPY (/ *error* space ss nent br copy xdata)
  (vl-load-com)
  (or *acad* (setq *acad* (vlax-get-acad-object)))
  (or *acdoc* (setq *acdoc* (vla-get-ActiveDocument *acad*)))

  (defun *error* (msg)
    (and msg
      (/=(strcase msg) "FUNCTION CANCELLED")
      (princ (strcat "\nError: " msg))
    )
    (vla-EndUndoMark *acdoc*)
    (princ)
  )

  (vla-StartUndoMark *acdoc*)
  (setq space
    (if (= 1 (getvar 'cvport))
      (vla-get-PaperSpace *acdoc*)
      (vla-get-ModelSpace *acdoc*)
    )
  )
  (setq ss (ssadd))
  (while
    (and
      (setq nent (nentselp "\nSelect a nested block: "))
      (= 4 (length nent))
      (= "AcDbBlockReference" (vla-get-ObjectName (setq br (vlax-ename->vla-object (car (last nent))))))
      (apply '= (mapcar 'abs (list (vla-get-XScaleFactor br) (vla-get-YScaleFactor br) (vla-get-ZScaleFactor br))))
    )
    (setq xdata (assoc -3 (entget (vlax-vla-object->ename br)'("*"))))
    (setq copy (vla-insertblock space (vlax-3d-point '(0 0 0)) (vla-get-Name br) 1.0 1.0 1.0 0.0))
    (if xdata (entmod (append (entget (vlax-vla-object->ename copy)) (list xdata))))
    (vla-TransformBy copy (vlax-tmatrix (caddr nent)))
    (vla-Highlight copy :vlax-true)
    (ssadd (vlax-vla-object->ename copy) ss)
  )
  (command "_.move" ss "")
  (*error* nil)
)