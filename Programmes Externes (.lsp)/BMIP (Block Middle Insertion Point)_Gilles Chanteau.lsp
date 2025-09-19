;; by Gilles Chanteau (_gile)
(defun gc:MostInnerPoint (obj fuzz / 2d-coord->pt-lst 3d-coord->pt-lst dich-sub len tmp)

  (defun 2d-coord->pt-lst (lst)
    (if lst (cons (list (car lst) (cadr lst)) (2d-coord->pt-lst (cddr lst))))
  );end_defun

  (defun 3d-coord->pt-lst (lst)
    (if lst (cons (list (car lst) (cadr lst) (caddr lst)) (3d-coord->pt-lst (cdddr lst))))
  );end_defun

  (defun dich-sub (inf sup / of new pts)
    (if (equal inf sup fuzz)
      (progn
        (setq of  (vlax-invoke obj 'Offset inf)
              pts (if (= (vla-get-ObjectName (car of)) "AcDbPolyline")
                    (2d-coord->pt-lst (vlax-get (car of) 'Coordinates))
                    (3d-coord->pt-lst (vlax-get (car of) 'ControlPoints))
                  );end_if
        );end_setq
        (mapcar 'vla-delete of)
        (mapcar (function (lambda (x) (/ x (length pts)))) (apply 'mapcar (cons '+ pts)))
      );end_progn
      (progn
        (setq new (/ (+ inf sup) 2.0)
              of  (vl-catch-all-apply 'vlax-invoke (list obj 'Offset new))
        );end_setq
        (if (vl-catch-all-error-p of)
          (dich-sub inf new)
          (progn
            (mapcar 'vla-delete of)
            (dich-sub new sup)
          )
        );end_if
      );end_progn
    );end_if
  );end_defun

  (if (and  (member (vla-get-ObjectName obj) '("AcDbPolyline" "AcDbSpline"))
            (vlax-curve-isClosed obj)
            (or (= (vla-get-ObjectName obj) "AcDbPolyline")
                (vlax-curve-isPlanar obj)
            );end_or
            (setq tmp (vl-catch-all-apply 'vlax-invoke (list obj 'Offset fuzz)))
            (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))
                  tmp (car tmp)
            );end_setq
            (if (< len (vlax-curve-getDistAtParam tmp (vlax-curve-getEndParam tmp)))
              (setq len (/ len (* -2 pi)))
              (setq len (/ len (* 2 pi)))
            );end_if
            (not (vla-delete tmp))
      );end_and
      (dich-sub 0.0 len)
  );end_if
);end_defun (MostInnerPoint)

(vl-load-com)

(defun c:bmip ( / *error* c_doc c_spc sv_lst sv_vals blk bname ss l_obj i_pt n_obj)

  (defun *error* ( msg )
    (mapcar 'setvar sv_lst sv_vals)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")) (princ (strcat "\nOops an Error : " msg " occurred")))
    (princ)
  );end_defun

  (setq c_doc (vla-get-activedocument (vlax-get-acad-object))
        c_spc (vlax-get-property c_doc (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
        sv_lst (list 'cmdecho 'osmode)
        sv_vals (mapcar 'getvar sv_lst)
  );end_setq

  (mapcar 'setvar sv_lst '(0 0))

  (setq blk (vlax-ename->vla-object (car (entsel "\nSelect Block to Insert : ")))
        bname (vlax-get blk 'name)
        ss (ssget '((0 . "LWPOLYLINE") (70 . 1)))
  );end_setq

  (cond (ss
          (repeat (setq cnt (sslength ss))
            (setq l_obj (vlax-ename->vla-object (ssname ss (setq cnt (1- cnt))))
                  i_pt (gc:MostInnerPoint l_obj 0.01)
                  n_obj (vla-InsertBlock c_spc (vlax-3d-point i_pt) bname 1 1 1 0)
            );end_setq
            (vlax-put-property n_obj 'layer (vlax-get-property blk 'layer))
          );end_repeat
        )
        (t (alert "No Polylines Selected"))
  );end_cond

  (mapcar 'setvar sv_lst sv_vals)
  (princ)
);end_defun