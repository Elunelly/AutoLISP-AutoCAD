

(defun princ-a-list-properties (lst msg p-lst fun / r p s u c)
  (setq
    r (car p-lst)
    p (car r)    ; Prefix
    s (cadr r)   ; Suffix
    u (caddr r)  ;
    c (cadddr r) ; Count
  )
  (if msg (princ msg))
  (mapcar
    '(lambda (l / k v)
      (setq
        k (car l)
        v (cdr l)
      )
      (princ
        (strcat
          (cond (p) (""))
          (vl-princ-to-string k)
          (cond (s) (""))
        )
      )
      (princ
        (strcat
          (vl-princ-to-string
            (if (null (cdr p-lst))
              v
              (setq v (apply c (princ-a-list-properties v nil (cdr p-lst) fun)))
            )
          )
          (cond (u) (""))
        )
      )
      v
     )
    (if fun
      (vl-sort lst '(lambda (kp1 kp2) ((eval fun) (car kp1) (car kp2))))
      lst
    )
  )
)

;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;

(defun princ-a-list-properties (lst msg p-lst fun / r p s u c)
  (setq
    r (car p-lst)
    p (car r)
    s (cadr r)
    u (caddr r)
    c (cadddr r)
  )
  (if msg (princ msg))
  (mapcar
    '(lambda (l / k v)
      (setq
        k (car l)
        v (cdr l)
      )
        (strcat
          (cond (p) (""))
          (vl-princ-to-string k)
          (cond (s) (""))
          (vl-princ-to-string
            (if (null (cdr p-lst))
              v
              (setq v (apply c (princ-a-list-properties v nil (cdr p-lst) fun)))
            )
          )
          (cond (u) (""))
        )
      v
     )
    (if fun
      (vl-sort lst '(lambda (kp1 kp2) ((eval fun) (car kp1) (car kp2))))
      lst
    )
  )
)

;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;
;; []========================================================================================================[] ;;













































(defun princ-a-list-properties (lst msg p-lst fun / l r p s u c)
  (setq
    r (car p-lst)
    p (car r)
    s (cadr r)
    u (caddr r)
    c (cadddr r)
  )
  (if msg
    (princ
      (strcat
        msg
        (car (princ-a-list-properties lst nil p-lst fun))
      )
    )
    (progn
      (setq l
        (mapcar
          '(lambda (l / k v)
            (setq
              k (car l)
              v (cdr l)
            )
            (cons
              (strcat
                (cond (p) (""))
                (vl-princ-to-string k)
                (cond (s) (""))
                (vl-princ-to-string
                  (if (null (cdr p-lst))
                    v
                    (setq v (cdr (setq l (princ-a-list-properties v nil (cdr p-lst) fun))))
                  )
                )
                (cond (u) (""))
                (if (cdr p-lst)
                  (car l)
                )
              )
              v
            )
          )
          (if fun
            (vl-sort lst '(lambda (kp1 kp2) ((eval fun) (car kp1) (car kp2))))
            lst
          )
        )
      )
      (cons
        (apply 'strcat (mapcar 'car l))
        (apply c (mapcar 'cdr l))
      )
    )
  )
)

(defun loop-a-list-properties (jsel PropertyList value f / i name lst)
  (if jsel
    (repeat (setq i (sslength jsel))
      (setq
        name (ssname jsel (setq i (1- i)))
        lst
          (make-a-list-properties
            lst
            (mapcar
              '(lambda (pp / ppType)
                (setq ppType (type pp))
                (cdr
                  (GetAnyProperty
                    name
                    "*"
                    (cond ((= ppType 'INT) 0) ((= ppType 'STR) 1) ((= ppType 'SYM) 2))
                    pp
                  )
                )
               )
              PropertyList
            )
            (eval value)
            f
          )
      )
    )
  )
)

(defun c:SCP (/ m-lst value msg fun v-fun filter)
  (setq
    m-lst
      (list
        (cons 8 (list "\nLayer name : \"" "\" :" "u" '+))
        (cons 'ObjectName (list "\n  - ObjectName <" "> :" nil '+))
        (cons "COLOR" (list "\n   > Color " " : " "u" '+))
      )
    value   1
    msg     ""
    fun     '+
    v-fun   '<
    filter  (list '((0 . "*POLYLINE,LINE,ARC,CIRCLE")))
  )
  (setq lst (loop-a-list-properties (apply 'ssget filter) (mapcar 'car m-lst) value fun))
  (princ-a-list-properties
    lst
    msg
    (mapcar 'cdr m-lst)
    v-fun
  )
  (princ)
)