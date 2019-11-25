(vl-load-com)

(defun C:mm1 ( / ENT1 ENT2 LST1 LST2)
  (while (not ent1)
    (while (not (setq ent1 (car (nentsel "\nSelect source object: ")))))
    (if (not (assoc 1 (entget ent1))) (setq ent1 nil))
    )
  (setq lst1 (entget ent1))
  (while (setq ent2 (car (nentsel "\nSelect destination object: ")))
    (if (assoc 1 (entget ent2))
      (progn
	(setq lst2 (entget ent2))
	(setq lst2 (subst (assoc 1 lst1) (assoc 1 lst2) lst2))
	(entmod lst2)
	(entupd ent2)
	)
      )
    )
  (print)
  )

(defun C:mm2 ( / ENT1 ENT2 LST1 LST2)
  (while (not (setq lst1 (att_get (setq ent1 (ent_pick "INSERT" "\nSelect source object: "))))))
  (while (setq ent2 (car (entsel "\nSelect destination object: ")))
    (if (setq lst2 (att_get ent2))
      (foreach lst lst2
	(if (assoc (car lst) lst1)
	  (att_set ent2 (car lst) (cdr (assoc (car lst) lst1)))
	  )
	)
      )
    )
  (print)
  )

(defun C:mm3 ( / DYN ELST ENT LST LST1 LST2 LST_DYN STR)
  (while (not (setq lst_dyn (dyn_get (setq ent (ent_pick "INSERT" "\nSelect source block: "))))))
  (while (assoc "Origin" lst_dyn) (setq lst_dyn (vl-remove (assoc "Origin" lst_dyn) lst_dyn)))
  (setq lst1 (mapcar 'car lst_dyn))
  (setq lst2 (mapcar '(lambda (str) (while (vl-string-search " " str) (setq str (vl-string-subst "" " " str))) str) lst1))
  (setq dyn (keyword lst2 (car lst2) "Select dynamic"))
  (setq dyn (nth (vl-position dyn lst2) lst1))
  (setq elst (acet-ss-to-list (ssget (list (cons 0 "INSERT")))))
  (foreach ent elst
    (if (setq lst (dyn_get ent))
      (if (assoc dyn lst)
	(dyn_set ent dyn (cdr (assoc dyn lst_dyn)))
	)
      )
    )
  (print)
  )

(defun C:mm4 ( / DYN ELST ENT LST LST_DYN VAL)
  (while (not (setq lst_dyn (dyn_get (setq ent (ent_pick "INSERT" "\nSelect source block: "))))))
  (while (assoc "Origin" lst_dyn) (setq lst_dyn (vl-remove (assoc "Origin" lst_dyn) lst_dyn)))
  (setq elst (acet-ss-to-list (ssget (list (cons 0 "INSERT")))))
  (repeat 2
  (foreach ent elst
    (if (setq lst (dyn_get ent))
      (foreach lst1 lst
	(setq dyn (car lst1))
	(if (setq val (cdr (assoc dyn lst_dyn)))
	  (dyn_set ent dyn val)
	  )
	)
      )
    )
    )
  (print)
  )

(defun C:mm5 ( / ELST ENT OBJ SCALEX SCALEY SCALEZ)
  (setq ent (ent_pick "INSERT" "\nSelect source block: "))
  (setq obj (vlax-ename->vla-object ent))
  (setq scaleX (vla-get-XScaleFactor obj))
  (setq scaleY (vla-get-YScaleFactor obj))
  (setq scaleZ (vla-get-ZScaleFactor obj))
  (setq elst (acet-ss-to-list (ssget (list (cons 0 "INSERT")))))
  (foreach ent elst
    (setq obj (vlax-ename->vla-object ent))
    (vla-put-XScaleFactor obj scaleX)
    (vla-put-YScaleFactor obj scaleY)
    (vla-put-ZScaleFactor obj scaleZ)
    )
  (print)
  )

(defun C:mm6 ( / ELST ENT OBJ ROT)
  (setq ent (ent_pick "INSERT" "\nSelect source block: "))
  (setq obj (vlax-ename->vla-object ent))
  (setq rot (vla-get-Rotation obj))
  (setq elst (acet-ss-to-list (ssget (list (cons 0 "INSERT")))))
  (foreach ent elst
    (setq obj (vlax-ename->vla-object ent))
    (vla-Rotate obj (vla-get-InsertionPoint obj) (- rot (vla-get-Rotation obj)))
    )
  (print)
  )

;CHON DOI TUONG
(defun ent_pick (typ promp / ent)
  (if (not (listp typ)) (setq typ (list typ)))
  (setq typ (mapcar 'list typ))
  (while (not ent)
    (while (not (setq ent (car (entsel (strcat "\n" promp))))))
    (if (not (assoc (cdr (assoc 0 (entget ent))) typ)) (setq ent nil))
    )
  ent
  )

;THONG KE ATT
(defun att_get (ent)
  (if ent
    (mapcar '(lambda (att) (cons (vla-get-tagstring att) (vla-get-textstring att)))
	    (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)
	    )
    )
  )

;SUA ATT THEO TAG
(defun att_set (ent tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
       (if (= tag (strcase (vla-get-tagstring att)))
	 (progn (vla-put-textstring att val) val)
	 )
       )
    (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)
    )
  )

;THONG KE DYN
(defun dyn_get (ent)
  (mapcar '(lambda (dyn) (cons (vla-get-propertyname dyn) (vlax-get dyn 'value)))
	  (vlax-invoke (vlax-ename->vla-object ent) 'getdynamicblockproperties)
	  )
  )

;SUA DYN THEO PRP
(defun dyn_set (ent prp val)
  (setq prp (strcase prp))
  (vl-some
    '(lambda (x)
       (if (= prp (strcase (vla-get-propertyname x)))
	 (progn
	   (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
	   (cond (val) (t))
	   )
	 )
       )
    (vlax-invoke (vlax-ename->vla-object ent) 'getdynamicblockproperties)
    )
  )

;NHAP KEYWORD
(defun keyword (key default promp / str1 str2 str3 str4)
  (setq str1 (apply 'strcat (mapcar (function (lambda (x) (strcat x " "))) key)))
  (setq str2 (apply 'strcat (mapcar (function (lambda (x) (strcat x "/"))) key)))
  (setq str1 (substr str1 1 (1- (strlen str1))))
  (setq str2 (substr str2 1 (1- (strlen str2))))
  (initget str1)
  (setq str3 (strcat "\n" promp " [" str2 "] <" default "> "))
  (if (not (setq str4 (getkword str3)))
    default
    str4
    )
  )