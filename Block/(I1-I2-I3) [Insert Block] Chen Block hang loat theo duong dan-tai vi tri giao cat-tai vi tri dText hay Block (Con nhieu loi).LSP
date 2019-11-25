(alert (strcat "LISP Insert Block"
	       "\nNguoi viet: 3Duy"
	       "\nLenh thuc hien:"
	       "\n        I1 - Insert theo dinh Pline"
	       "\n        I2 - Insert theo giao cat Pline"
	       "\n        I3 - Insert theo Block, Text"
	       )
)

;Ten lenh
(defun C:ii ()
  (alert (strcat "LISP Insert Block"
	       "\nNguoi viet: 3Duy"
	       "\nLenh thuc hien:"
	       "\n        I1 - Insert theo dinh Pline"
	       "\n        I2 - Insert theo giao cat Pline"
	       "\n        I3 - Insert theo Block, Text"
	       )
)
  )

(vl-load-com)

;DINH VI THEO DINH PLINE
(defun C:i1 ( / ANG ELST ENT I LAYER LST LST_PT NAME OBJ PT1 PT2 ROT SC)
  (DISPLAY-OFF)

  (setq elst (acet-ss-to-list (ssget (list (cons 0 "LINE,*POLYLINE")))))

  (setq ent (ent_pick "INSERT" "\nChon Block: "))
  (setq str1 (get_key (list "Dinh" "Trungdiem" "dAu" "cUoi") "Dinh" "Rai Block tai vi tri"))
  (setq str2 (get_key (list "Yes" "No") "No" "Xoay Block theo duong dan"))
  (setq rot (get_real 0 "Goc xoay cong them"))
  
  (setq lst (entget ent))
  (setq name (vla-get-effectivename (vlax-ename->vla-object ent)))
  (setq layer (cdr (assoc 8 lst)))
  (setq sc (cdr (assoc 41 lst)))

  (foreach ent elst
    (setq obj (vlax-ename->vla-object ent))
    (setq lst_pt (get_vertex ent))
    (if (= str1 "Dinh")
      (progn
	(setq i 0)
	(repeat (length lst_pt)
	  (setq pt1 (nth i lst_pt))
	  (if (< i (1- (length lst_pt)))
	    (setq pt2 (nth (1+ i) lst_pt)
		  ang (angle pt1 pt2)
		  )
	    (setq pt2 (nth (1- i) lst_pt)
		  ang (angle pt2 pt1)
		  )
	    )
	  (if (= str2 "No") (setq ang 0))
	  (insert_att name pt1 layer sc (+ ang rot) nil nil)
	  (setq i (1+ i))
	  )
	)
      )
    (if (= str1 "Trungdiem")
      (progn
	(setq i 0)
	(repeat (length lst_pt)
	  (setq pt1 (nth i lst_pt))
	  (if (< i (1- (length lst_pt)))
	    (setq pt2 (nth (1+ i) lst_pt)
		  ang (angle pt1 pt2)
		  )
	    (setq pt2 (nth (1- i) lst_pt)
		  ang (angle pt2 pt1)
		  )
	    )
	  (if (= str2 "No") (setq ang 0))
	  (setq pt3 (list (/ (+ (car pt1) (car pt2)) 2) (/ (+ (cadr pt1) (cadr pt2)) 2)))
	  (insert_att name pt3 layer sc (+ ang rot) nil nil)
	  (setq i (1+ i))
	  )
	)
      )
    (if (= str1 "dAu")
      (progn
	(setq pt1 (car lst_pt))
	(setq pt2 (cadr lst_pt))
	(setq ang (angle pt1 pt2))
	(if (= str2 "No") (setq ang 0))
	(insert_att name pt1 layer sc (+ ang rot) nil nil)
	)
      )
    (if (= str1 "cUoi")
      (progn
	(setq pt1 (last lst_pt))
	(setq pt2 (cadr (reverse lst_pt)))
	(setq ang (angle pt2 pt1))
	(if (= str2 "No") (setq ang 0))
	(insert_att name pt1 layer sc (+ ang rot) nil nil)
	)
      )
    )
  
  (DISPLAY-ON)
  (print)
  )

;DINH VI THEO GIAO CAT PLINE
(defun C:i2 ( / ELST ENT ENT1 LAYER LST LST_PT NAME PT SC X)
  (DISPLAY-OFF)

  (setq elst (acet-ss-to-list (ssget (list (cons 0 "LINE,*POLYLINE")))))

  (setq ent (ent_pick "INSERT" "\nChon Block: "))
  (setq lst (entget ent))
  (setq name (vla-get-effectivename (vlax-ename->vla-object ent)))
  (setq layer (cdr (assoc 8 lst)))
  (setq sc (cdr (assoc 41 lst)))

  (setq lst_pt nil)
  (while (> (length elst) 1)
    (setq ent1 (car elst))
    (setq lst (apply 'append (mapcar '(lambda (x) (vla-inters ent1 x 0)) (cdr elst))))
    (setq lst_pt (append lst_pt lst))
    (setq elst (cdr elst))
    )

  (setq lst_pt (unique_fuzz lst_pt (get_real sc "Fuzz")))

  (foreach pt lst_pt (insert_att name pt layer sc 0 nil nil))
  
  (DISPLAY-ON)
  (print)
  )

;DINH VI THEO BLOCK
(defun C:i3 ( / ELST ENT LAYER LST NAME PT ROT1 ROT2 SC STR TAG)
  (DISPLAY-OFF)

  (setq elst (acet-ss-to-list (ssget (list (cons 0 "INSERT,TEXT")))))

  (setq ent (ent_pick "INSERT" "\nChon Block: "))
  (setq lst (entget ent))
  (setq name (vla-get-effectivename (vlax-ename->vla-object ent)))
  (setq layer (cdr (assoc 8 lst)))
  (setq sc (cdr (assoc 41 lst)))

  (if (= (cdr (assoc 0 (entget (car elst)))) "TEXT") (setq tag (att_pick ent "Chon Tag")))

  (foreach ent1 elst
    (setq lst (entget ent1))
    (if (and (assoc 11 lst) (/= (assoc 11 lst) '(0.0 0.0 0.0)))
      (setq pt (cdr (assoc 11 lst)))
      (setq pt (cdr (assoc 10 lst)))
      )
    (setq str (cdr (assoc 1 lst)))
    (setq rot1 (cdr (assoc 50 lst)))
    (setq rot2 nil)
    (if (= (cdr (assoc 0 lst)) "INSERT")
      (if (assoc "Angle1" (dyn_get ent1))
	(if (assoc "Angle1" (dyn_get ent))
	  (setq rot2 (list (assoc "Angle1" (dyn_get ent1))))
	  )
	)
      )
    (insert_att name pt layer sc rot1 (if str (list (cons tag str))) rot2)
    )

  (DISPLAY-ON)
  (print)
  )

;LIST VAR
(setq 3DUY_SYSTEM_VARIABLES_DISPLAY_NAME '("CMDECHO" "DIMZIN" "ATTDIA" "ATTREQ"))
(setq 3DUY_SYSTEM_VARIABLES_SNAP_NAME '("OSMODE" "ORTHOMODE" "SNAPMODE"))
(setq 3DUY_SYSTEM_VARIABLES_DISPLAY_CURRENT (mapcar 'getvar 3DUY_SYSTEM_VARIABLES_DISPLAY_NAME))
(setq 3DUY_SYSTEM_VARIABLES_SNAP_CURRENT (mapcar 'getvar 3DUY_SYSTEM_VARIABLES_SNAP_NAME))

;DISPLAY-OFF
(defun DISPLAY-OFF ()
  (setq 3DUY_SYSTEM_VARIABLES_DISPLAY_CURRENT (mapcar 'getvar 3DUY_SYSTEM_VARIABLES_DISPLAY_NAME))
  (mapcar '(lambda (X) (setvar X 0)) 3DUY_SYSTEM_VARIABLES_DISPLAY_NAME)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  )

;DISPLAY-ON
(defun DISPLAY-ON ()
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (mapcar 'setvar 3DUY_SYSTEM_VARIABLES_DISPLAY_NAME 3DUY_SYSTEM_VARIABLES_DISPLAY_CURRENT)
  )

;ERROR
(defun *error* (msg)
  (print msg)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (mapcar 'setvar 3DUY_SYSTEM_VARIABLES_DISPLAY_NAME 3DUY_SYSTEM_VARIABLES_DISPLAY_CURRENT)
  (mapcar 'setvar 3DUY_SYSTEM_VARIABLES_SNAP_NAME 3DUY_SYSTEM_VARIABLES_SNAP_CURRENT)
  (print)
  )

;BLOCK - ATT, DYN
(defun insert_att (block pt layer scale rotation att dyn / acdoc acspc cur_layer ent)
  (if (tblsearch "BLOCK" block)
    (progn
      (setq cur_layer (getvar "CLAYER"))
      (setvar "CLAYER" layer)
      (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq acspc (vlax-get-property acdoc (if (= (getvar "CVPORT") 1) 'paperspace 'modelspace)))
      (vla-InsertBlock acspc (vlax-3d-point (car pt) (cadr pt) (caddr pt)) block scale scale scale rotation)
      (setq ent (entlast))
      (setvar "CLAYER" cur_layer)
      (att_lst ent att)
      (dyn_lst ent dyn)
      ent
      )
    )
  )

;THONG KE ATT
(defun att_get (ent)
    (mapcar '(lambda (att) (cons (vla-get-tagstring att) (vla-get-textstring att)))
	    (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)
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

;SUA ATT THEO LIST
(defun att_lst (ent lst / itm)
  (foreach att (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)
    (if (setq itm (assoc (vla-get-tagstring att) lst))
      (vla-put-textstring att (cdr itm))
      )
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
    '(lambda ( x )
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

;SUA DYN THEO LIST
(defun dyn_lst (ent lst / itm)
  (setq lst (mapcar '(lambda (x) (cons (strcase (car x)) (cdr x))) lst))
  (foreach x (vlax-invoke (vlax-ename->vla-object ent) 'getdynamicblockproperties)
    (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
      (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
      )
    )
  )

;UNIQUE-FUZZ
(defun unique_fuzz (lst fuzz / x lst1)
  (while lst
    (setq x (car lst)
	  lst (vl-remove-if (function (lambda (y) (equal x y fuzz))) (cdr lst))
	  lst1 (cons x lst1)
	  )
    )
  (reverse lst1)
  )

;GET VERTEX
(defun get_vertex (ent / i lst)
  (setq i 0)
  (repeat (fix (1+ (vlax-curve-getEndParam ent)))
    (setq lst (append lst (list (vlax-curve-getPointAtParam ent i))))
    (setq i (1+ i))
    )
  lst
  )

;GIAO CAT
(defun vla-inters (ent1 ent2 mode / lst1 lst2)
  (setq lst1 (vlax-invoke (vlax-ename->vla-object ent1) 'intersectwith (vlax-ename->vla-object ent2)
	       (cond
		 ((= mode 0) acextendnone)
		 ((= mode 1) acextendthisentity)
		 ((= mode 2) acextendotherentity)
		 ((= mode 3) acextendboth)
		 )))
  (repeat (/ (length lst1) 3)
    (setq lst2 (cons (list (car lst1) (cadr lst1) (caddr lst1)) lst2)
	  lst1 (cdddr lst1)
	  )
    )
  (reverse lst2)
  )

;NHAP SO THUC
(defun get_real (default promp / str)
  (while (not str)
    (setq str (getstring (strcat "\n" promp " <" (rtos (float default) 2 (getvar "LUPREC")) "> ")))
    (if (= (substr str 1 1) ".") (setq str (strcat "0" str)))
    (setq str (cond
		((= str "") (float default))
		((numberp (read str)) (atof str))
		(t nil)
		))
    )
  )

;NHAP KEYWORD
(defun get_key (key default promp / key_fix str1 str2 str3 str4)
  (setq key_fix key)
  (foreach str1 (list " " "_")
    (setq key_fix (mapcar '(lambda (str) (while (vl-string-search str1 str) (setq str (vl-string-subst "" str1 str))) str) key_fix))
    )
  (setq str1 (apply 'strcat (mapcar (function (lambda (x) (strcat x " "))) key_fix)))
  (setq str2 (apply 'strcat (mapcar (function (lambda (x) (strcat x "/"))) key_fix)))
  (setq str1 (substr str1 1 (1- (strlen str1))))
  (setq str2 (substr str2 1 (1- (strlen str2))))
  (if (not (assoc default (mapcar 'list key_fix))) (setq default (car key_fix)))
  (initget str1)
  (setq str3 (strcat "\n" promp " [" str2 "] <" default "> "))
  (if (not (setq str4 (getkword str3)))
    (nth (vl-position default key_fix) key)
    (nth (vl-position str4 key_fix) key)
    )
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