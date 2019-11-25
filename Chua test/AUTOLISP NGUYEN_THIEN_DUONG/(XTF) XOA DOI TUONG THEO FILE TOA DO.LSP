;;;(Alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh x\U+00F3a \U+0111\U+1ED1i t\U+01B0\U+1EE3ng theo file t\U+1ECDa \U+0111\U+1ED9 (STT X Y .....) "
;;;               "\n"
;;;	       "\nL\U+1EC7nh s\U+1EED d\U+1EE5ng: XTF"
;;;               "\n"
;;;               "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;               "\n"
;;;               "\nMobile: 0972.0168.25"
;;;               "\n"
;;;	)	
;;;)
;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh x\U+00F3a \U+0111\U+1ED1i t\U+01B0\U+1EE3ng theo file t\U+1ECDa \U+0111\U+1ED9 (STT X Y .....) "
;;;               "\n"
;;;	       "\nL\U+1EC7nh s\U+1EED d\U+1EE5ng: XTF"
;;;               "\n"
;;;               "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;               "\n"
;;;               "\nMobile: 0972.0168.25"
;;;               "\n"
;;;	)	
;;;)
;;;(vl-load-com)
(defun replace_str(str)
  (setq rs (acet-str-replace "," " " str)
	rs (acet-str-replace "	" " " rs)
  )
  (split_space rs)
)
(defun split_space (str)(vl-remove-if '(lambda (x) (= x ""))(acet-str-to-list " " str)))
(defun c:XTF ( / Obj LtsEnameText LtsEnamePnt  L1 L2  X1 Y1 Ent NameBlock ) ;;;Xoa diem theo file
(vl-load-com)
(setq Ent (entsel "\nCh\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng m\U+1EABu \U+0111\U+1EC3 x\U+00F3a (Point, Text hay Block)) " ))
(setq Obj (cdr (assoc 0 (entget (car Ent)))))
(cond
  ((= Obj "TEXT")
   (progn
	(Alert (strcat  "\n\U+0110\U+1ED1i t\U+01B0\U+1EE3ng c\U+1EA7n qu\U+00E9t ch\U+1ECDn v\U+00E0 s\U+1EBD b\U+1ECB x\U+00F3a l\U+00E0 \U+0111\U+1ED1i t\U+01B0\U+1EE3ng : " Obj ))
	(setq LtsEname (acet-ss-to-list (ssget (list (cons 0 "TEXT")))))
	(setq LtsEnamePnt (mapcar '(lambda (x)  (cons x (list (list (car (TD:Text-Base x)) (cadr (TD:Text-Base x)))))) LtsEname))
   )
   )
   ((= Obj "POINT")
   (progn
	(Alert (strcat  "\n\U+0110\U+1ED1i t\U+01B0\U+1EE3ng c\U+1EA7n qu\U+00E9t ch\U+1ECDn v\U+00E0 s\U+1EBD b\U+1ECB x\U+00F3a l\U+00E0 \U+0111\U+1ED1i t\U+01B0\U+1EE3ng : " Obj ))
	(setq LtsEname (acet-ss-to-list (ssget (list (cons 0 "POINT")))))
	(setq LtsEnamePnt (mapcar '(lambda (x)  (cons x (list (list (car (cdr (assoc 10 (entget x)))) (cadr (cdr (assoc 10 (entget x)))))))) LtsEname))
   )
   )
   ((= Obj "INSERT")
   (progn
     	(setq NameBlock (cdr (assoc 2 (entget (car Ent)))))
	(Alert (strcat  "\n\U+0110\U+1ED1i t\U+01B0\U+1EE3ng c\U+1EA7n qu\U+00E9t ch\U+1ECDn v\U+00E0 s\U+1EBD b\U+1ECB x\U+00F3a l\U+00E0 \U+0111\U+1ED1i t\U+01B0\U+1EE3ng :  BLOCK "
			"\nT\U+00EAn l\U+00E0: " NameBlock
	       )
	)
	(setq LtsEname (acet-ss-to-list (ssget (list (cons 0 "INSERT")(cons 2 NameBlock)))))
	(setq LtsEnamePnt (mapcar '(lambda (x)  (cons x (list (list (car  (vlax-get (vlax-ename->vla-object x) 'InsertionPoint)) (cadr (vlax-get (vlax-ename->vla-object x) 'InsertionPoint)))))) LtsEname))
   )
   )
)
(or *BKX* (setq *BKX* 0.001))
(setq BKX (getreal (strcat "\nNh\U+1EADp kho\U+1EA3ng c\U+00E1ch Delta ch\U+00EAnh l\U+1EC7ch \U+0111\U+1EC3 t\U+00ECm x\U+00F3a: <"
		  (rtos *BKX* 2  3)
		 "> :"
	  )
 )
)
(if (not BKX) (setq BKX *BKX*) (setq *BKX* BKX))
  
(setq L1 (list))
(setq L2 (list))
  
(if (setq ten (getfiled "Chon File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))	
	(if (wcmatch Line (strcat "*"(chr 9)"*,*"(chr 32)"*,*"(chr 44)"*"))
	  (progn
	    (setq data (replace_str Line))
	    (setq X1  (atof (nth 1 data)))
	    (setq Y1  (atof (nth 2 data)))
	    (setq L1 (list Y1 X1))
	    (setq L2 (append L2 (list L1)))
	  )
	 )
	)
      )
)
(foreach Pnt_i L2
	 (mapcar '(lambda (x) (if (equal (cadr x) Pnt_i  BKX) (entdel (car x)))) LtsEnamePnt)
)
  
(Alert "\nXong!")
(princ L2)
(PRINC)
)

(defun TD:Text-Base (ent)
(setq Ma10  (cdr (assoc 10 (entget ent))))
(setq Ma11  (cdr (assoc 11 (entget ent))))
(setq X11 (car Ma11))
(setq Ma71  (cdr (assoc 71 (entget ent))))
(setq Ma72  (cdr (assoc 72 (entget ent))))
(if (or (and (= Ma71 0) (= Ma72 0) (= X11 0))
  (and (= Ma71 0) (= Ma72 3) )
  (and (= Ma71 0) (= Ma72 5) )
)
Ma10
Ma11
)
)