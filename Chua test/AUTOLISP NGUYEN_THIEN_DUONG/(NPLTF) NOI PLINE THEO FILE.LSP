;;;(Alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh n\U+1ED1i \U+0111\U+01B0\U+1EDDng theo t\U+00EAn l\U+01B0u trong file d\U+1EEF li\U+1EC7u"
;;;		"\nL\U+1EC7nh: NTF"
;;;		"\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;		"\nEmail: heaven2407@gmail.com"
;;;		"\nMobile: 0972.0168.25"
;;;	)
;;;)
;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh n\U+1ED1i \U+0111\U+01B0\U+1EDDng theo t\U+00EAn l\U+01B0u trong file d\U+1EEF li\U+1EC7u"
;;;		"\nL\U+1EC7nh: NTF"
;;;		"\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;		"\nEmail: heaven2407@gmail.com"
;;;		"\nMobile: 0972.0168.25"
;;;	)
;;;)

(defun replace_str(str)
  (setq rs (acet-str-replace "," " " str)
	rs (acet-str-replace "	" " " rs)
  )
  (split_space rs)
)
(defun split_space (str)(vl-remove-if '(lambda (x) (= x ""))(acet-str-to-list " " str)))

(defun c:NPLTF (/ LtsT L1a L2a Name Pnt L2b Lt2 ten f Line   data i Ptu Lt1 LtsPoint DsLoc ) ;NOI THEO FILE
(vl-load-com)
(setvar "CMDECHO" 0)
(setq olmode (getvar "OSMODE"))
(setvar "OSMODE" 0)
(Alert "Qu\U+00E9t ch\U+1ECDn t\U+1EADp t\U+00EAn \U+0111i\U+1EC3m")
(setq LtsT (acet-ss-to-list (ssget (list (cons 0 "TEXT")))))
(setq L1a (list))
(setq L1b (list))
(setq L1c (list))
(setq L2a (list))
(foreach e1 LtsT
	(setq Name (cdr (assoc 1 (entget e1))))
  	(setq Pnt  (TD:Text-Base e1))
  	(setq L1a (list Name  Pnt))
  	(setq L1b (append L1b (list Name)))
  	(setq L2a (append L2a (list L1a)))
)
(setq L1c (FilterName0 L1b))
(setq L2b (FilterName L2a))
(setq Lt2 (list))
(if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))	
	(if (wcmatch Line (strcat "*"(chr 9)"*,*"(chr 32)"*,*"(chr 44)"*"))
	  (progn
	    (setq data (replace_str Line)) 
	    (progn
		(setq i 0)
		(setq Lt1 (list))
		(while
			(setq Ptu  (nth i data))
			(setq Lt1 (append Lt1  (list Ptu)))
			(setq i (1+ i))
		)
	    )
	    (setq Lt2 (append Lt2 (list Lt1)))
	  )
	 )
	)
      )
)
(setq LtsPoint (list))
(foreach e2 Lt2
  	(setq LtsPoint (mapcar '(lambda(x)(GetName x L2b)) e2))
  	(MakeLWPolyline (vl-remove nil LtsPoint)  nil nil nil nil)
)
(if  (setq LtsPntNo(TD:NameDifference (FilterName0 (LM:ListsUnion  Lt2)) L1b))
     (progn
        (setq DsLoc  (LM:lst->str LtsPntNo ","))
     	(Alert (strcat "\nNh\U+1EEFng \U+0111i\U+1EC3m:   " DsLoc " c\U+00F3 trong file nh\U+01B0ng kh\U+00F4ng c\U+00F3 tr\U+00EAn b\U+1EA3n v\U+1EBD"))
      )
)
(if  (setq LtsPntNo(TD:NameDifference (FilterName0 (LM:ListsUnion  Lt2)) L1b))
     (progn
        (setq DsLoc  (LM:lst->str LtsPntNo ","))
     	(progn
	        (setq DsLoc  (LM:lst->str LtsPntNo ","))
	     	(Prompt (strcat "\nNh\U+1EEFng \U+0111i\U+1EC3m:   " DsLoc " c\U+00F3 trong file nh\U+01B0ng kh\U+00F4ng c\U+00F3 tr\U+00EAn b\U+1EA3n v\U+1EBD"))
      	)
      )
)
(setvar "OSMODE" olmode)

(princ)
)







(defun GetName (Tenss LtsNPoint / i e Loop Pnt)
(setq i 0)
(setq Loop nil)
(while (< i (length LtsNPoint))
	(setq e (nth i LtsNPoint))
	(if (= (car e)  Tenss)
	    (progn
		(setq Pnt (nth 1 e))
		(setq Loop T)
	    )
	)
  	(setq i (1+ i))
)
Pnt
)
(defun FilterName ( l)
    (if l
      (cons (car l)
        (FilterName
          (vl-remove-if '(lambda ( x ) (equal (car x) (car (car l)) )) (cdr l)) 
        )
      )
    )
)
(defun FilterName0 ( l)
    (if l
      (cons (car l)
        (FilterName0
          (vl-remove-if '(lambda ( x ) (equal x (car l) )) (cdr l)) 
        )
      )
    )
)



(defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
)
(defun LM:ListsUnion ( l / x u )
  (setq l (apply 'append l))
  (while (setq x (car l)) (setq u (cons x u) l (vl-remove x l)))
  (reverse u)
)



(defun MakeLWPolyline (listpoint  Linetype LTScale Layer Color  / Lst)
(setq Lst (list (cons 0  "LWPOLYLINE")
		(cons 100  "AcDbEntity")
		(cons 8 (if Layer Layer (getvar "Clayer")))
	        (cons 6 (if Linetype Linetype "bylayer"))
		(cons 48 (if LTScale LTScale 1))
		(cons 62 (if Color Color 256))
		(cons 100  "AcDbPolyline")
		(cons 90 (length listpoint))
	 )
)
(foreach PP listpoint(setq Lst (append Lst (list (cons 10 PP)))))
(entmakex Lst)
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


;;;;;;;;;(TD:NameDifference (setq l1 (list 1 2 3 4 444 5 7)) (setq l2 (list 2 4 6 7 8 8 9 888 999 999 9 7 5)))
(defun TD:NameDifference ( l1 l2 )
  (if l1
    (if (member (car l1) l2)
      (TD:NameDifference (cdr l1) l2)
      (cons (car l1) (TD:NameDifference (cdr l1) l2))
    )
  )
)