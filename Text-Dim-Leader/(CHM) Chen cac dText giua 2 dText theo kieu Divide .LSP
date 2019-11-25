(vl-load-com)
(defun c:chm( / CAODO CAODO1 CAODO2 D D1 D2 DHZ DIEM1 DIEM2 DZ12 GOCDT 
	        I ITEM1 ITEM2 PT_I  TDO1 TDO2
	        TEMP1 TEMP2 X1 X2 X3 Y1 Y2 Y3 Z1 Z2 Z3)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))
(setq i 1)
(_layer2 "TNS" 2)

(or *h* (setq *h* 1))
(setq h (getreal (strcat "\nNh\U+1EADp chi\U+1EC1u cao Text <"
			  (rtos *h* 2 2)
			 ">: "
		  )
	 )
)
(if (not h) (setq h *h*) (setq *h* h))


(if (not sle)
    (setq sle 2)
)
(setq sle0 (getint (strcat "\nS\U+1ED1 l\U+1EBB sau ph\U+1EA7n th\U+1EADp ph\U+00E2n (T\U+1ED1i \U+0111a = 10, t\U+1ED1i thi\U+1EC3u = 0) <" (itoa sle) ">: ")))
(if sle0
   (progn
	(if (< sle0 0)(setq sle0 0))
	(if (> sle0 10)(setq sle0 10))
        (setq sle sle0)
   )
)

(progn
	    (setq item1 (entsel "\nCh\U+1ECDn Text th\U+1EE9 nh\U+1EA5t: "))
	    (setq temp1  (entget (car item1)))
	    (setq Tdo1 (TD:Text-Base (car item1 )))
	    (setq  Caodo1 (cdr (assoc 1 temp1))
	              x1 (car Tdo1)
	              y1 (cadr Tdo1)
	    )
	    (setq  z1 (atof Caodo1))
	    (setq diem1 (list x1 y1))
	    

	    (setq item2 (entsel "\nCh\U+1ECDn Text th\U+1EE9 hai: "))
	    (setq temp2  (entget (car item2)))
	    (setq Tdo2 (TD:Text-Base (car item2 )))
	    (setq  Caodo2 (cdr (assoc 1 temp2))
	              x2 (car Tdo2)
	              y2 (cadr Tdo2)
	    )
	    (setq z2 (atof Caodo2))
	    (setq diem2 (list x2 y2 ))
	    
	)
(setq d (distance diem1 diem2))
(or *n* (setq *n* 1))
(setq n (getreal (strcat "\nNh\U+1EADp s\U+1ED1 \U+0111i\U+1EC3m c\U+1EA7n ch\U+00E8n <"
			  (rtos *n* 2 0)
			 ">: "
		  )
	 )
)
(if (not n) (setq n *n*) (setq *n* n))


(setq kcl (/ d (+ n 1)))
(setq gocdt (angle diem1 diem2))
	(while  (<= i n)
	    (setvar "OSMODE" 0)
	    (setq pt_i (polar diem1 gocdt (* i kcl)))
	    (setq x3 (car pt_i))
	    (setq y3 (cadr pt_i))
	    (setq d1 (distance diem1 pt_i))
	    (setq d2 (distance diem2 pt_i))
	    (setq kcdai (+ d1 d2))
	    (setq dz12 (- z2 z1))
	    (setq dhz (* dz12 (/ d1 kcdai)))
	    (setq z3 (+ z1 dhz))
	    (setq Caodo (rtos z3 2 sle))
	    (setq pt_i (list x3 y3 z3))
	    (MakeText pt_i Caodo h 0 "L" "TNS")
	    (setq i (1+ i))
	)
(setvar "OSMODE" Olmode)
(princ)
)


(defun c:chd( /  Olmode  h item1  temp1 Tdo1 x1 y1 z1 diem1 item2  temp2 Tdo2 Caodo2 x2 y2 z2 diem2 x3 y3 z3  d n kcl pt_i kcdai dz12 d1 d2  dhz)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))
(setq i 1)
(_layer2 "TNS" 2)

(if (not sle)
    (setq sle 2)
)
(setq sle0 (getint (strcat "\nS\U+1ED1 l\U+1EBB sau ph\U+1EA7n th\U+1EADp ph\U+00E2n (T\U+1ED1i \U+0111a = 10, t\U+1ED1i thi\U+1EC3u = 0) <" (itoa sle) ">: ")))
(if sle0
   (progn
	(if (< sle0 0)(setq sle0 0))
	(if (> sle0 10)(setq sle0 10))
        (setq sle sle0)
   )
)


(or *h* (setq *h* 1))
(setq h (getreal (strcat "\nNh\U+1EADp chi\U+1EC1u cao Text <"
				  (rtos *h* 2 2)
				 ">: "
			  )
		)
)
  (if (not h) (setq h *h*) (setq *h* h))
	(progn
	    (setq item1 (entsel "\nCh\U+1ECDn Text th\U+1EE9 nh\U+1EA5t: "))
	    (setq temp1  (entget (car item1)))
	    (setq Tdo1 (TD:Text-Base (car item1 )))
	    (setq  Caodo1 (cdr (assoc 1 temp1))
	              x1 (car Tdo1)
	              y1 (cadr Tdo1)
	    )
	    (setq  z1 (atof Caodo1))
	    (setq pt1 (list x1 y1 ))
	    

	    (setq item2 (entsel "\nCh\U+1ECDn Text th\U+1EE9 hai: "))
	    (setq temp2  (entget (car item2)))
	    (setq Tdo2 (TD:Text-Base (car item2 )))
	    (setq  Caodo2 (cdr (assoc 1 temp2))
	              x2 (car Tdo2)
	              y2 (cadr Tdo2)
	    )
	    (setq z2 (atof Caodo2))
	    (setq pt2 (list x2 y2 ))
	    
	)
	(setq Diem1 (list x1 y1))
	(setq Diem2 (list x2 y2))
	(setq d (distance diem1 diem2))
  	(or *kcl* (setq *kcl* 0))
	(setq kcl (getreal (strcat "\nNh\U+1EADp kho\U+1EA3ng c\U+00E1ch gi\U+1EEFa c\U+00E1c \U+0111i\U+1EC3m: <"
				  (rtos *kcl* 2 2)
				 ">: "
			  )
		 )
	)
	(if (not kcl) (setq kcl *kcl*) (setq *kcl* kcl))

  
	(setq n (/ d kcl))
	(setq gocdt (angle diem1 diem2))

	(while  (< i n)
	    (setvar "OSMODE" 0)
	    (setq pt_i (polar diem1 gocdt (* i kcl)))
	    (setq x3 (car pt_i))
	    (setq y3 (cadr pt_i))
	    (setq d1 (distance diem1 pt_i))
	    (setq d2 (distance diem2 pt_i))
	    (setq kcdai (+ d1 d2))
	    (setq dz12 (- z2 z1))
	    (setq dhz (* dz12 (/ d1 kcdai)))
	    (setq z3 (+ z1 dhz))
	    (setq Caodo (rtos z3 2 sle))
	    (setq pt_i (list x3 y3 z3))
	    (MakeText pt_i Caodo h 0 "L" "TNS")
	    (setq i (1+ i))
	)
(setvar "OSMODE" Olmode)
(princ)
)

(defun c:CLT (/ CAODO1 CAODO2 CAODO3 D D1 D2 DH DHZ ITEM1 ITEM2 OLMODE PT1 PT2 PT3 
	        STT TDO1 TDO2 TEMP1 TEMP2 X1 X2 X3 Y1 Y2 Y3 Z1 Z2 Z3 LOOP) ;chen lien tiep tu 2 diem
	(defun *error* ( msg )
		(if Olmode (setvar 'osmode Olmode))
		(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
		    (princ (strcat "\nError: " msg))
		)
		(princ)
	)
  	
  	
	(or *h* (setq *h* 1))
	(setq h (getreal (strcat "\nNh\U+1EADp chi\U+1EC1u cao Text <"
			  (rtos *h* 2 2)
			 ">: "
		  )
	 )
	)
	(if (not h) (setq h *h*) (setq *h* h))
	(_layer2 "Docao1" 2)
	(setq Olmode (getvar "OSMODE"))
	(if (not sle)
	(setq sle 2)
	)
	(setq sle0 (getint (strcat "\nS\U+1ED1 l\U+1EBB sau ph\U+1EA7n th\U+1EADp ph\U+00E2n (T\U+1ED1i \U+0111a = 10, t\U+1ED1i thi\U+1EC3u = 0) <" (itoa sle) ">: ")))
	(if sle0
	   (progn
		(if (< sle0 0)(setq sle0 0))
		(if (> sle0 10)(setq sle0 10))
	        (setq sle sle0)
	   )
	)
      
(if (and (setq item1 (entsel "\nCh\U+1ECDn Text th\U+1EE9 nh\U+1EA5t: "))
	 (setq item2 (entsel "\nCh\U+1ECDn Text th\U+1EE9 hai "))
    )
    (progn
	  (progn
	            
	  	    (setq temp1  (entget (car item1)))
		    (setq Tdo1 (TD:Text-Base (car item1 )))
		    (setq  Caodo1 (cdr (assoc 1 temp1))
		              x1 (car Tdo1)
		              y1 (cadr Tdo1)
	            )
		    (setq pt1 (list x1 y1))
	            (setq  z1 (atof Caodo1))
	            
	  	    (setq temp2  (entget (car item2)))
		    (setq Tdo2 (TD:Text-Base (car item2 )))
		    (setq  Caodo2 (cdr (assoc 1 temp2))
		              x2 (car Tdo2)
		              y2 (cadr Tdo2)
	            )
	            (setq pt2 (list x2 y2))
	            (setq z2 (atof Caodo2))
	    )
	(setq loop T)
	(while loop
		(setq pt3 (getpoint "\nV\U+1ECB tr\U+00ED ch\U+00E8n : "))
	  	(cond
		  (T
			(if pt3
				(progn
					(setq x3 (car pt3))
					(setq y3 (cadr pt3))
					(setq d1 (distance pt1 pt3))
					(setq d2 (distance pt2 pt3))
					(setq d (+ d1 d2))
					(setq dh (- z2 z1))
					(setq dhz (* dh (/ d1 d)))
					(setq z3 (+ z1 dhz))
					(setq Caodo3 (rtos z3 2 sle))
					(setq pt3 (list x3 y3 z3))
					(MakeText pt3 Caodo3 h 0 "L" "Docao1")
				)
			  (setq loop nil)
			)
		  )
		)
	  	
	)
      )
)
(setvar "OSMODE" Olmode )
(princ)
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

(defun _layer2 ( name colour )
    (if (null (tblsearch "LAYER" name))
        (entmake
            (list
               '(0 . "LAYER")
               '(100 . "AcDbSymbolTableRecord")
               '(100 . "AcDbLayerTableRecord")
               '(70 . 0)
                (cons 2 name)
                (cons 62 colour)
            )
        )
    )
)


(defun MakeText (point string Height Ang justify  Layer  / Lst); Ang: Radial
	(setq Lst (list '(0 . "TEXT")
									(cons 10 point)
									(cons 40 Height)
									(cons 1 string)
								        (cons 50 Ang)
									(cons 8 Layer)
			)
				justify (strcase justify))
	(cond ((= justify "C") (setq Lst (append Lst (list (cons 72 1) (cons 11 point)))))
	      			((= justify "L") (setq Lst (append Lst (list (cons 72 0)(cons 73 0) (cons 10 point)))))
				((= justify "R") (setq Lst (append Lst (list (cons 72 2) (cons 11 point)))))
				((= justify "M") (setq Lst (append Lst (list (cons 72 4) (cons 11 point)))))
				((= justify "TL") (setq Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 3)))))
				((= justify "TC") (setq Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 3)))))
				((= justify "TR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 3)))))	
				((= justify "ML") (setq Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 2)))))
				((= justify "MC") (setq Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 2)))))
				((= justify "MR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 2)))))
				((= justify "BL") (setq Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 1)))))
				((= justify "BC") (setq Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 1)))))
				((= justify "BR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 1))))))
	(entmakex Lst)
  )