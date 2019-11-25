(defun c:MAICP (/ oldos Olmode kch kcc TenBlock Obj name pt ); Multi Array in Close Pline
(vl-load-com)
(defun *error* ( msg )
		(if Olmode (setvar 'osmode Olmode))
		(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
		    (princ (strcat "\nError: " msg))
		)
		(princ)
)
(setvar "CMDECHO" 0)
(setq Olmode (getvar "osmode"));;;LUU CHE DO BAT DIEM OSNAP
(setvar "hpgaptol" 0.5);;;;;NHAP KHOANG CACH HO CHO PHEP TAO BOUDARY (VI DU 0.5)
(setvar "OSMODE" 0);;;;CHO CHE DO BAT DIEM VE KHONG
(setq i 0)
;;;NHAP TEN BLOCK HOAC ENTER DE CHON BLOCK
(setq TenBlock (getstring "\n Nh\U+1EADp t\U+00EAn Block / Enter \U+0111\U+1EC3 ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng:  "))
;;;NEU TEN BLOCK = KHOANG TRONG THI CHON BLOCK
(if (= TenBlock "") (setq TenBlock (cdr (assoc 2 (entget (car (entsel "\n Chon Block :")))))))
(or *kch* (setq *kch* 1))
;;;NHAP KHOANG CACH GIUA CAC BLOCK
(setq kch (getdist (strcat "\n Nh\U+1EADp kho\U+1EA3ng c\U+00E1ch h\U+00E0ng <c\U+1ED9t>   <"
			  (rtos *kch* 2 2)
			 "> :"
		  )
	 )
)
(if (not kch) (setq kch *kch*) (setq *kch* kch))
(setq lstAll (list))
(setq Elast (entlast))
(setq Clor (getvar "CECOLOR"))
(setvar "CECOLOR" "2")
;;;CHON DIEM TRONG VUNG KIN
(while
	(setq pt (getpoint "\n Pick t\U+1EEBng \U+0111i\U+1EC3m trong t\U+1EEBng v\U+00F9ng: "))
	(vl-cmdf  "-boundary" pt "")
)
(while
          (setq Elast  (entnext Elast ))
	  (setq lstAll (reverse (cons Elast lstAll)))
)
(setvar "CECOLOR" Clor)
;;;;;;TRAI MANG TRONG TUNG VUNG KIN
(while (< i (length lstAll))
  (setq P1 (mident (nth i lstAll)))
  (setq P2 (Polar_1 P1 (/ kch 2) (/ kch 2)))
  (AIPL (nth i lstAll) TenBlock P1  kch)
  (AIPL (nth i lstAll) TenBlock P2  kch)
  (entdel (nth i lstAll))
  (setq i (1+ i))
)
(setvar "OSMODE" Olmode)
(princ)
)

;;;;;;;;;;TIM DIEM TRONG TAM CUA VUNG;;;;;;;;;;;;;
(defun mident (ent / p1 p2)
	(vla-getboundingbox (vlax-ename->vla-object ent) 'p1 'p2)
	(setq   p1 (vlax-safearray->list p1)
	  	p2 (vlax-safearray->list p2)
		pt (mapcar '+ p1 p2)
		pt (mapcar '* pt '(0.5 0.5 0.5))
	)
	pt
)
(defun Polar_1 (Pnt KC_X KC_Y / )
(setq P1a (polar Pnt 0 KC_X))
(setq P1b (polar P1a (/ pi 2) KC_Y))
P1b
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ar (s nr nc nl dr dc dl)
	(vlax-invoke s 'ArrayRectangular
			nr nc nl dr dc dl
	)
)

;;;;;;;;;;;;;HAM ARRAY TRONG VUNG KIN;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun AIPL (Ent TenBlock Pnt kch / ms pl minp maxp minpp name kc kcc dtd dchen ssa ans line minp2); Array in Close Pline
(vl-load-com)
(setq ms (vla-get-modelspace (vla-get-activedocument(vlax-get-acad-object))))
(setq oldos (getvar "osmode"))
(setq Olmode (getvar "OSMODE"))
(setq ssa (list))
(setq pl (vlax-ename->vla-object Ent))
(vla-getboundingbox pl 'minp 'maxp);;;LAY TOA DO MAX MIN CUA PLINE
(setq minp (safearray-value minp))
(setq maxp (safearray-value maxp))
(setq minpp (mapcar '- minp (list (distance maxp minp) (distance maxp minp) 0)))
  (setvar "osmode" 0)
  (vl-cmdf "-insert" TenBlock Pnt 1 1 0.0)
  (setq Enlast (entlast))
      (setq dtd (vlax-ename->vla-object Enlast))
      (setq ssa
	(append (list dtd)
		(ar dtd (1+ (fix (/ (- (cadr maxp) (cadr Pnt)) kch)))
			(1+ (fix (/ (- (car maxp) (car Pnt)) kch)))
		  	1 (+ kch) (+ kch) 0
	  	 )
		(ar dtd	(1+ (fix (/ (- (cadr Pnt) (cadr minp)) kch)))
			(1+ (fix (/ (- (car Pnt) (car minp)) kch)))
		  	1 (- kch) (- kch) 0
	  	)
	        (ar dtd (1+ (fix (/ (- (cadr maxp) (cadr Pnt)) kch)))
			(1+ (fix (/ (- (car Pnt) (car minp)) kch)))
		  	1 (+ kch) (- kch) 0
	  	)
		(ar dtd (1+ (fix (/ (- (cadr Pnt) (cadr minp)) kch)))
			(1+ (fix (/ (- (car maxp) (car Pnt)) kch)))
		  	1 (- kch) (+ kch) 0
	  	)
	)
     )
	(foreach x ssa
		(setq line (vla-addline ms (vlax-3d-point minpp)
		 	       (vla-get-insertionpoint x)
			   )
		)
		(if (= (rem (length (vlax-invoke pl 'intersectwith line 0)) 2) 0)
			(vla-erase x)
		)	
		(vla-erase line)
	)
	(setvar "OSMODE" Olmode)
)