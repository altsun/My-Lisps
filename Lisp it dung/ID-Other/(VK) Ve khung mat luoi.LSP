;;;(Alert (strcat "\n Ch\U+01B0\U+01A1ng tr\U+00ECnh t\U+1EA1o khung b\U+00ECnh \U+0111\U+1ED3"
;;;		"\n H\U+00E3y g\U+00F5 l\U+1EC7nh VK \U+0111\U+1EC3 ch\U+1EA1y ch\U+01B0\U+01A1ng tr\U+00ECnh"
;;;		"\n Ng\U+01B0\U+1EDDi vi\U+1EBFt: Ad Nguy\U+1EC5n Thi\U+00EAn \U+0110\U+01B0\U+1EDDng"
;;;       )
;;;)
;;;(Prompt (strcat "\n Ch\U+01B0\U+01A1ng tr\U+00ECnh t\U+1EA1o khung b\U+00ECnh \U+0111\U+1ED3"
;;;		"\n H\U+00E3y g\U+00F5 l\U+1EC7nh VK \U+0111\U+1EC3 ch\U+1EA1y ch\U+01B0\U+01A1ng tr\U+00ECnh"
;;;		"\n Ng\U+01B0\U+1EDDi vi\U+1EBFt: Ad Nguy\U+1EC5n Thi\U+00EAn \U+0110\U+01B0\U+1EDDng"
;;;       )
;;;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TAO WBLOCK MAT LUOI KHUNG NGHIENG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:VK( / olmode P1 P2 Tleebd)
(vl-load-com)
(setq olmode (getvar "OSMODE"))
(setvar "OSMODE" 1)
(setq P1 (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m tr\U+00E1i tr\U+00EAn => "))
(setq P2 (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m ph\U+1EA3i d\U+01B0\U+1EDBi => "))
(or *Tleebd* (setq *Tleebd* 1000))
(setq Tleebd (getreal (strcat "\n \n Nh\U+1EADp t\U+1EF7 l\U+1EC7 b\U+1EA3n \U+0111\U+1ED3   <"
		  (rtos *Tleebd* 2 0)
		 "> :"
	  )
 )
)
(if (not Tleebd) (setq Tleebd *Tleebd*) (setq *Tleebd* Tleebd))
(TML1 P1 P2 Tleebd)
(setvar "OSMODE" olmode)
(princ)
)

(defun TML1 (P1 P22 tile_tmp /  Ent Height rau tHeight len_per ob ) ;;;; Tao mat luoi
(vl-load-com)

(setq olmode (getvar "OSMODE"))
(setvar "Osmode" 1)
(setq Height (abs (- (cadr P1) (cadr P22))))
(or #tile (setq #tile 500))
(if tile_tmp (setq #tile tile_tmp))
(setq dis (/ #tile 10.0)
        rau (/ #tile 200.0)
        tHeight  (/ (* 2 rau) 5)
        len_per (/ #tile 100.0)
)
(setq WithLine (* 2 (/ rau 5)))
(setq olmode (getvar "OSMODE"))
(setvar "Osmode" 0)
(setq P11 (list (car P1) (cadr P22)))
(setq
      Gocxoay (angle P11 P22)
      Kc (distance P11 P22)
      P3 (polar P11 (+ (/ pi 2) Gocxoay) Height)
      P4 (polar P3  Gocxoay  Kc)
)
(command "Pline" P11 P3 P4 P22 P11 "")
(setq e (entlast))
(setq Elast (entlast))
(vla-getboundingbox (vlax-ename->vla-object e) 'p1 'p2)
(mapcar  '(lambda (a b ) (* 0.5 (+ a b )))
(setq p1 (vlax-safearray->list p1)) (setq p2(vlax-safearray->list p2)))
(setq x1 (car p1) x2 (car p2) y1 (cadr p1) y2 (cadr p2) y1_tmp (round+ y1 dis) x1_tmp (round+ x1 dis))
;;; DoX 
(while (< y1_tmp y2)
	(setq objLine (ST:Entmake-Line (list x1 y1_tmp) (list x2 y1_tmp)))
	(setq y1_tmp (+ y1_tmp dis)
		lstInter (ST:Ent-IntersObj (entlast) e)
		lstInter (vl-sort lstInter '(lambda (x y) (< (car x)(car y))))
		1st  (car lstInter)
	        2nd  (cadr lstInter)
	)
	;Trai
	(ST:Entmake-Line 1st (mapcar '- 1st (list (* 2 rau) 0 0)))
	(wtxt (substr (rtos (cadr 1st) 2 0) 1 4) (list (- (+ (car 1st) WithLine) (* 2 rau)) (+ (cadr 1st) (/ rau 10))) tHeight 0 "L")
  	(wtxt  (substr (rtos (cadr 1st) 2 0) 5) (list (- (car 1st) (/ rau 10)) (- (cadr 1st) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TR")
	;Phai
	(ST:Entmake-Line 2nd (mapcar '+ 2nd (list (* 2 rau) 0 0)))
	(wtxt (substr (rtos (cadr 2nd) 2 0) 1 4) (list (car 2nd) (+ (cadr 2nd) (/ rau 10))) tHeight 0 "L")
  	(wtxt  (substr (rtos (cadr 2nd) 2 0) 5) (list (+ (- (car 2nd) WithLine) (* 2 rau)) (-  (cadr 2nd)  (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TR")
  
	(ST:GGBP (car lstInter) (cadr lstInter) dis len_per)				
	(entdel objLine)
;;Do sth else		
)
(ST:Entmake-Line P11 (list (- (car P11) (* 2 rau)) (cadr P11)))
(ST:Entmake-Line P11 (list (car P11) (- (cadr P11) (* 2 rau)) ))

(ST:Entmake-Line P22 (list (+ (car P22) (* 2 rau)) (cadr P22)))
(ST:Entmake-Line P22 (list (car P22) (- (cadr P22) (* 2 rau)) ))

(ST:Entmake-Line P4 (list (+ (car P4) (* 2 rau)) (cadr P4)))
(ST:Entmake-Line P4 (list (car P4) (+ (cadr P4) (* 2 rau))))

(ST:Entmake-Line P3 (list (- (car P3) (* 2 rau)) (cadr P3)))
(ST:Entmake-Line P3 (list (car P3) (+ (cadr P3) (* 2 rau)) ))
;;;;;;;;GHI TOA DOA Y 2314678.789
  	;Tay Nam
(wtxt  (substr (rtos (cadr P11) 2 0) 1 4) (list (- (+ (car P11) WithLine) (* 2 rau) ) (+ (cadr p11) (/ rau 10))) tHeight 0 "L")
(wtxt  (substr (rtos (cadr P11) 2 0) 5) (list  (- (car P11) (/ rau 10))  (- (cadr P11) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TR")
; Dong Nam
(wtxt  (substr (rtos (cadr P22) 2 0) 1 4) (list (car P22) (+ (cadr p22) (/ rau 10))) tHeight 0 "L")
(wtxt  (substr (rtos (cadr P22) 2 0) 5) (list (- (+ (car P22) (* 2 rau)) WithLine) (- (cadr P22) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TR")
;Dong Bac
(wtxt  (substr (rtos (cadr P4) 2 0) 1 4) (list (car P4) (+ (cadr P4) (/ rau 10))) tHeight 0 "L")
(wtxt  (substr (rtos (cadr P4) 2 0) 5) (list (- (+ (car P4) (* 2 rau)) WithLine) (- (cadr P4) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TR")
;Tay Bac
(wtxt  (substr (rtos (cadr P3) 2 0) 1 4) (list (- (+ (car P3) WithLine) (* 2 rau)) (+ (cadr P3) (/ rau 10))) tHeight 0 "L")
(wtxt  (substr (rtos (cadr P3) 2 0) 5) (list (- (car P3) (/ rau 10)) (- (cadr P3) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TR")
;;;;;;;;GHI TOA DOA X 503456.789
	;Tay Nam
(wtxt  (substr (rtos (car P11) 2 0) 1 3) (list (-  (car P11)  (/ rau 10)) (+  (- (cadr P11) rau ) (/ rau 10))) tHeight 0 "TR")
(wtxt  (substr (rtos (car P11) 2 0) 4) (list (+ (car P11) (/ rau 10)) (+  (- (cadr P11)rau) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TL")
;;;  	; Dong Nam
(wtxt  (substr (rtos (car P22) 2 0) 1 3) (list (- (car P22) (/ rau 10)) (+  (- (cadr P22) rau) (/ rau 10))) tHeight 0 "TR")
(wtxt  (substr (rtos (car P22) 2 0) 4) (list (+ (car P22) (/ rau 10))  (+  (- (cadr P22) rau ) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TL")
;;;	;Dong Bac
(wtxt  (substr (rtos (car P4) 2 0) 1 3) (list (- (car P4) (/ rau 10)) (+  (+ (cadr P4) rau) (* 2 (/ rau 10)))) tHeight 0 "TR")
(wtxt  (substr (rtos (car P4) 2 0) 4) (list (+ (car P4) (/ rau 10)) (+  (+ (cadr P4) rau) (* 2 (/ rau 10))))  (/ (* 3 tHeight) 2) 0 "TL")
;;;	;Tay Bac
(wtxt  (substr (rtos (car P3) 2 0) 1 3) (list (- (car P3) (/ rau 10)) (+  (+ (cadr P3) rau) (* 2 (/ rau 10)))) tHeight 0 "TR")
(wtxt  (substr (rtos (car P3) 2 0) 4) (list (+ (car P3) (/ rau 10)) (+  (+ (cadr P3) rau) (* 2 (/ rau 10))))  (/ (* 3 tHeight) 2) 0 "TL")
(setvar "CECOLOR" "40")
(command "rectangle" "w" WithLine (list (- (car P11) (* 2 rau)) (- (cadr P11) (* 2 rau))) (list (+ (car P4) (* 2 rau)) (+ (cadr P4) (* 2 rau))))
(setvar "CECOLOR" "256")
	;;DoY
(while (< x1_tmp x2)
	(setq objLine (ST:Entmake-Line (list x1_tmp y1)(list x1_tmp y2)))
	(setq x1_tmp (+ x1_tmp dis) 
		lstInter (ST:Ent-IntersObj (entlast) e)
		lstInter (vl-sort lstInter '(lambda (x y) (< (cadr x)(cadr y))))
		1st (car lstInter) 2nd (cadr lstInter)
		)
	;Duoi
	(ST:Entmake-Line 1st (mapcar '- 1st (list 0 (* 2 rau) 0)))
	(wtxt  (substr (rtos (car 1st) 2 0) 1 3) (list (- (car 1st) (/ rau 10)) (+  (- (cadr 1st) rau) (/ rau 10))) tHeight 0 "TR")
	(wtxt  (substr (rtos (car 1st) 2 0) 4) (list (+ (car 1st) (/ rau 10)) (+  (- (cadr 1st) rau) (/ rau 10)))  (/ (* 3 tHeight) 2) 0 "TL")
  
	;Tren
	(ST:Entmake-Line 2nd (mapcar '+ 2nd (list 0 (* 2 rau) 0 )))
	(wtxt  (substr (rtos (car 2nd) 2 0) 1 3) (list (- (car 2nd) (/ rau 10)) (+  (+ (cadr 2nd) rau) (* 2 (/ rau 10)))) tHeight 0 "TR")
	(wtxt  (substr (rtos (car 2nd) 2 0) 4) (list (+ (car 2nd) (/ rau 10)) (+  (+ (cadr 2nd) rau) (* 2 (/ rau 10))))  (/ (* 3 tHeight) 2) 0 "TL")
  
	(entdel objLine)
;;Do sth else		
)
(setvar "OSMODE" olmode)
(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dinhhcn (P Cdai CCao / ) ;Dinh hinh chu nhat
(setq P2 (Polar P 0 Cdai))
(setq P4 (Polar P (/ pi 2) CCao))
(setq P3 (Polar P4 0 Cdai))
(setq DHV (list P P2 P3 P4 P))
DHV
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Defun RTD(x) (/ (* x 180) pi) )
(defun round+ (num prec)
	(if (< 0 prec)
		(* prec
			 (if (minusp (setq num (/ num prec)))
			     (fix num)
			     (if (= num (fix num))
			         num
			         (fix (1+ num))
			     )
			 )
		)
	num
	)
)
(defun ST:Entmake-Point (pt Len / lstEn)
	(append (list (ST:Entmake-Line (mapcar '- pt (list (/ len 2) 0 0))(mapcar '+ pt (list (/ len 2) 0 0)))) 
	(list (ST:Entmake-Line (mapcar '- pt (list 0 (/ len 2)  0))(mapcar '+ pt  (list 0 (/ len 2)  0)))))
)
(defun ST:Entmake-Line (p1 p2)(entmakex (list (cons 0 "LINE")(cons 10 p1)(cons 11 p2))))
(defun ST:Ent-IntersObj (e1 e2  / ob1 ob2 g L i kq)
(setq
    ob1 (vlax-ename->vla-object e1)
    ob2 (vlax-ename->vla-object e2)
)	
(setq g (vlax-variant-value (vla-IntersectWith ob1 ob2 acExtendNone)))
(if (/= (vlax-safearray-get-u-bound g 1) -1) (setq L (vlax-safearray->list g)))
(setq i 0)
(repeat (/ (length L) 3)
    (setq kq (append (list (list (nth i L) (nth (+ i 1) L) (nth (+ i 2) L))) kq))
    (setq i (+ i 3))
)
kq
)
(defun ST:GGBP (p1 p2 dis len_perLine / x1) ;trai -> phai
		(setq x1 (round+ (car p1) dis))
		(while (< x1 (car p2))
			(ST:Entmake-Point  (list  x1 (cadr p1))	len_perLine)
			(setq x1 (+ x1 dis)))
)



(defun wtxt (string Point Height Ang justify / Lst)
 (setq Lst (list '(0 . "TEXT")
   (cons 8 (if Layer Layer (getvar "Clayer")))
   (cons 62 (if Color Color 256))
   (cons 10 point)
   (cons 40 Height)
   (cons 1 string)
   (if Ang (cons 50 Ang))
   (cons 7 (if Style Style (getvar "Textstyle"))))
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
 (entmake Lst)
)

