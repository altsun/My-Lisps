;;===========================================
;;;(alert (strcat
;;;	 "\n Ch\U+01B0\U+01A1ng tr\U+00ECnh t\U+1EA1o t\U+1EC7p b\U+00ECnh sai l\U+01B0\U+1EDBi \U+0111\U+1ED9 cao APNET"
;;;	 "\n - L\U+1EC7nh ch\U+01B0\U+01A1ng tr\U+00ECnh: LDC"
;;;	 "\n Ng\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n  - \U+0110\U+1ECBa Ch\U+00EDnh K48"
;;;	)
;;;)

;;;(prompt (strcat
;;;	 "\n Ch\U+01B0\U+01A1ng tr\U+00ECnh t\U+1EA1o t\U+1EC7p b\U+00ECnh sai l\U+01B0\U+1EDBi \U+0111\U+1ED9 cao APNET"
;;;	 "\n - L\U+1EC7nh ch\U+01B0\U+01A1ng tr\U+00ECnh: TDC"
;;;	 "\n Ng\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n  - \U+0110\U+1ECBa Ch\U+00EDnh K48"
;;;	)
;;;)
	 
(defun c:LDC()
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq chon (getstring "\n Ch\U+1ECDn d\U+1EA1ng l\U+01B0\U+1EDBi b\U+00ECnh sai: 10 - M\U+1ED9t \U+0111i\U+1EC3m g\U+1ED1c (kh\U+00E9p k\U+00EDn) / 11 - Hai \U+0111i\U+1EC3m g\U+1ED1c (1 \U+0110\U+1EA7u -1 Cu\U+1ED1i):  "))
  (if chon (setq chon (strcase chon nil)))
  (progn
	(if (= chon  "10")
		(1dau_0cuoi)
	)
   	(if (= chon  "11")
		(1dau_1cuoi)
	)
  )
(princ)
)
;;===========================================

(defun 1dau_0cuoi( / LayerTD en tenct ob n i k dd dc Text P1 P2 )
(vl-load-com)
(command "undo" "be")
(setq LayerTD (cdr (assoc 8 (entget (car (entsel "\n Ch\U+1ECDn Text l\U+00E0 t\U+00EAn \U+0111i\U+1EC3m: "))))))
(alert (strcat "\n Layer c\U+1EE7a t\U+00EAn \U+0111i\U+1EC3m v\U+1EEBa ch\U+1ECDn c\U+00F3 t\U+00EAn l\U+00E0: : " LayerTD
	 "\n H\U+00E3y ch\U+1ECDn Pline \U+0111\U+1EC3 ti\U+1EBFp t\U+1EE5c......."
       )
)
(setq en (entsel "\n Ch\U+1ECDn Polyline"))
(setq tenct (getstring T "\nNh\U+1EADp t\U+00EAn l\U+01B0\U+1EDBi: "))
(setq i 0)
(setq k 1)
(if en
  (progn
    	(setq fn (getfiled "Save file"  "" "csv;txt" 1))
	(if (/= fn nil)
	        (progn
	          (setq fh (open fn "w")
		        ob (vlax-ename->vla-object (car en))
		         n (vlax-curve-getEndParam ob)
			 dd (vlax-curve-getPointAtParam ob 0)
		  )
		  (write-line (strcat tenct) fh)
		  (write-line (strcat ( rtos n 2 0) "," ( rtos (- n 1) 2 0) "," ( rtos 1 2 0) "," ( rtos 20 2 0)) fh)
		  (write-line (strcat ( rtos n 2 0) "," (rtos (caddr (nth 0 (GetDGN dd LayerTD))) 2 3)) fh)
		  (while (< k n)
		    (progn
		      	(setq PntPL (vlax-curve-getPointAtParam ob k))
		      	(setq Text (nth 1 (GetDGN PntPL LayerTD)))
			(write-line Text fh)
		    )
	                (setq k (+ k 1))
	          )
		  (write-line (nth 1 (GetDGN dd LayerTD)) fh)
	          (while (<= i n)
		      (progn
			(setq p1 (vlax-curve-getPointAtParam ob i)
			      p2 (vlax-curve-getPointAtParam ob (+ i 1))
			)
		      )
			(progn
			    	(if (and  (>= i 0) (< i (- n 1)))
				    (write-line (strcat ( rtos (+ i 1) 2 0) "," ( rtos (+ i 1) 2 0) "," ( rtos (+ i 2) 2 0) "," (rtos (- (caddr (nth 0 (GetDGN p2 LayerTD))) (caddr (nth 0 (GetDGN p1 LayerTD)))) 2 3 ) "," (rtos (* (TKC P2 P1) 1) 2 3)) fh)
			        )
				(if (= i n)
				    (write-line (strcat (rtos n 2 0) "," (rtos n 2 0) "," (rtos 1 2 0) "," (rtos (- (caddr (nth 0 (GetDGN dd LayerTD))) (caddr (nth 0 (GetDGN (vlax-curve-getPointAtParam ob (- n 1)) LayerTD)))) 2 3 ) "," (rtos (* (TKC dd (vlax-curve-getPointAtParam ob (- n 1))) 1) 2 3)) fh)
			        )
                  	)
		    	(setq i (+ i 1))
		  )
		  (close fh)
	         )
        )
   )
)
(princ)
)



(defun 1dau_1cuoi( / LayerTD en tenct ob n i k dd dc Text P1 P2 )
(vl-load-com)
(command "undo" "be")
(setq LayerTD (cdr (assoc 8 (entget (car (entsel "\n Ch\U+1ECDn Text l\U+00E0 t\U+00EAn \U+0111i\U+1EC3m: "))))))
(alert (strcat "\n Layer c\U+1EE7a t\U+00EAn \U+0111i\U+1EC3m v\U+1EEBa ch\U+1ECDn c\U+00F3 t\U+00EAn l\U+00E0: : " LayerTD
	 "\n H\U+00E3y ch\U+1ECDn Pline \U+0111\U+1EC3 ti\U+1EBFp t\U+1EE5c......."
       )
)
(setq en (entsel "\n Ch\U+1ECDn Polyline"))
(setq tenct (getstring T "\nNh\U+1EADp t\U+00EAn l\U+01B0\U+1EDBi: "))
(setq i 0)
(setq k 1)
(if en
  (progn
    	(setq fn (getfiled "Save file"  "" "csv;txt" 1))
	(if (/= fn nil)
	        (progn
	          (setq fh (open fn "w")
		        ob (vlax-ename->vla-object (car en))
		         n (vlax-curve-getEndParam ob)
			 dd (vlax-curve-getPointAtParam ob 0)
			 dc (vlax-curve-getPointAtParam ob n)
		  )
		  (write-line (strcat tenct) fh)
		  (write-line (strcat ( rtos n 2 0) "," ( rtos (- n 1) 2 0) "," ( rtos 2 2 0) "," ( rtos 1 2 0)) fh)
		  (write-line (strcat ( rtos n 2 0) "," (rtos (caddr (nth 0 (GetDGN dd LayerTD))) 2 3)) fh)
		  (write-line (strcat ( rtos (+ n 1) 2 0) "," (rtos (caddr (nth 0 (GetDGN dc LayerTD))) 2 3)) fh)
		  (while (< k n)
		    (progn
		      	(setq PntPL (vlax-curve-getPointAtParam ob k))
		      	(setq Text (nth 1 (GetDGN PntPL LayerTD)))
			(write-line Text fh)
		    )
	                (setq k (+ k 1))
	          )
		  (write-line (nth 1 (GetDGN dd LayerTD)) fh)
		  (write-line (nth 1 (GetDGN dc LayerTD)) fh)
	          (while (<= i n)
		      (progn
			(setq p1 (vlax-curve-getPointAtParam ob i)
			      p2 (vlax-curve-getPointAtParam ob (+ i 1))
			)
		      )
			(progn
				(if (= i 0)
				    (write-line (strcat ( rtos (+ i 1) 2 0) "," (rtos n 2 0) "," (rtos (+ i 1) 2 0) "," (rtos (- (caddr (nth 0 (GetDGN P2 LayerTD))) (caddr (nth 0 (GetDGN P1 LayerTD)))) 2 3 ) "," (rtos (* (TKC P2 P1) 1) 2 3) ) fh)
			        )
			    	(if (and  (> i 0) (< i (- n 1)))
				    (write-line (strcat ( rtos (+ i 1) 2 0) "," ( rtos i 2 0) "," ( rtos (+ i 1) 2 0) "," (rtos (- (caddr (nth 0 (GetDGN p2 LayerTD))) (caddr (nth 0 (GetDGN p1 LayerTD)))) 2 3 ) "," (rtos (* (TKC P2 P1) 1) 2 3)) fh)
			        )
				(if (= i n)
				    (write-line (strcat (rtos n 2 0) "," (rtos (- n 1) 2 0) "," (rtos (+ n 1) 2 0) "," (rtos (- (caddr (nth 0 (GetDGN dc LayerTD))) (caddr (nth 0 (GetDGN (vlax-curve-getPointAtParam ob (- n 1)) LayerTD)))) 2 3 ) "," (rtos (* (TKC dc (vlax-curve-getPointAtParam ob (- n 1))) 1) 2 3)) fh)
			        )
                  	)
		    	(setq i (+ i 1))
		  )
		  (close fh)
	         )
        )
   )
)
(princ)
)

(defun TKC (P1 P2 /)
(setq KC (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2))))
KC
)


;;
(defun GetDGN (Pnt1 LayerTD / KC_Ename Ds_KC_Ename  KC Pnt Ename_Text_nearest);Tim diem gan nhat
(vl-load-com)
(setq KC_Ename (list))
(setq Ds_KC_Ename (list))
(setq DGN (list))
(setq Pnt (list (car Pnt1) (cadr Pnt1)))
(setq sstext (ssget "_X" (list (cons 0 "TEXT") (cons 8 LayerTD)) ))
(setq ss (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex sstext))))
(foreach item ss
	(setq temp  (entget item))
	(setq   Tdo (list (car (TD:Text-Base item )) (cadr (TD:Text-Base item ))))
	(setq KC (distance Pnt Tdo))
  	(setq KC_Ename (list KC item))
	(setq Ds_KC_Ename (append Ds_KC_Ename (list KC_Ename)))
)
(setq Ename_Text_nearest (car (vl-sort Ds_KC_Ename '(lambda (e1 e2) (< (car e1) (car e2))))))
(setq  Tendiem (cdr (assoc 1 (entget (cadr Ename_Text_nearest)))))
(setq Pnt (TD:Text-Base (cadr Ename_Text_nearest)))
(setq DGN (list Pnt Tendiem))
DGN
)
 DAO CHIEU POLILINE+LINE+ARC+.........

(defun c:daochieu(/ ss count lwp ent obj oname sss revlwpl revln)
  (vl-load-com)
  (defun revlwpl(/ eo el len)
	(setq eo ent)
	(setq el (list(assoc 210 ent)))
	(while (member (assoc 10 ent) ent)
	  (if (= 0.0 (assoc 42 ent))
   (setq el (cons (assoc 42 ent) el))
   (setq el (cons (cons 42 (- (cdr (assoc 42 ent)))) el))
	  )
	  (setq el (cons (assoc 41 ent) el))
	  (setq el (cons (assoc 40 ent) el))
	  (setq el (cons (assoc 10 ent) el))
	  (setq ent (member (assoc 10 ent) ent))
	  (setq ent (cdr ent))
	)
	(setq len(- (LENGTH eo) (LENGTH (member (assoc 10 eo) eo)) 1))
	(while (>= len 0)
	  (setq el (cons (nth len eo) el))
	  (setq len (- len 1))
	)
	(setq ent el)
	(entmod ent)
  )
  (defun revln (/ pt1 pt2)
	(setq pt1 (cons 10 (cdr (assoc 11 ent))))
	(setq pt2 (cons 11 (cdr (assoc 10 ent))))
	(setq ent (subst pt1 (assoc 10 ent) ent))
	(setq ent (subst pt2 (assoc 11 ent) ent))
	(entmod ent)
  )
	 
  (princ "\nSelect Lines & Polylines to reverse direction of:   ")
  (setq ss (ssget '((0 . "POLYLINE,LWPOLYLINE,LINE"))))
  (setvar "CMDECHO" 0)
  (command "._UNDO" "_BEgin")
  (if ss
	(progn
	  (setq count 0 lwp 0)
	  (while (> (sslength ss) count)
		(setq ent (ENTGET (ssname ss count))
				  obj (vlax-ename->vla-object (ssname ss count))
				  oname (vlax-get-property obj 'ObjectName)
		)
		(cond
		  ((= oname "AcDb3dPolyline")(setq lwp(+ 1 lwp)))
		  ((= (cdadr ent) "LWPOLYLINE")(revlwpl))
		  ((= (cdadr ent) "POLYLINE")
			(progn
			  (setq sss (ssadd (ssname ss count)))
			  (vl-cmdf "convertpoly" "Light" sss "")
			  (setq ent (ENTGET (ssname sss 0)))
			  (revlwpl)
			)
		  )
		  ((= (cdadr ent) "LINE")(revln))
		)
		(setq count (+ count 1))
	  )
	)
  )
  (command "._UNDO" "_End")
  (if(> lwp 0)
	(if(> lwp 1)
	  (princ(strcat "\nCould not reverse " (itoa lwp) " 3dPolylines."))
	  (princ"\nCould not reverse the 3dPolyline.")
	)
  )
  (princ)
)


(defun Cong (ss / n c d)
(setq i 0)
(setq Tong 0)
(progn
      (foreach item ss 
	(setq Temp  (entget item))
	(setq  Noidung (cdr (assoc 1 Temp)))
	(setq Tong (+ Tong (atof Noidung)))
      )
)
Tong
)
(defun c:+()
  (setq ss (ssget ' ((0 . "TEXT"))))
  (setq ss (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex ss))))
  (setq Tong (Cong ss))
  
  (Setq h (TD:GetXWithDefault getreal "\n Nh\U+1EADp chi\U+1EC1u cao ch\U+1EEF: " '*h* "1"))
  (setq p1 (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m ghi k\U+1EBFt qu\U+1EA3: "))
  (entmake (list (cons 0 "TEXT") (cons 10 p1) (cons 40  h) (cons 1  (rtos Tong 2 2))))
  (princ)
)
;*******************************************************
(defun c:-()
  (Prompt "\n Ch\U+1ECDn s\U+1ED1 b\U+1ECB tr\U+1EEB: ")
  (setq ss2 (ssget ' ((0 . "TEXT"))))
  (setq ss2 (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex ss2))))
  (setq Sobitru (Cong ss2))
  
  (Alert "\n Ch\U+1ECDn s\U+1ED1 tr\U+1EEB")
  (setq ss3 (ssget ' ((0 . "TEXT"))))
  (setq ss3 (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex ss3))))
  (setq Sotru (Cong ss3))
  
  (setq Hieu (- Sobitru Sotru))
  
  (Setq h (TD:GetXWithDefault getreal "\n Nh\U+1EADp chi\U+1EC1u cao ch\U+1EEF: " '*h* "1"))
  (setq p1 (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m ghi k\U+1EBFt qu\U+1EA3: "))
  (entmake (list (cons 0 "TEXT") (cons 10 p1) (cons 40  h) (cons 1  (rtos Hieu 2 2))))
  (princ)
)

;*******************************************************
(defun c:* (/ n c d)
(setq Boiso 1)
(setq ss (ssget ' ((0 . "TEXT"))))
(setq ss (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex ss))))
(progn
      (foreach item ss 
	(setq Temp  (entget item))
	(setq  Noidung (cdr (assoc 1 Temp)))
	(setq Boiso (* Boiso (atof Noidung)))
      )
)
  
  (Setq h (TD:GetXWithDefault getreal "\n Nh\U+1EADp chi\U+1EC1u cao ch\U+1EEF: " '*h* "1"))
  (setq p1 (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m ghi k\U+1EBFt qu\U+1EA3: "))
  (entmake (list (cons 0 "TEXT") (cons 10 p1) (cons 40  h) (cons 1  (rtos Boiso 2 2))))
  (princ)
)

(DEFUN c:/()
  (setq a (car (entsel "\n Ch\U+1ECDn s\U+1ED1 b\U+1ECB chia")))
  (setq c  (entget a))
  (setq d (cdr (assoc 1 c)))
  (setq Sobichia (atof d))
  (prompt "\n Ch\U+1ECDn s\U+1ED1 chia")
  (Alert "\n Ch\U+1ECDn s\U+1ED1 chia")

  (setq a1 (car (entsel "\n Ch\U+1ECDn s\U+1ED1 chia")))
  (setq c1 (entget a1))
  (setq d1 (cdr (assoc 1 c1)))
  (setq Sochia (atof d1))
  
  (setq Thuong (/ Sobichia  Sochia))
  (Setq h (TD:GetXWithDefault getreal "\n Nh\U+1EADp chi\U+1EC1u cao ch\U+1EEF: " '*h* "1"))
  (setq p1 (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m ghi k\U+1EBFt qu\U+1EA3: "))
  (entmake (list (cons 0 "TEXT") (cons 10 p1) (cons 40  h) (cons 1  (rtos Thuong 2 2))))
  (princ)
)


(defun TD:GetXWithDefault ( _function _prompt _symbol _default / _toString )

	(setq _toString
		(lambda ( x )
			(cond
				( (eq getangle _function) (angtos x) )
				( (eq 'REAL (type x)) (rtos x) )
				( (eq 'INT (type x)) (itoa x) )
				( x )
			)
		)
	)

	(set _symbol
	(
	(lambda ( input ) (if (or (not input) (eq "" input)) (eval _symbol) input))
	(_function (strcat _prompt "<" (_toString (set _symbol (cond ( (eval _symbol) ) ( _default )))) "> : "))
	)
	)
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