; Chỉnh sửa: Phạm Thế Dương, duongphamhn97@gmail.com

(defun C:TBC ( /  ED ENAMET ENTTEXTGHI LOOP LTS N NOIDUNG TBC1 TONG)
(setq loop T)
(setq Lts (list))
(while loop
	(setq EnameT (Car (Entsel "\nCh\U+1ECDn t\U+1EEBng Text: ")))
  	(cond
	  (T
		(if EnameT
		  (progn
		    	(setq  Noidung (cdr (assoc 1 (entget EnameT))))
		    	(if (distof Noidung 2)
			    (setq Lts (append Lts (list Noidung)))
			)
		  )
		  (setq loop nil)
		)
	  )
	)
)

(setq lam_tron (getint "Ban muon lam tron may chu so sau dau phay?") )

(setq n (length Lts))
(setq Tong 0)
(foreach e Lts
	(setq Tong (+ Tong (atof e)))
)
(setq TBC1 (/ Tong n))
(setq EntTextGhi (car (entsel "\nCh\U+1ECDn Text ghi k\U+1EBFt qu\U+1EA3 t\U+00EDnh trung b\U+00ECnh c\U+1ED9ng: ")))
(setq ed (entget EntTextGhi))
(setq ed (subst (cons 1 (rtos TBC1 2 lam_tron)) (assoc 1 ed) ed ))
(entmod ed) 
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

(defun ChonTextSo (ss / i ent str ss1) 
  (if ss
    (progn
      (setq i	0
	    ss1	(ssadd)
      )
      (repeat (sslength ss)
	(setq ent (ssname ss i)
	      str (cdr(assoc 1 (entget ent)))
	      i	  (+ 1 i)
	)
	(if (distof str 2)
	  (ssadd ent ss1)
	)
      )
      (if (> (sslength ss1) 0)
	ss1
      )      
    )
  )
)