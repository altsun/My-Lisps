; copy text +
; www.vietlisp.com
; Modded by duongphamhn97@gmail.com
;-------------------------------------------------------------------------------
(defun c:c++ (/ copy+ ss sslst i k k0 p1 p2 )
  
(defun copy++ (ename p1 p2 add / ent i number str)

	(command "copy" ename "" p1 p2)
	(setq ent (entget (entlast)))
	(if (setq str (cdr (assoc 1 ent)))
		(progn
			(setq i 1)
			(while (<= i (strlen str))
				(if (not (wcmatch (substr str i) "*@*"))
				(progn
				(setq number (substr str i))
				(setq i 10000)
				)
				(setq i (1+ i))
				)
			)
			(if number (setq str (strcat (substr str 1 (- (strlen str) (strlen number)))
			(if (vl-string-search " " number) " " "")
			(itoa (+ (atoi number) add)))))

			(setq ent (subst (cons 1 str) (assoc 1 ent) ent))
			(entmod ent)
		)
	)
)

(setq ss (ssget))
(if ss
	(progn
		(setq sslist (append))
		(setq i 0)

		(while (setq ename (ssname ss i))
			(setq sslst (append sslst (list ename)))
			(setq i (1+ i))
		)

		(setq k0 0)
		(setq k0 (getint (strcat "\nNhap so gia: ")))
		(setq k k0)

		(setq p1 (getpoint"\nSpecify base point or [Displacement/mOde] <Displacement>:"))
		(while
			(setq p2 (getpoint p1 "\nSpecify second point or <use first point as displacement>:"))
			(mapcar '(lambda (x) (copy++ x p1 p2 k)) sslst)
			(setq k (+ k k0))
		)
	)
)
(princ)
)
