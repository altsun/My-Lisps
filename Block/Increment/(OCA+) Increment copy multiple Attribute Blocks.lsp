;;;**********************************************
;;;CHUONG TRINH DANH SO THU TU VA COPY TANG DAN
;;;1. Lenh OD: danh so thu tu, tuy chon so bat dau (begin) va so gia (increment) tuy y
;;;2. Lenh OC: copy tang dan tu mot so thu tu co san
;;;3. Lenh OCA: copy tang dan voi doi tuong Attribute Block
;;;Chuong trinh chap nhan cac dinh dang bang so, chu, so va chu ket hop:
;;;1, 2... A, B..., A1, A2..., AB-01, AB-02..., AB-01-C1, AB-01-C2...
;;;Cac chu gioi han trong khoang tu A den Z. Cac so khong han che
;;;Copyright by ssg - www.cadviet.com - December 2008
;;;**********************************************

(defun c:OCA+ ( / ss i e e0 dn p1 p2 ename sslist sslst)
	(setq ss (ssget '((0 . "INSERT"))))  ; Select blocks
	(if ss
		(progn
			(setq sslist (append))
			(setq i 0)

			(while (setq ename (ssname ss i))
				(setq sslst (append sslst (list ename)))
				(setq i (1+ i))
			)

			(setq
				k0 (getint "\nIncrement <1>: ")
				k k0
				p1 (getpoint "\nBase point:")
			)

			(while
				(setq p2 (getpoint p1 "\nSpecify second point or <use first point as displacement>:"))
				(mapcar '(lambda (x) (_oca x p1 p2 k)) sslst)
				(setq k (+ k k0))
			)
		)
	)
)

(defun _oca( e0 p1 p2 dn / cn c ndat) ;;;Make Ordinal number. Copy from Atttribute block
	(setq e (entnext e0))

	(if (/= (etype e) "ATTRIB") (progn (exit)))

	(setq
		cn (cdr (assoc 1 (entget e)))
	)
	(if (not dn) (setq dn 1))
	(if (= cn "") (setq cn "1"))
	(setq
		c (vl-string-right-trim "0 1 2 3 4 5 6 7 8 9" cn)
		n (vl-string-subst "" c cn)
	)

	(command "copy" e0 "" p1 p2)

	(if (= n "")
		(setq cn (incC cn))
		(setq cn (strcat c (incN (vl-string-subst "" c cn) dn)))
	)
	(setq
	dat (entget (entnext (entlast)))
	dat (subst (cons 1 cn) (assoc 1 dat) dat)
	)
	(entmod dat)
	(command "regen")
	(princ)
)
;;;============================
