; Chỉnh sửa: Phạm Thế Dương, duongphamhn97@gmail.com

(defun c:srt (/ cmd ss lst data i lst1 lst2)
(setq ctnc (cond (ctnc) ("Cong")))
(initget "Cong Tru Nhan CHia")
(setq ctnc (cond ((getkword (strcat "\nChon phep tinh: [Cong/Tru/Nhan/CHia/] <" ctnc ">"))) (ctnc)))
(setq cmd (getvar "cmdecho"))
(setvar "cmdecho" 0)
(command "ucs" "world")
(prompt"\nChon hang-cot text thu nhat\n")
(if (setq ss1 (ssget (list (cons 0 "TEXT"))))
(progn
(setq lst1 (vl-sort (mapcar 'entget (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))))
			   '(lambda (x y) (if (equal (car(setq x1 (cdr (assoc 10 x)))) (car(setq y1 (cdr (assoc 10 y)))))
					      (> (cadr x1) (cadr y1)) (< (car x1) (car y1))))))))
(prompt"\nChon hang-cot text thu 2\n")
(if (setq ss2 (ssget (list (cons 0 "TEXT"))))
	(if (< (- (cadr (cdr(assoc 10 (nth 0 lst1)))) (cadr (cdr(assoc 10 (nth 1 lst1))))) 
                 (- (car (cdr(assoc 10 (nth 1 lst1)))) (car (cdr(assoc 10 (nth 0 lst1))))))
		(setq lst2 (vl-sort (mapcar 'entget (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss2))))
                        '(lambda (x y) (if (equal (car(setq x2 (cdr (assoc 10 x)))) (car(setq y2 (cdr (assoc 10 y)))))
                         (> (cadr x2) (cadr y2))  (< (car x2) (car y2))))))
		(setq lst2 (vl-sort (mapcar 'entget (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss2))))
                        '(lambda (x y) (if (equal (car(setq x2 (cdr (assoc 10 x)))) (car(setq y2 (cdr (assoc 10 y)))))
                         (< (cadr x2) (cadr y2)) (> (car x2) (car y2))))))
		))
(if (/= (sslength ss2) (sslength ss1)) (alert "\n    Hai tap hop text co so \ndoi tuong khong bang nhau!")



(progn
  (setq lam_tron (getint "Ban muon lam tron may chu so sau dau phay?") )

  (setq ptkq (getpoint "\nChon diem ghi ket qua hoac enter de ghi ket qua vao hang-cot text khac\n"))
  
  (if (= ptkq nil) 
  (progn
	(prompt"\nChon hang-cot text ghi ket qua\n")
	(if (setq ss3 (ssget (list (cons 0 "TEXT"))))
	(progn
	(setq lst3 (vl-sort (mapcar 'entget (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss3))))
			   '(lambda (x y) (if (equal (car(setq x3 (cdr (assoc 10 x)))) (car(setq y3 (cdr (assoc 10 y)))))
					      (> (cadr x3) (cadr y3))  (< (car x3) (car y3))))))))
	(if (/= (sslength ss2) (sslength ss3)) (alert "\nTap hop text ghi ket qua \nthua hoac thieu doi tuong!"))
  );progn
  );if
);progn
);if
;----------------------------------
(command "undo" "be")
(setq angbs (getvar "angbase"))
(setq oldos (getvar "osmode"))
(setq Ladim (getvar "Dimzin"))
(setq olstyle (getvar "textstyle"))
(setq olcol (getvar "CEColor"))
(setvar "Dimzin" 0)
(setq txti 0)

(while (< txti (sslength ss1))
(if (eq ctnc "Cong") (setq kqi (+ (atof (cdr(assoc 1 (nth txti lst1)))) (atof (cdr(assoc 1 (nth txti lst2)))))))
(if (eq ctnc "Tru")  (setq kqi (- (atof (cdr(assoc 1 (nth txti lst1)))) (atof (cdr(assoc 1 (nth txti lst2)))))))
(if (eq ctnc "Nhan") (setq kqi (* (atof (cdr(assoc 1 (nth txti lst1)))) (atof (cdr(assoc 1 (nth txti lst2)))))))
(if (eq ctnc "CHia") (setq kqi (/ (atof (cdr(assoc 1 (nth txti lst1)))) (atof (cdr(assoc 1 (nth txti lst2)))))))
(if ptkq
  (progn
  (if (< (- (cadr (cdr(assoc 10 (nth 0 lst1)))) (cadr (cdr(assoc 10 (nth 1 lst1))))) 
     (- (car (cdr(assoc 10 (nth 1 lst1)))) (car (cdr(assoc 10 (nth 0 lst1)))))) 
  (setq ptkqi (list (car (cdr(assoc 10 (nth txti lst1)))) (cadr ptkq)))
  (setq ptkqi (list (car ptkq) (cadr (cdr(assoc 10 (nth txti lst1)))))))
  (command "textstyle" (cdr(assoc 7 (nth txti lst1))) "osmode" 0 "angbase" 0 "color" 1)
  (command "text" ptkqi (cdr(assoc 40 (nth txti lst1))) (/ (* 180 (cdr(assoc 50 (nth txti lst1)))) pi) (rtos kqi 2 lam_tron))
  );progn
  (entmod (subst (cons 1 (rtos kqi 2 lam_tron)) (assoc 1 (nth txti lst3)) (nth txti lst3)))
);if
(setq txti (1+ txti))
);while
;----------------------------------
(command "ucs" "p")
(setvar "textstyle" olstyle)
(setvar "Dimzin" Ladim)
(setvar "CECOLOR" olcol) 
(setvar "angbase" angbs)
(setvar "osmode" oldos)
(command "undo" "e")
(setvar "cmdecho" cmd)
(princ)
)

