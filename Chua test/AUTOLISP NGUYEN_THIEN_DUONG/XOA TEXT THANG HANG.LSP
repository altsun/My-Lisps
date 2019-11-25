(defun c:XTTH (/ Pnt ssText L1  L2 EnameT STT loop )
(vl-load-com)
(setvar "CMDECHO" 0)
(setq ssText (ssget (list (cons 0  "TEXT"))))
(setq L1 (acet-ss-to-list ssText))
(setq loop T)
(setq STT 1)
(while loop
(setq Pnt (TD:Text-Base  (car (entsel "\nChon Text chuan lam mau: "))))
(cond
	(T
		(if Pnt
			(progn
				(setq L2 (vl-remove nil (mapcar '(lambda (x) (if (or (equal (car Pnt) (car (TD:Text-Base x)) 0.000000001) (equal (cadr Pnt) (cadr (TD:Text-Base x)) 0.000000001)) x nil)) L1)))
				(foreach e L2
					(entdel e)
				)
			)
			(setq loop nil)
		)
	)
)
  	(setq STT (1+ STT))
  	
)
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

