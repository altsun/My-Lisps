(defun C:NLTSTT (/ ss lst lstPt);Noi Line
	(setq ss (ssget (list (cons 0 "TEXT"))))
	(setq lst (HV:ss->ent ss))
	(setq lst (vl-sort lst (function (lambda (e1 e2)(< (atof (dxf 1 e1)) (atof (dxf 1 e2)))))))
	(setq lstPt (mapcar '(lambda (x) (dxf 10 x)) lst))
	(MakeLWPolyline lstPt nil nil nil nil nil nil)
(princ)
)
(defun HV:ss->ent ( ss / i l )
	(if ss
		(repeat (setq i (sslength ss))
			(setq l (cons (ssname ss (setq i (1- i))) l))
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dxf (code e /)
  (cdr (assoc code (entget e)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MakeLWPolyline (listpoint closed Linetype LTScale Layer Color xdata / Lst)
(setq Lst (list (cons 0 "LWPOLYLINE")
		(cons 100 "AcDbEntity")
		(cons 8 (if Layer Layer (getvar "Clayer")))
		(cons 6 (if Linetype Linetype "bylayer"))
		(cons 48 (if LTScale LTScale 1))
		(cons 62 (if Color Color 256))
		(cons 100 "AcDbPolyline")
		(cons 90 (length listpoint))
		(cons 70 (if closed 1 0))))
		(foreach PP listpoint (setq Lst (append Lst (list (cons 10 PP)))))
		(if xdata (setq Lst (append lst (list (cons -3 (list xdata))))))
		(entmakex Lst)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


