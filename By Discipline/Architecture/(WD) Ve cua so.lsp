; A lisp by Phạm Thế Dương
; duongphamhn97@gmail.com

(defun c:wd()
	(setq p1 (getpoint "Enter the point 1"))
	(setq p2 (getpoint "Enter the point 2"))
	(command "_rectangle" p1 p2)

	(setq x1 (car p1))
	(setq y1 (cadr p1))
	(setq x2 (car p2))
	(setq y2 (cadr p2))

	(if (> (abs (- x1 x2)) (abs (- y1 y2)) )
		(progn
			(setq y_mid (/ (+ y1 y2) 2))
			(command ".line" (list x1 y_mid) (list x2 y_mid) "")
		)
		(progn
			(setq x_mid (/ (+ x1 x2) 2))
			(command ".line" (list x_mid y1) (list x_mid y2) "")
		)
	)
)