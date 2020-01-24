; A lisp by Phạm Thế Dương
; duongphamhn97@gmail.com

(defun c:sa ( / p1 p2 p3 x1 y1 x3 y3 x4 y4)
	(setq p3 nil)
	(setq p1 (getpoint "Pick the start point"))
	(command ".circle" p1 50)
	(command "-bhatch" "S" (entlast) "" "P" "solid" "")
	(while (setq p2 (getpoint p1 "\nPick next point: "))  ; p1 is reference point
		(command ".line" p1 p2 "")
		(setq p3 p1)
		(setq p1 p2)
	)
	(if (/= p3 nil)
		(progn
			(setq x1 (car p1))
			(setq y1 (cadr p1))
			(setq x3 (car p3))
			(setq y3 (cadr p3))
			; Distance between 2 last points
			(setq distance (sqrt (+ (* (- x1 x3) (- x1 x3)) (* (- y1 y3) (- y1 y3))) ) )
			; Determine point between 2 last points and 300 unit distance from last point
			(setq x4 (+ x1 (* (/ 300 distance) (- x3 x1))))
			(setq y4 (+ y1 (* (/ 300 distance) (- y3 y1))))
			; Draw line between that point and last point
			(command ".line" (list x1 y1)  (list x4 y4) "")
			; Rotate that line and mirror it
			(command ".rotate" (entlast) "" p1 "60")
			(command ".mirror" (entlast) "" p1 p3 "")
		)
	)
)