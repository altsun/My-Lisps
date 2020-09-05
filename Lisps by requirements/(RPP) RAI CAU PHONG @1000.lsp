(vl-load-com)

(defun c:RPP ( / ent e len c_osmode p1 p2 p3 p4 temp_len num_CP)
	; Pick line
	(if (and (setq ent (car (entsel "\nPick line: \n")))
   		(vlax-property-available-p (setq e (vlax-ename->vla-object ent)) 'length)
   	)
		(setq len (vla-get-length e))
   	)

	; Turn off osnap
	(setq c_osmode (getvar "osmode"))
	(setvar "osmode" 0)

	; Find endpoints of line
	(setq p1 (cdr (assoc 10 (entget ent))))
	(setq p2 (cdr (assoc 11 (entget ent))))

	; Find midpoint
	(setq pm (mid-pt p1 p2))

	; Place steel in midpoint
	(rai_CP_1000 pm p1 p2)

	; Determine points between 2 endpoints and 100 unit distance from last point
	(setq p3 (polar p1 (angle p1 p2) 100))
	(setq p4 (polar p2 (angle p2 p1) 100))
	; Place first and last steels
	(rai_CP_1000 p3 p1 p2)
	(rai_CP_1000 p4 p1 p2)

	; Calculate number of steel in half of the line
	(setq temp_len (- (/ len 2) 100))
	(setq num_CP (/ temp_len 1000))
	(setq num_CP (ceil num_CP))
	; Place remained steels
	(rai_CP_tren_line_1000 p1 p2 pm num_CP)

	; Restore osnap
	(setvar "osmode" c_osmode)
)

(defun rai_CP_1000 (place_position p1 p2 / pt)
	(setvar "clayer" "LA-25_CP")  ; Set current layer to "LA-25_CP"
	(cond
		((= place_position p1)
			(setq pt (polar place_position (+ (angle p1 p2) (/ pi 2)) 100))
			(command "._xline" place_position pt "")
		)
		((= place_position p2)
			(setq pt (polar place_position (- (angle p2 p1) (/ pi 2)) 100))
			(command "._xline" place_position pt "")
		)
		(t
			(setq pt (polar place_position (+ (angle p1 p2) (/ pi 2)) 100))
			(command "._xline" place_position pt "")
		)
	)
)

(defun rai_CP_tren_line_1000 (p1 p2 pm num / pt temp_pm)
	(setq l 1000)
	(setq temp_pm pm)
	(repeat (- num 1)
		(setq pt (polar pm (angle pm p1) l))
		(rai_CP_1000 pt p1 p2)
		(setq pm pt)
	)
	(setq pm temp_pm)
	(repeat (- num 1)
		(setq pt (polar pm (angle pm p2) l))
		(rai_CP_1000 pt p1 p2)
		(setq pm pt)
	)
)

(defun ceil (x / n)
	(if (or (= (setq n (fix x)) x) (< x 0))
		n
		(1+ n)
	)
)
(princ)

;; Returns the middle of two points
(defun mid-pt (p1 p2)
 (polar p1 (angle p1 p2) (/ (distance p1 p2) 2.) )
)