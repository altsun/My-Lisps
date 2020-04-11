(vl-load-com)

(defun c:RTT ( / ent e len c_osmode p1 p2 p3 p4 temp_len num_steel)
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

	(if (<= len 1200)
		; If line's length <= 1200, place 2 steel in 2 endpoints
		(progn
			(rai_thep p1 p1 p2)
			(rai_thep p2 p1 p2)
		)
		; Else
		(progn
			; Determine points between 2 endpoints and 100 unit distance from last point
			(setq p3 (polar p1 (angle p1 p2) 100))
			(setq p4 (polar p2 (angle p2 p1) 100))
			; Place first and last steels
			(rai_thep p3 p1 p2)
			(rai_thep p4 p1 p2)
			; Calculate number of steel
			(setq temp_len (- len 200))
			(setq num_steel (/ temp_len 1200))
			(setq num_steel (ceil num_steel))
			; Place remained steels
			(rai_thep_tren_line p1 p2 p3 p4 num_steel)
		)
	)

	; Restore osnap
	(setvar "osmode" c_osmode)
)

(defun rai_thep (position p1 p2 / pt)
	(setvar "clayer" "0")  ; Set current layer to "0"
	(cond
		((= position p1)
			(setq pt (polar position (+ (angle p1 p2) (/ pi 2)) 100))
			(command "._xline" position pt "")
		)
		((= position p2)
			(setq pt (polar position (- (angle p2 p1) (/ pi 2)) 100))
			(command "._xline" position pt "")
		)
		(t
			(setq pt (polar position (+ (angle p1 p2) (/ pi 2)) 100))
			(command "._xline" position pt "")
		)
	)
)

(defun rai_thep_tren_line (p1 p2 p3 p4 num / pt)
	(setq l (/ (distance p3 p4) num))
	(repeat (- num 1)
		(setq pt (polar p3 (angle p3 p4) l))
		(rai_thep pt p1 p2)
		(setq p3 pt)
	)
)

(defun ceil (x / n)
	(if (or (= (setq n (fix x)) x) (< x 0))
		n
		(1+ n)
	)
)
