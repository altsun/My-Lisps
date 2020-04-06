;; YEU CAU: https://www.cadviet.com/forum/topic/180202-nh%E1%BB%9D-vi%E1%BA%BFt-lisp/

(vl-load-com)

(defun c:RTT ()
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
			(rai_thep p1)
			(rai_thep p2)
		)
		; Else
		(progn
			; Determine points between 2 endpoints and 100 unit distance from last point
			(setq p3 (polar p1 (angle p1 p2) 100))
			(setq p4 (polar p2 (angle p2 p1) 100))
			; Place first and last steels
			(rai_thep p3)
			(rai_thep p4)
			; Calculate number of steel
			(setq temp_len (- len 200))
			(setq num_steel (/ temp_len 1200))
			(setq num_steel (ceil num_steel))
			; Place remained steels
			(rai_thep_tren_line p3 p4 num_steel)
		)
	)

	; Restore osnap
	(setvar "osmode" c_osmode)
)

(defun rai_thep (position)
	(command "._circle" position 20)
	(command "._bhatch" "S" (entlast) "" "P" "solid" "")
)

(defun rai_thep_tren_line (p3 p4 num)
	(setq l (/ (distance p3 p4) num))
	(repeat (- num 1)
		(setq pt (polar p3 (angle p3 p4) l))
		(rai_thep pt)
		(setq p3 pt)
	)
)

(defun ceil (x / n)
	(if (or (= (setq n (fix x)) x) (< x 0))
		n
		(1+ n)
	)
)
