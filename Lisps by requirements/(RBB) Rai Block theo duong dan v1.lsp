(vl-load-com)

(defun rai_block_theo_duong_dan ()
    ; Select objects to place
    (princ "\nChon cac doi tuong rai: ")
    (setq ss (ssget))

    (cond 
        ((= ss nil) (princ "\nChua chon duoc doi tuong nao"))
        ((/= ss nil)
            (setq dsl (sslength ss))
            (setq dc (getpoint "\nChon diem goc: "))
        )
    )

    ; Select path
    (setq ddd (entsel "\nChon duong dan:"))
    (while
        (or
            (null ddd)
            (or (= "TEXT" (cdr (assoc 0 (entget (car ddd))))) (= "MTEXT" (cdr (assoc 0 (entget (car ddd))))) (= "HATCH" (cdr (assoc 0 (entget (car ddd))))) (= "INSERT" (cdr (assoc 0 (entget (car ddd))))) (= "REGION" (cdr (assoc 0 (entget (car ddd))))) (= "DIMENSION" (cdr (assoc 0 (entget (car ddd))))))
        )
        (setq ddd (entsel "\nDoi tuong khong the lam duong dan! Chon lai"))
    )

    (setq chondd (car ddd))

    ; Turn off osnap
    (setq c_osmode (getvar "osmode"))
    (setvar "osmode" 0)

    (setq path_len (vlax-curve-getDistAtParam chondd (vlax-curve-getEndParam chondd)))
    (setq start_point (vlax-curve-getPointAtDist chondd 0))
    (setq end_point (vlax-curve-getPointAtDist chondd path_len))

    (place_by_dist path_len)

    ; Delete selection set
    (command "._erase" ss "")

    ; Restore osnap
    (setvar "osmode" c_osmode)
)

(defun place_by_dist (path_len)
    (setq dist (getdist "\nKhoang cach doan chia: "))
    (setq num  (/ path_len dist))
    (setq num (ceil num))
    (place_obj num dist)
)

(defun place_obj (num dist)
    (setq quaykhong (strcase (getstring "\nCo quay doi tuong theo duong dan khong? K/<C>")))
    (cond
        ((= quaykhong "K") (setq copy copykhongquay))
        ((/= quaykhong "K")(setq copy copyquay))
    ) 

    (setq index -1)

    (repeat num
        (setq index (1+ index))
        (setq d2 (* dist index))
        (setq p2 (vlax-curve-getPointAtDist chondd d2))
        (copy)
    )
)

(defun copycuoiquay()
    (setq d5 (- path_len 0.001))
    (setq p5 (vlax-curve-getPointAtDist chondd d5))
    (setq L 0)
    (setq M (sslength ss))
    (while (< L M)
        (setq DT (ssname ss L))
        (command ".copy" DT "" dc p5)
        (command ".rotate" "last" "" start_point p5)
        (command ".rotate" "last" "" end_point 180)
        (command "._move" "last" "" start_point dc)
        (setq L (1+ L))
    )
)

(defun copyquay ()
    (setq d3 (+ (* dist index) 0.001))
    (setq p3 (vlax-curve-getPointAtDist chondd d3))
    (cond
        ((= p3 nil) (copycuoiquay))
        ((/= p3 nil) 
            (setq L 0)
            (setq M (sslength ss))
            (while (< L M)
                (setq DT (ssname ss L))
                (command ".copy" DT "" dc p2)
                (command ".rotate" "last" "" p2 p3)
                (command "._move" "last" "" start_point dc)
                (setq L (1+ L))
            )
        )
    )
)

(defun copykhongquay ()
    (command ".copy" ss "" dc p2 "")
    (command "._move" "last" "" start_point dc)
)

(defun ceil (x / n)
	(if (or (= (setq n (fix x)) x) (< x 0))
		n
		(1+ n)
	)
)

(defun *error* (msg)
    (vl-bt)
)

(defun c:RBB ()
    (rai_block_theo_duong_dan)
)