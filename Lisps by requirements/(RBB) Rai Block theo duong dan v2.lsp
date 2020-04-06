(vl-load-com)

(defun rai_block_theo_duong_dan ()
    ; Select objects to place
    (princ "\nChon cac doi tuong rai: ")
    (setq ss (ssget))

    (cond 
        ((= ss nil) (princ "\nChua chon duoc doi tuong nao"))
        ((/= ss nil)
            (setq dsl (sslength ss))
            ; (setq dc (car (LM:boundingbox ss)))
            (setq dc (car (LM:ssboundingbox ss)))
            ; (setq dc (getpoint "\nChon diem goc: "))
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
    (setq dist 440)
    (setq num  (/ path_len dist))
    (setq num (ceil num))
    (place_obj num dist)
)

(defun place_obj (num dist)
    (setq index -1)

    (repeat num
        (setq index (1+ index))
        (setq d2 (* dist index))
        (setq p2 (vlax-curve-getPointAtDist chondd d2))
        (copykhongquay)
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

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (cons (vlax-safearray->list llp) ls1)
                  ls2 (cons (vlax-safearray->list urp) ls2)
            )
        )
    )
    (if (and ls1 ls2)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list ls1 ls2))
    )
)

(defun *error* (msg)
    (vl-bt)
)

(defun c:RBB ()
    (rai_block_theo_duong_dan)
)