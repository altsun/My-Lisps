(defun c:brg(/ p1 p2 ss ename1 ename2 tapgiao tapgiaodiem tapgiao2 tapgiaolay )
;;write by Tue_NV
(vl-load-com)
  (while (and (setq p1 (getpoint "\nChon diem thu 1 :") p2 (getcorner p1 "\nChon diem thu 2 :")))
  (if (setq ss (ssget "c" p1 p2))
    (progn
      (setq i 0 j 0)
      (setq tapgiao nil tapgiaodiem nil tapgiao2 nil tapgiaolay nil)
      (while (setq ename1 (ssname ss i))
	(while (setq ename2 (ssname ss j))
	   (setq giaodiem
		    (vlax-invoke
			    (vlax-ename->vla-object ename1)
			       'IntersectWith
			    (vlax-ename->vla-object ename2) 0
		    )
	   )
	   	(setq tapgiaodiem (append tapgiaodiem (list giaodiem)))
	  (setq j (1+ j))
	)	
	(setq tapgiao (append tapgiao tapgiaodiem) tapgiaodiem nil)
	(setq i (1+ i) j 0)
	
        (setq tapgiao2 nil)
        (foreach x (vl-remove nil tapgiao)
	  (if (null (member (VL-PRINC-TO-STRING x) (mapcar 'VL-PRINC-TO-STRING tapgiao2)))
	    (setq tapgiao2 (append tapgiao2 (list x)))
	  )
        )
	(setq tapgiaolay (append tapgiaolay (list (list ename1
							(vl-sort tapgiao2 '(lambda(x y) 
									      	(> (vlax-curve-getparamatpoint ename1 x)
										   (vlax-curve-getparamatpoint ename1 y)
										)
									   )
							)
	))))
	(setq tapgiao nil)	
      )
    );progn
  );if
     
    (FOREACH x tapgiaolay
      (setq i 0)
       ;(while (<= i (/ (length (cadr x)) 2))
       (if (= 0 (rem (length (cadr x)) 2))
         (Repeat (/ (length (cadr x)) 2)
	   (if  (and (null (equal (vlax-curve-getstartpoint (car x)) (nth (1+ i) (cadr x)) 0.001))
		      (null (equal (vlax-curve-getstartpoint (car x)) (nth i (cadr x)) 0.001))
		)
              (command "._break" (car x) "_non" (nth i (cadr x)) "_non" (nth (1+ i) (cadr x)))
	   )
	      (setq i (+ i 2))
         )
       )
    ) 
)
)