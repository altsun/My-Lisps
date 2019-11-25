;;;;;;;;;;;;;;;;;;;chuyen bang thong ke tu cad sang excel;;;;;;;;;;;;

(defun c:c2e ( / hangdau)
  (defun sosanh	(e1 e2 / p1 p2)
    (setq p1 (car e1)
	  p2 (car e2)
    )
    (if	(equal (cadr p1) (cadr p2) fuzz)
      (< (car p1) (car p2))
      (> (cadr p1) (cadr p2))
    )
  )
  (setq
    ss	    (ssget '((0 . "TEXT")))    
    lst	    (ss2ent ss)
    lst     (mapcar '(lambda (e) (cons (cdr (assoc 10 (entget e))) (cdr (assoc 1 (entget e))))) lst)  
    caotext (cdr (assoc 40 (entget (ssname ss 0))))
    fuzz    (* caotext 1.0)
    lst	    (vl-sort lst 'sosanh)
    index   1
    oldy nil
    fn      (getfiled "Chon file de save" "" "csv" 1)
    fid     (open fn "w")
  )
  
  (foreach e lst
    (if	(equal oldy (cadr (car e)) fuzz)
      (progn        
	(princ "," fid)
	(setq index (1+ index))
      )      
      (progn
	(if hangdau
	  (progn
	    (setq index 1)	
	    (princ "\n" fid)	    
	  )
	  (setq hangdau t)
	 )
      )
    )
    (princ (cdr e) fid)
    (setq oldy (cadr (car e)))
  )
  (close fid)
)

(defun ss2ent (ss / sodt index lstent)
  (setq
    sodt  (if ss
	    (sslength ss)
	    0
	  )
    index 0
  )
  (repeat sodt
    (setq ent	 (ssname ss index)
	  index	 (1+ index)
	  lstent (cons ent lstent)
    )
  )
  (reverse lstent)
)
