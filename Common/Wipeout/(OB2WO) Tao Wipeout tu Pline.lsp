;;; OB2WO (gile) -Gilles Chanteau- 10/03/07
;;; Creates a "Wipeout" from an object (circle, ellipse, or polyline with arcs)
;;; Works whatever the current ucs and object OCS
;;; http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/wipeout-with-arcs/m-p/786490#M12148

(defun c:ob2wo (/ ent lst nor)
  (vl-load-com)
  (if (and (setq ent (car (entsel)))
	   (member (cdr (assoc 0 (entget ent)))
		   '("CIRCLE" "ELLIPSE" "LWPOLYLINE")
	   )
	   (setq lst (ent2ptlst ent))
	   (setq nor (cdr (assoc 210 (entget ent))))
      )
    (progn
      (vla-StartundoMark
	(vla-get-ActiveDocument (vlax-get-acad-object))
      )
      (makeWipeout lst nor)
      (initget "Yes No")
      (if
	(= (getkword "\nDelete source object? [Yes/No] <No>: ")
	   "Yes"
	)
	 (entdel ent)
      )
      (vla-EndundoMark
	(vla-get-ActiveDocument (vlax-get-acad-object))
      )
    )
  )
)


;;; ENT2PTLST
;;; Returns the vertices list of the polygon figuring the curve object
;;; Coordinates defined in OCS

(defun ent2ptlst (ent / obj dist n lst p_lst prec)
  (vl-load-com)
  (if (= (type ent) 'ENAME)
    (setq obj (vlax-ename->vla-object ent))
  )
  (cond
    ((member (cdr (assoc 0 (entget ent))) '("CIRCLE" "ELLIPSE"))
     (setq dist	(/ (vlax-curve-getDistAtParam
		     obj
		     (vlax-curve-getEndParam obj)
		   )
		   50
		)
	   n	0
     )
     (repeat 50
       (setq
	 lst
	  (cons
	    (trans
	      (vlax-curve-getPointAtDist obj (* dist (setq n (1+ n))))
	      0
	      (vlax-get obj 'Normal)
	    )
	    lst
	  )
       )
     )
    )
    (T
     (setq p_lst (vl-remove-if-not
		   '(lambda (x)
		      (or (= (car x) 10)
			  (= (car x) 42)
		      )
		    )
		   (entget ent)
		 )
     )
     (while p_lst
       (setq
	 lst
	  (cons
	    (append (cdr (assoc 10 p_lst))
		    (list (cdr (assoc 38 (entget ent))))
	    )
	    lst
	  )
       )
       (if (/= 0 (cdadr p_lst))
	 (progn
	   (setq prec (1+ (fix (* 25 (sqrt (abs (cdadr p_lst))))))
		 dist (/ (- (if	(cdaddr p_lst)
			      (vlax-curve-getDistAtPoint
				obj
				(trans (cdaddr p_lst) ent 0)
			      )
			      (vlax-curve-getDistAtParam
				obj
				(vlax-curve-getEndParam obj)
			      )
			    )
			    (vlax-curve-getDistAtPoint
			      obj
			      (trans (cdar p_lst) ent 0)
			    )
			 )
			 prec
		      )
		 n    0
	   )
	   (repeat (1- prec)
	     (setq
	       lst (cons
		     (trans
		       (vlax-curve-getPointAtDist
			 obj
			 (+ (vlax-curve-getDistAtPoint
			      obj
			      (trans (cdar p_lst) ent 0)
			    )
			    (* dist (setq n (1+ n)))
			 )
		       )
		       0
		       ent
		     )
		     lst
		   )
	     )
	   )
	 )
       )
       (setq p_lst (cddr p_lst))
     )
    )
  )
  lst
)


;;; MakeWipeout creates a "wipeout" from a points list and the normal vector of the object

(defun MakeWipeout (pt_lst nor / dxf10 max_dist cen dxf_14)
  (if (not (member "acismui.arx" (arx)))
    (arxload "acismui.arx")
  )
  (setq	dxf10 (list (apply 'min (mapcar 'car pt_lst))
		    (apply 'min (mapcar 'cadr pt_lst))
		    (caddar pt_lst)
	      )
  )
  (setq
    max_dist
     (float
       (apply 'max
	      (mapcar '- (apply 'mapcar (cons 'max pt_lst)) dxf10)
       )
     )
  )
  (setq cen (mapcar '+ dxf10 (list (/ max_dist 2) (/ max_dist 2) 0.0)))
  (setq
    dxf14 (mapcar
	    '(lambda (p)
	       (mapcar '/
		       (mapcar '- p cen)
		       (list max_dist (- max_dist) 1.0)
	       )
	     )
	    pt_lst
	  )
  )
  (setq dxf14 (reverse (cons (car dxf14) (reverse dxf14))))
  (entmake (append (list '(0 . "WIPEOUT")
			 '(100 . "AcDbEntity")
			 '(100 . "AcDbWipeout")
			 '(90 . 0)
			 (cons 10 (trans dxf10 nor 0))
			 (cons 11 (trans (list max_dist 0.0 0.0) nor 0))
			 (cons 12 (trans (list 0.0 max_dist 0.0) nor 0))
			 '(13 1.0 1.0 0.0)
			 '(70 . 7)
			 '(280 . 1)
			 '(71 . 2)
			 (cons 91 (length dxf14))
		   )
		   (mapcar '(lambda (p) (cons 14 p)) dxf14)
	   )
  )
)