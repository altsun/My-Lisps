(defun c:ltruc(/ oldos dai rong ang i j ssx ssy ltx lty ent dau cuoi ang i
		 minx miny maxx maxy i1 i2 i3 i4 pt1 pt2 pt3 pt4 in)
(vl-load-com)
;copyright by Tue_NV
(command "undo" "be")
(setq oldos (getvar "osmode"))
(setvar "osmode" 0)
(Setq dai (getdist "\n chieu dai cot theo phuong X :"))
(Setq rong (getdist "\n chieu rong cot theo phuong Y :"))
(setq ang 0)
(setq ss (ssget '((0 . "*LINE"))) i 0 j 0 
	 ssx (ssadd) ssy (ssadd) ltx nil lty nil)

(Repeat (sslength ss)
(setq ent (ssname ss i))
(setq dau (vlax-curve-getStartPoint ent))
(setq cuoi (vlax-curve-getEndPoint ent))

	(if (or (= (angle dau cuoi) ang) (= (angle dau cuoi) (+ ang pi)) )
	  (progn
	        (if (= (angle dau cuoi) pi) 
			(progn
				(setq dau (vlax-curve-getEndPoint ent)) 
				(setq cuoi (vlax-curve-getStartPoint ent)) 
			)
		)
		(setq ssx (ssadd ent ssx))
		(setq ltx (append ltx (list (list dau cuoi ent))))
	  )
	)

	(if (or (= (angle dau cuoi) (+ ang (/ pi 2)) ) (= (angle dau cuoi) (+ ang (/ (* 3 pi) 2))) )
	  (progn
	        (if (= (angle dau cuoi) pi) 
			(progn
				(setq dau (vlax-curve-getEndPoint ent)) 
				(setq cuoi (vlax-curve-getStartPoint ent)) 
			)
		)
		(setq ssy (ssadd ent ssy))
		(setq lty (append lty (list (list dau cuoi ent))))
	  )
	)
(setq i (1+ i))
)
(setq ltx (vl-sort ltx '(lambda (x1 x2) (< (cadr(car x1)) (cadr(car x2))) 
	   		) 
	   )
)
(setq lty (vl-sort lty '(lambda (y1 y2) (< (car(car y1)) (car(car y2))) 
	   		) 
	   )
)
(alert "\n Vui long cho mot chut. Chuong trinh se bo tri luoi cot cho ban")
;Case #1
(setq minx (car ltx) miny (car lty) 
      maxx (nth (1- (length ltx)) ltx)
      maxy (nth (1- (length lty)) lty)
	i1 (car(giaodt (caddr minx) (caddr miny)))
	i2 (car(giaodt (caddr maxx) (caddr maxy)))
	i3 (car(giaodt (caddr maxx) (caddr miny)))
	i4 (car(giaodt (caddr maxy) (caddr minx)))
      pt1 (list (+ (car i1) dai) (+ (cadr i1) rong) 0)
      pt2 (list (- (car i2) dai) (- (cadr i2) rong) 0)
      pt3 (list (+ (car i3) dai) (- (cadr i3) rong) 0)
      pt4 (list (- (car i4) dai) (+ (cadr i4) rong) 0)
)
(vecot i1 pt1) 
(vecot i2 pt2)
(vecot i3 pt3)
(vecot i4 pt4)
(setq ltx (vl-remove minx ltx)) (setq ltx (vl-remove maxx ltx))
(setq lty (vl-remove miny lty)) (setq lty (vl-remove maxy lty))

;Case #2 : Intersect minx - y
(foreach x lty
	(setq in (car(giaodt (caddr minx) (caddr x))) 
		pt1 (list (- (car in) (/ dai 2)) (cadr in) 0)
		pt2 (list (+ (car in) (/ dai 2)) (+ (cadr in) rong) 0)
	)
		(vecot pt1 pt2)
);foreach

;Case #3 : Intersect maxx - y
(foreach x lty
	(setq in (car(giaodt (caddr maxx) (caddr x))) 
		pt1 (list (- (car in) (/ dai 2)) (cadr in) 0)
		pt2 (list (+ (car in) (/ dai 2)) (- (cadr in) rong) 0)
	)
		(vecot pt1 pt2)
);foreach

;Case #4 : Intersect miny - x
(foreach x ltx
	(setq in (car(giaodt (caddr miny) (caddr x))) 
		pt1 (list (car in) (- (cadr in) (/ rong 2)) 0)
		pt2 (list (+ (car in) dai) (+ (cadr in) (/ rong 2)) 0)
	)
		(vecot pt1 pt2)
);foreach

;Case #5 : Intersect maxy - x
(foreach x ltx
	(setq in (car(giaodt (caddr maxy) (caddr x))) 
		pt1 (list (car in) (+ (cadr in) (/ rong 2)) 0)
		pt2 (list (- (car in) dai) (- (cadr in) (/ rong 2)) 0)
	)
		(vecot pt1 pt2)
);foreach

;Case #5 : Intersect maxxy - minxy
(foreach x ltx
    (foreach y lty
	(setq in (car(giaodt (caddr x) (caddr y))) 
		pt1 (list (- (car in) (/ dai 2)) (- (cadr in) (/ rong 2)) 0)
		pt2 (list (+ (car in) (/ dai 2)) (+ (cadr in) (/ rong 2)) 0)
	)
		(vecot pt1 pt2)
     );foreach
);foreach
(alert "\n Finish")
(setvar "osmode" oldos)
(command "undo" "be")
(princ)
)
;
(defun vecot(p1 p2)
(command "rectang" p1 p2)
;(command "zoom" "w" p1 p2)
(command "hatch" "solid" (entlast) "")
;(command "zoom" "P")
(princ)
)
;
(defun GiaoDT (ent1 ent2)
(setq ob1 (vlax-ename->vla-object ent1)
ob2 (vlax-ename->vla-object ent2)
)
(setq g (vlax-variant-value
(vla-IntersectWith ob1 ob2 acExtendNone)
)
)
(if (/= (vlax-safearray-get-u-bound g 1) -1)
(setq g (vlax-safearray->list g))
(setq g nil)
)
(if g
(progn
(setq kq nil
sd (fix (/ (length g) 3))
)
(repeat sd
(setq kq (append kq (list (list (car g) (cadr g) (caddr g))))
g (cdddr g)
)
)
kq
)
nil
)

)