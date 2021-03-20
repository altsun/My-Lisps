;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/topic/13203-viet-lisp-theo-yeu-cau-phan-2/page-29

(defun c:sxt()
(vl-load-com)
  ;copyright by Tue_NV
  (setq ss (ssget '((0 . "*TEXT"))) i 0 lispobj (list))
  (setq kc (getdist "\n Khoang cach giua cac Text :"))
  (while (< i (sslength ss))
    (vla-getboundingbox (setq obj (vlax-ename->vla-object (ssname ss i))) 'bl 'tl)
    	(setq lispobj (cons (cons (safearray-value bl) obj) lispobj))
    (setq i (1+ i))
    )
  (setq lispobj (vl-sort lispobj
			 '(lambda (x y)
			    (< (caar x) (caar y))
			  )
		)
	)
  (setq i 0)
(foreach x lispobj
  (setq des (list (+ (caaar lispobj) (* i kc)) (cadar x) 0))
  (vla-move (cdr x) (vlax-3d-point (car x)) (vlax-3d-point (caar lispobj)))
  (vla-move (cdr x) (vlax-3d-point (caar lispobj)) (vlax-3d-point des))
	  (setq i (1+ i))
  )
  (princ "Chuc ban lam viec hieu qua _ Tue_NV")
  )
