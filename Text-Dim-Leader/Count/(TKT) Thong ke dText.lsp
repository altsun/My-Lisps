;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/topic/14684-cho-em-xin-lisp-dem-text/
(defun c:tkt (/ lst msp pt ss str txtsiz)  (vl-load-com)  (setq msp (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))) lst (list))  (prompt (strcat "\nChon Text de Liet ke hay ENTER de chon tat ca :"))  (if (null (setq ss (ssget(list (cons 0 "TEXT"))))) (setq ss (ssget "_X" (list(cons 410 (getvar "Ctab")) (cons 0 "TEXT")))))  (if ss    (progn      (foreach e (mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))	(setq str (vla-get-TextString e))	(if (not (assoc str lst))	  (setq lst (cons (cons str 1) lst))	  (setq lst (subst (cons str (1+ (cdr (assoc str lst))))			   (assoc str lst) lst)))	)      (setq lst (vl-sort lst '(lambda (x y) (< (cdr x) (cdr y))))	    pt (getpoint "\nDiem dat Bang :" )	    txtsiz (* (getvar "dimtxt")(getvar "dimscale")))      (foreach e lst	(vla-addtext msp (cdr e) (vlax-3d-point pt) txtsiz )	(vla-addtext msp (car e) (vlax-3d-point (polar pt 0 (* 5 txtsiz))) txtsiz )	(setq pt (polar pt (/ pi -2) (* 1.5 txtsiz)))	)      )    (alert "Khong chon duoc Text.")    )  (princ))