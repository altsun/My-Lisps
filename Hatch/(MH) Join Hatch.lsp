;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=30153
(defun c:mh (/ ss entht sl i dt dtht)
  (princ "\nMerge Hatch - Gop 2 hatch rieng biet thanh 1")
  (setq
    ss (ssget '((0 . "HATCH")))
    sl (if ss
	 (sslength ss)
	 0
       )
    i  0
    l  0
  )
  (repeat sl
    (setq
      entht (ssname ss i)
      dtht  (getbdata entht)
      dt    (append dt dtht)
      l	    (+ l (cdr (assoc 91 (entget entht))))
      i	    (1+ i)
    )
  )
  (setq	ent  (ssname ss 0)
	ss   (ssdel ent ss)
	tt   (entget ent)
	duoi (member (assoc 75 tt) tt)
	dau  (reverse (member (assoc 91 tt) (reverse tt)))
	tt   (append dau dt duoi)
	tt   (subst (cons 91 l) (assoc 91 tt) tt)
  )
  (entmod tt)
  (command ".erase" ss "")
  (princ)
)
(defun getbdata	(ent)
  (setq	tt (entget ent)
	tt (cdr (member (assoc 75 tt) (reverse tt)))
	tt (cdr (member (assoc 91 tt) (reverse tt)))
  )
)
(princ "\nHatch Extend by ketxu has started!")
(princ)
