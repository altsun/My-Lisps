;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/181340-nh%E1%BB%9D-vi%E1%BA%BFt-lisp-c%C4%83n-d%C3%A3n-kho%E1%BA%A3ng-c%C3%A1ch-gi%E1%BB%AFa-c%C3%A1c-%C4%91%E1%BB%91i-t%C6%B0%E1%BB%A3ng/
(Defun c:te (/ ss p1 p2 x a e lst)
  (setq lst (list))
  (Setq ss (ssget ))
  
  (setq p1 (getpoint  "pick diem chen hoac diem goc"))
 (if  (setq p2 (getpoint p1 "pick phuong, khoang cach")) (progn
  (setq x (distance p1 p2) a (angle p1 p2))
  (while (setq e (ssname ss 0))
    (setq ss (ssdel e ss))
    (command "copy" e "" "_NON" (cdr (assoc 10 (entget e))) "_NON" p1)
    (setq p1 (polar p1 a x)))
  ) (progn
   (setq lst (append lst (list (list ss p1))))
   (while (and (setq ss (ssget))
	       (setq p1 (getpoint "Pick diem goc")))
     (setq lst (append lst (list (list ss p1))))
     )
   (setq p1 (getpoint  "pick diem chen")
	 p2 (getpoint p1 "pick phuong, khoang cach"))
   (setq x (distance p1 p2) a (angle p1 p2))
   (while (setq l1 (car lst))
     (setq lst (cdr lst))
    (command "copy" (car l1) "" "_NON" (cadr l1) "_NON" p1)
    (setq p1 (polar p1 a x)))

   )))