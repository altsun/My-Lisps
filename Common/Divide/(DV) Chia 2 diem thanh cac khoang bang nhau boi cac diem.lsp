;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=13203&st=3980
(defun C:DV(/ p1 p2 sk x)
 (setq p1 (getpoint "\nPick diem 1: "))
 (setq p2 (getpoint p1 "\nPick diem 2: "))
 (setq sk (getint "\nNhap so khoang chia: "))
 (setq x 0)
 (repeat (+ sk 1)
  (command "point" (polar p1 (angle p1 p2) (* x (/ (distance p1 p2) sk))))
  (setq x (1+ x))))

