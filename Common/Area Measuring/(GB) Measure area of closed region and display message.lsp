;; free lisp from cadviet.com
;;; this lisp was downloaded from http://webcache.googleusercontent.com/search?q=cache:R2kCctDqgFAJ:www.cadviet.com/forum/index.php%3Fshowtopic%3D16363+%22t%C3%ADnh+di%E1%BB%87n+t%C3%ADch+nhi%E1%BB%81u+h%C3%ACnh%22&cd=1&hl=vi&ct=clnk&gl=vn&source=www.google.com.vn
(defun c:gb(/ p ss S frome cur toe tt)
(setq p (getpoint "\n Pick 1 diem vao mien trong hinh kin :") 
	ss (ssadd) S 0)
(while p
(setq frome (entlast))
(command ".boundary" p "")
(setq toe (entlast));; 
(setq cur frome
)
    (while (not (eq cur toe))
	(setq cur (entnext cur)
		ss (ssadd cur ss))
	(command "area" "S" "O" ss "" "")
	(setq tt (getvar "area"))
	(setq S (+ S tt))
     )
  (command "area" "A" "O" "L" "" "")
  (setq tt (getvar "area"))
  (setq S (+ S (* tt 2))) 
(sssetfirst ss ss)
(setq p (getpoint "\n Pick 1 diem vao mien trong hinh kin :"))

)
(if (> (sslength ss) 0)
(alert (strcat "Area = " (rtos S 2 2)))
(alert "\n Ban chua Pick vao mien kin nao ca ")
)
(command "erase" ss "")
(Princ)
)

