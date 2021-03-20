;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/26670-gi%C3%BAp-em-lisp-tr%E1%BB%AB-2-s%E1%BB%91/
(defun c:-()
 (vl-load-com)  
 (setq sbt (car (entsel "\nChon so bi tru:"))
	   st (car (entsel "\nChon so tru:\n"))
	   kq (- (atof (cdr (assoc 1 (entget sbt))))
		 (atof (cdr (assoc 1 (entget st))))))	  
 (princ kq)
 (setq obj (vlax-ename->vla-object (car (entsel "\nChon text ghi ket qua:"))))
 (vla-put-TextString obj (rtos kq 2 2))
 (princ))