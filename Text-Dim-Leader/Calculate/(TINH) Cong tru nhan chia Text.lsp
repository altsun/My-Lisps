;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/4077-y%C3%AAu-c%E1%BA%A7u-lisp-c%E1%BB%99ng-tr%E1%BB%AB-nh%C3%A2n-chia-text/
(defun c:tinh()
 (vl-load-com)
 (initget 1 "+ - * /")
 (setq ptinh (getkword "Chon phep tinh <+ - * />: "))

 (cond ((= ptinh "+")  ;;; cong
 (prompt "\nChon text de cong:")
 (setq ss (ssget '((0 . "TEXT")))
       kqua 0)
 (while (and ss (> (sslength ss) 0))
   (setq kqua (+ kqua (atof (cdr (assoc 1 (entget (setq ent (ssname ss 0))))))))
   (ssdel ent ss))
 (princ kqua))

((= ptinh "*")  ;;;nhan
 (prompt "\nChon text de nhan:")
 (setq ss (ssget '((0 . "TEXT")))
       kqua 1)
 (while (and ss (> (sslength ss) 0))
   (setq kqua (* kqua (atof (cdr (assoc 1 (entget (setq ent (ssname ss 0))))))))
   (ssdel ent ss))
 (princ kqua))

((= ptinh "-")  ;;;tru
 (setq sobitru (car (entsel "\nChon so bi tru:"))
       sotru (car (entsel "\nChon so tru:\n"))
       kqua (- (atof (cdr (assoc 1 (entget sobitru))))
	     (atof (cdr (assoc 1 (entget sotru))))))	  
 (princ kqua))

((= ptinh "/")  ;;;chia
 (setq sobichia (car (entsel "\nChon so bi chia:"))
       sochia (car (entsel "\nChon so chia:\n"))
       kqua (/ (atof (cdr (assoc 1 (entget sobichia))))
	     (atof (cdr (assoc 1 (entget sochia))))))	  
 (princ kqua))	
 )  
 (if (not ssle) (setq ssle 0))
 (setq obj (vlax-ename->vla-object (car (entsel "\nChon text de ghi ket qua:")))
	ssle1 (getint (strcat "\nSo so le <" (itoa ssle) ">: ")))
 (if ssle1 (setq ssle ssle1))
 (vla-put-TextString obj (rtos kqua 2 ssle))  
 (princ)	       
)
