;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=13203&st=1620
(defun  C:CoBlk (/  i ss ls la)
	(setq la (getvar "CLAYER"))
;(setq la "DAN CU")
	(princ "\n Chon Blocks doi mau bylayer: ")
	(setq i 0 ss (ssget '((0 . "INSERT"))))
	(command ".UNDO" "BE")
	(repeat (sslength ss)
		(CoBylayer (ssname ss i))
		(setq i (1+ i))
	)
	(command ".REGEN")
	(command ".UNDO" "E")
	(princ)
)
(defun  CoBylayer (blk /  e el s)
	(setq s (cdr (assoc 2 (entget blk))))
	(if (not (member s ls))
		(progn
			(setq ls (append ls (list s)))
			(setq e (cdr (assoc -2 (tblsearch "BLOCK" s))))
			(while e
				(setq el (entget e))
				(if (= "INSERT" (cdr (assoc 0 el)))
					(CoBylayer e)
				)
				(setq el (subst (cons 8 la) (assoc 8 el) el))
				(entmod (subst (cons 62 256) (assoc 62 el) el))
				(setq e (entnext e))
			)
		)
	)
)
