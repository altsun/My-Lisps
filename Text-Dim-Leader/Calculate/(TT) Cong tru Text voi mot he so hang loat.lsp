; Nguồn: cadviet.com
; Chỉnh sửa: Phạm Thế Dương, duongphamhn97@gmail.com

(defun c:tt (/ els ent i new sst str val)
(or delta (setq delta 0.))

(if (and (princ "\nQuet chon Text...!")

(setq sst (ssget '((0 . "*TEXT"))))

(setq delta (cond ((getreal (strcat "\nNhap so gia <" (rtos delta 2 2) ">: ")))

(delta))

)

(setq lam_tron (getint "Ban muon lam tron may chu so sau dau phay?") )

)

(repeat (setq i (sslength sst))

(setq ent (ssname sst (setq i (1- i)))

els (entget ent)

str (cdr (assoc 1 els)))

(and (setq val (distof str))

(setq new (+ val delta))

(setq els (subst (cons 1 (rtos new 2 lam_tron)) (assoc 1 els) els))

(entmod els))))

(princ))