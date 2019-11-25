; Chỉnh sửa: Phạm Thế Dương, duongphamhn97@gmail.com

(defun c:+c (/ b p1 p2 cong value dimzin)
(defun *error* (msg) (and dimzin (setvar "dimzin" dimzin)) (setq *error* nil) (princ))
(while (null (setq b (ssget ":S" '((0 . "TEXT") (1 . "~*[~0-9]*,~*[~0-9`.0-9]*")))))
 (princ "\nNot number_Chosen again")
);_ end while
(setq dimzin (getvar "dimzin"))
(setvar "dimzin" 0)
(setq b     (ssname b 0)
      value (cdr (assoc 1 (entget B)))
);_ end setq

(setq lam_tron (getint "Ban muon lam tron may chu so sau dau phay?") )

(or *cong* (setq *cong* 1.0))
(or (setq cong (getreal (strcat "\nInput number plus: <" (rtos *cong* 2 lam_tron) ">")))
    (setq cong *cong*)
);_ end or
(setq *cong* cong)



(setq p1 (getpoint "\nStart Point "))
(while (setq p2 (getpoint p1 "\nNext Point "))
 (command "copy" b "" p1 p2)
 (setq value (rtos (+ cong (atof value)) 2 lam_tron))
 (entmod (subst (cons 1 value) (assoc 1 (entget (entlast))) (entget (entlast))))
);_ end while
(setvar "dimzin" dimzin)
(setq *error* nil)
(princ)
);_ end defun