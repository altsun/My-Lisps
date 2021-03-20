(defun c:gt (/ p1 p2 txt etxt d)

(setq p1 (getpoint "\n Chon diem thu nhat")

          p2 (getpoint "\n Chon diem thu hai ")

          txt (car (entsel "\n Chon text can thay" ))

          d (distance p1 p2)

         etxt (entget txt)

         etxt (subst (cons 1 (rtos d 2 2)) (assoc 1 etxt) etxt)

)

(entmod etxt)

(princ)

)