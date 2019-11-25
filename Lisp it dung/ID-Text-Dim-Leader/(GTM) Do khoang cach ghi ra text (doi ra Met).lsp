(defun c:gtm (/ p1 p2 txt etxt d)

(setq p1 (getpoint "\n Chon diem thu nhat")

          p2 (getpoint "\n Chon diem thu hai ")

          txt (car (entsel "\n Chon text can thay" ))

          d (distance p1 p2)

          dm (/ d 1000)  // dm = d / 1000

         etxt (entget txt)

         etxt (subst (cons 1 (rtos dm 2 1)) (assoc 1 etxt) etxt)

)

(entmod etxt)

(princ)

)