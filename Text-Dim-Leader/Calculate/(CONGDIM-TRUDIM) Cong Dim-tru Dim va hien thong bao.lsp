;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/205-vi%E1%BA%BFt-lisp-theo-y%C3%AAu-c%E1%BA%A7u/?page=84&tab=comments#comment-62720
(defun c:congdim(/ ss n i S duyet ent sst nt j St duyett entt Skq)
(prompt "\n Chon cac Dim cong:")
(setq ss (ssget '((0 . "DIMENSION"))))

(setq n (sslength ss) i 0 S 0 duyet 0)

(while (< i n)
(setq ent (entget(ssname ss i)))

(if (= (cdr(assoc 1 ent)) "")
(setq duyet (cdr(assoc 42 ent)))
(setq duyet (atof(cdr(assoc 1 ent))))
)
(setq S (+ S duyet))
(setq i (1+ i))
)


(alert (rtos S 2 0))

(princ)
)
(defun c:Trudim(/ ss n i S duyet ent sst nt j St duyett entt Skq)
(prompt "\n Chon cac Dim lam so bi tru :")
(setq ss (ssget '((0 . "DIMENSION"))))

(prompt "\n Chon cac Dim lam so tru :")
(setq sst (ssget '((0 . "DIMENSION"))))

(setq n (sslength ss) i 0 S 0 duyet 0)
(setq nt (sslength sst) j 0 St 0 duyett 0)

(while (< i n)
(setq ent (entget(ssname ss i)))

(if (= (cdr(assoc 1 ent)) "")
(setq duyet (cdr(assoc 42 ent)))
(setq duyet (atof(cdr(assoc 1 ent))))
)
(setq S (+ S duyet))
(setq i (1+ i))
)

(while (< j nt)
(setq entt (entget(ssname sst j)))

(if (= (cdr(assoc 1 entt)) "")
(setq duyett (cdr(assoc 42 entt)))
(setq duyett (atof(cdr(assoc 1 entt))))
)
(setq St (+ St duyett))
(setq j (1+ j))
)

(setq Skq (- S St))

(alert (rtos Skq 2 0))

(princ)
)
