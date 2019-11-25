;; free lisp from cadviet.com

(defun c:hc ()
(setq elist (entget (car (entsel "\nLUA CHON MAU HATCH DE SAO CHEP: ")))
etyp (cdr (assoc 0 elist))
old (getvar "clayer")
);setq

(if (/= etyp "HATCH")
(prompt "\nKHONG PHAI DOI TUONG HATCH! HAY THU LAI!")
(progn
(setq layn (cdr (assoc 8 elist)))
(setvar "HPNAME" (cdr (assoc 2 elist)))
(setvar "HPSCALE" (cdr (assoc 41 elist)))
(setvar "HPANG" (cdr (assoc 52 elist)))
(setq pt1 (getpoint "\nCHON MOT DIEM TRONG VUNG SE HATCH: "))
(setvar "clayer" layn)
(command "bhatch" pt1 "")
(setvar "clayer" old)
);progn
);if
(princ)
);defun
;****** mk ****** mk ****** mk ****** mk ****** mk ****** mk ******
(defun c:hm ()
(setq ename (entsel "\nLUA CHON MAU HATCH DE DI CHUYEN: ")
elist (entget (car ename))
etyp (cdr (assoc 0 elist))
old (getvar "clayer")
);setq

(if (/= etyp "HATCH")
(prompt "\nKHONG PHAI DOI TUONG HATCH! HAY THU LAI!")
(progn
(setq layn (cdr (assoc 8 elist)))
(setvar "HPNAME" (cdr (assoc 2 elist)))
(setvar "HPSCALE" (cdr (assoc 41 elist)))
(setvar "HPANG" (cdr (assoc 52 elist))) 
(setq pt1 (getpoint "\nCHON MOT DIEM TRONG VUNG SE HATCH: "))
(setvar "clayer" layn)
(entdel (car ename))
(command "bhatch" pt1 "")
(setvar "clayer" old)
);progn
);if
(princ)
);defun
;****** mk ****** mk ****** mk ****** mk ****** mk ****** mk ******
(defun c:ha ()
(setq ename(entsel "\nLUA CHON MAU HATCH DE THAY DOI GOC : ")
elist(entget (car ename))
etyp(cdr (assoc 0 elist))
oang(assoc 52 elist)
);setq

(if (/= etyp "HATCH")
(prompt "\nKHONG PHAI DOI TUONG HATCH! HAY THU LAI!")
(progn
(setq ang (getangle (strcat "\nSO DO GOC MOI <" (rtos (cdr oang) 2) ">:"))
nang (cons 52 ang)
nlst (subst nang oang elist)
)
(entmod nlst)
(command "hatchedit" ename "" "" "" "")
);progn
);if
(princ)
);defun
;****** mk ****** mk ****** mk ****** mk ****** mk ****** mk ******
(defun c:hs ()
(setq ename (entsel "\nLUA CHON MAU HATCH DE THAY DOI TY LE : ")
elist (entget (car ename))
etyp (cdr (assoc 0 elist))
oscl (assoc 41 elist)
);setq

(if (/= etyp "HATCH")
(prompt "\nKHONG PHAI DOI TUONG HATCH! HAY THU LAI!")
(progn
(setq hscl (getreal (strcat "\nTY LE MAU HATCH MOI <" (rtos (cdr oscl) 2) ">:"))
nscl (cons 41 hscl)
nlst (subst nscl oscl elist)
)
(entmod nlst)
(command "hatchedit" ename "" "" "" "")
);progn
);if
(princ)
);defun
