;;;**********************************************
;;;CHUONG TRINH DANH SO THU TU VA COPY TANG DAN
;;;1. Lenh OD: danh so thu tu, tuy chon so bat dau (begin) va so gia (increment) tuy y
;;;2. Lenh OC: copy tang dan tu mot so thu tu co san
;;;3. Lenh OCA: copy tang dan voi doi tuong Attribute Block
;;;Chuong trinh chap nhan cac dinh dang bang so, chu, so va chu ket hop:
;;;1, 2... A, B..., A1, A2..., AB-01, AB-02..., AB-01-C1, AB-01-C2...
;;;Cac chu gioi han trong khoang tu A den Z. Cac so khong han che
;;;Copyright by ssg - www.cadviet.com - December 2008
;;;**********************************************


;;;-------------------------------------------------
(defun etype (e) ;;;Entity Type
(cdr (assoc 0 (entget e)))
)
;;;-------------------------------------------------
(defun wtxt (txt p / sty d h) ;;;Write txt on graphic screen, defaul setting
(setq
sty (getvar "textstyle")
d (tblsearch "style" sty)
h (cdr (assoc 40 d))
)
(if (= h 0) (setq h (cdr (assoc 42 d))))
(entmake
(list (cons 0 "TEXT") (cons 7 sty) (cons 1 txt) (cons 10 p) (cons 40 h) (assoc 41 d))
)
)
;;;-------------------------------------------------
(defun incN (n dn / n2 i n1) ;;;Increase number n
(setq
n2 (itoa (+ dn (atoi n)))
i (- (strlen n) (strlen n2))
)
(if (> i 0) (setq n1 (substr n 1 i)) (setq n1 ""))
(strcat n1 n2)
)
;;;-------------------------------------------------
(defun incC (c / i c1 c2) ;;;Increase character c
(setq
i (strlen c)
c1 (substr c 1 (- i 1))
c2 (chr (1+ (ascii (substr c i 1))))
)
(if (or (= c2 "{") (= c2 "["))
(progn (command "erase" (entlast) "") (alert "Over character!") (exit))
(strcat c1 c2)
)
)
;;;============================
(defun C:OD( / cn dn c n p) ;;;Make OrDinal number with any format
(setq
cn (getstring "\nBegin at <1>: " T)
dn (getint "\nIncrement <1>: ")
)
(if (not dn) (setq dn 1))
(if (= cn "") (setq cn "1"))
(setq c (vl-string-right-trim "0 1 2 3 4 5 6 7 8 9" cn))
(setq n (vl-string-subst "" c cn))
(if (/= n "") (setq mode 1) (setq mode 0))
(while (setq p (getpoint "\nBase point : "))
(wtxt cn p)
(if (= n "") 
(setq cn (incC cn))
(setq cn (strcat c (incN (vl-string-subst "" c cn) dn))) 
)
)
(princ)
)
;;;============================
(defun C:OC( / e dn p1 cn c n p2 dat) ;;;Make Ordinal number. Copy from template
(setq
e (car (entsel "\nSelect template text:"))
dn (getint "\nIncrement <1>: ")
p1 (getpoint "\nBase point:")
cn (cdr (assoc 1 (entget e)))
)
(if (not dn) (setq dn 1))
(if (= cn "") (setq cn "1"))
(setq
c (vl-string-right-trim "0 1 2 3 4 5 6 7 8 9" cn)
n (vl-string-subst "" c cn)
)
(while (setq p2 (getpoint p1 "\nNew point : "))
(command "copy" e "" p1 p2)
(if (= n "") 
(setq cn (incC cn))
(setq cn (strcat c (incN (vl-string-subst "" c cn) dn))) 
)
(setq
dat (entget (entlast))
dat (subst (cons 1 cn) (assoc 1 dat) dat)
)
(entmod dat) 
)
(princ)
)
;;;============================
(defun C:OCA( / e e0 dn p1 cn c n p2 dat) ;;;Make Ordinal number. Copy from Atttribute block
(setq
e0 (car (entsel "\nSelect attribute block:"))
e (entnext e0)
)
(if (/= (etype e) "ATTRIB") (progn (alert "Object is not a Attribute Block!") (exit)))
(setq
dn (getint "\nIncrement <1>: ")
p1 (getpoint "\nBase point:")
cn (cdr (assoc 1 (entget e)))
)
(if (not dn) (setq dn 1))
(if (= cn "") (setq cn "1"))
(setq
c (vl-string-right-trim "0 1 2 3 4 5 6 7 8 9" cn)
n (vl-string-subst "" c cn)
)
(while (setq p2 (getpoint p1 "\nNew point : "))
(command "copy" e0 "" p1 p2)
(if (= n "") 
(setq cn (incC cn))
(setq cn (strcat c (incN (vl-string-subst "" c cn) dn))) 
)
(setq
dat (entget (entnext (entlast)))
dat (subst (cons 1 cn) (assoc 1 dat) dat)
)
(entmod dat)
(command "regen")
)
(princ)
)
;;;============================
