;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/4207-v%E1%BA%BD-%E1%BB%91ng-m%E1%BB%81m-ki%E1%BB%83u-ru%E1%BB%99t-g%C3%A0/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;CHUONG TRINH VE ONG NOI MEM - FLEXIBLE TUBE
;;;Yeu cau: AutoCAD 2000 tro len
;;;Lenh: NM, nhap duong kinh ong va pick chon duong tam
;;;Chap nhan cac loai: line, pline, spline, arc, circle, ellipse
;;;Chuong trinh bat dau ve tu dau mut gan hon so voi diem pick
;;;Tri so duong kinh D cua lan chay truoc duoc tu dong luu lai
;;;Neu khong muon thay doi D, Enter khong can nhap so
;;;Gia tri mac dinh ban dau D = 100
;;;Ket qua ve la 1 duong pline duy nhat
;;;Written by ssg - June 2008 - www.cadviet.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun C:NM( / D chon e pC pM xM L ps pe n S
                     p01 x flag p02 a p1 p2 p3 p4 i oldos p03 p5 p6)
(vl-load-com)
(if (not D0) (setq D0 100))
(setq D (getreal (strcat "\nNhap duong kinh ong <" (rtos D0) ">:")))
(if (not D) (setq D D0) (setq D0 D))
(setq
   chon (entsel "\nPick chon duong tam:")
   e (car chon)
   pC (cadr chon)
   pM (vlax-curve-getClosestPointTo e pc) 
   xM (vlax-curve-getDistAtPoint e pM)
   L (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
   ps (vlax-curve-getPointAtDist e 0)
   pe (vlax-curve-getPointAtDist e L) 
   n (fix (/ (* 4 L) D))
   S (/ L n)
)
(if (<= xM (/ L 2)) (setq p01 ps x 0 flag 1) (setq p01 pe x L flag -1))
(setq
   p02 (vlax-curve-getPointAtDist e (+ x (* S flag)))
   a (angle p01 p02)
   p1 (polar p01 (- a (/ pi 2)) (/ D 2))
   p2 (polar p1 (+ a (/ pi 2)) D)
   p3 (polar p1 a S)
   p4 (polar p2 a S)
   i 2
   oldos (getvar "osmode")
)
(setvar "osmode" 0)
(command "pline" p3 p4 p2 p1 p3)
(repeat (- n 1)
   (setq
       p03 (vlax-curve-getPointAtDist e (+ x (* i S flag)))
       a (angle p02 p03)
       p5 (polar p03 (- a (/ pi 2)) (/ D 2))
       p6 (polar p5 (+ a (/ pi 2)) D)
   )
   (if (= i 2) (command "a"))
   (command "a" -90 p5 "L" p6 "a" "a" -90 p4 "L" p3 "a" "a" -90 p5)
   (setq p02 p03 p3 p5 p4 p6 i (1+ i))
)
(command "a" 90 p6 "a" 180 p5 "a" 180 p6 "")
(setvar "osmode" oldos)
(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
