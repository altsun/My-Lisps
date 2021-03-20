;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=42771
(Defun c:rdt (/ ss)
(command "undo" "be")
(chonnhomdoituong)
(choncuver)
(hoikieurai)
(command "undo" "end")
  (princ)
)
;;;;;;;;;;;;;;;;;
(Defun chonnhomdoituong ()
  (princ "\nChon doi tuong rai:")
  (setq ss (ssget))

 (cond 
      ((= ss nil) (princ "\nChua chon duoc doi tuong nao:") (chonnhomdoituong))
      ((/= ss nil) 
 (setq dsl (sslength ss))
            (cond 
            ((= dsl 1) 
(setq doituong (ssname SS 0))
(setq doituong (entget doituong))
(setq KIEUDOITUONG (cdr (assoc 0 doituong)))
                   (cond 
                   ((= KIEUDOITUONG "INSERT") (setq dc (cdr (assoc 10 doituong))))
                   ((/= KIEUDOITUONG "INSERT") (setq dc (getpoint "\nChon diem goc: ")))
                   );ketthuccondxemblock
                 );kethucdsl1
            ((/= dsl 1) (setq dc (getpoint "\nChon diem goc: ")))
            );ketthuccondnho

);ketthucsetqdsl
 );ketthuccondtong  
  (princ)
)
;;;;;;;;;;;;;;;;;
(Defun choncuver ()

(setq ddd (entsel "\nChon duong dan:"))
(while
(or
(null ddd)
(or (= "TEXT" (cdr (assoc 0 (entget (car ddd))))) (= "MTEXT" (cdr (assoc 0 (entget (car ddd))))) (= "HATCH" (cdr (assoc 0 (entget (car ddd))))) (= "INSERT" (cdr (assoc 0 (entget (car ddd))))) (= "REGION" (cdr (assoc 0 (entget (car ddd))))) (= "DIMENSION" (cdr (assoc 0 (entget (car ddd)))))
)
)
(setq ddd (entsel "\nDoi tuong khong the lam duong dan! Chon lai"))
)

(setq chondd (car ddd))
(setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)
(setq chieudaicuver (vlax-curve-getDistAtParam chondd (vlax-curve-getEndParam chondd)))
(setq diemdau (vlax-curve-getPointAtDist chondd 0))
(setq diemcuoi (vlax-curve-getPointAtDist chondd chieudaicuver))
(setvar "osmode"luubatdiem)
  (princ)
)
;;;;;;;;;;;;;;;;;
(Defun hoikieurai (/ kieurai)
  (setq krai (strcase (getstring "\nKieu rai theo: So luong/<Khoang cach>")))
(Cond
((= krai "S") (raisoluong))
((/= krai "S")(raikhoangcach))
) 
  (princ)
)
;;;;;;;;;;;;;;
(Defun raikhoangcach ()
   (setq chieudaidoan (GETDIST "\nKhoang cach doan chia: "))
   (setq sol (+ (/ chieudaicuver chieudaidoan) 1))
    (setq sl (fix sol))
    (setq sl (fix sl))
(thuchienrai)
  (princ)
)
;;;;;;;;;;;;;;
(Defun raisoluong ()
(setq slc (getreal "\nChia duong dan thanh may lan:"))
(setq chieudaidoan (/ chieudaicuver slc))
(setq sl (fix (+ 1 slc)))
(thuchienrai)
  (princ)
)
;;;;;;;;;;;;;;
(Defun thuchienrai (/ quaykhong)

  (setq quaykhong (strcase (getstring "\nCo quay doi tuong vuong goc voi duong dan khong: Khong/<Co>")))
(Cond
((= quaykhong "K") (setq copygiua copykoquay))
((/= quaykhong "K")(setq copygiua copyquay))
) 

(setq index -1)

  (repeat sl
(setq index (1+ index))
(setq d2 (* chieudaidoan index))
(setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)
(setq p2 (vlax-curve-getPointAtDist chondd d2))
(setvar "osmode"luubatdiem)
(copygiua)
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;
(defun copycuoiquay()
(setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)
(setq d5 (- chieudaicuver 0.001))
(setq p5 (vlax-curve-getPointAtDist chondd d5))
 (setq L 0)
 (setq M (sslength ss))
 (while (< L M)
   (setq DT (ssname ss L))
   (command ".copy" DT "" dc p5)
   (command ".rotate" "last" "" diemcuoi p5)
   (command ".rotate" "last" "" diemcuoi 180)
   (setq L (1+ L))
)
(setvar "osmode"luubatdiem)
(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;
(defun COPYQUAY(/ p3)
(setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)
(setq d3 (+ (* chieudaidoan index) 0.001))
(setq p3 (vlax-curve-getPointAtDist chondd d3))
(setvar "osmode"luubatdiem)
(Cond
((= p3 nil) (copycuoiquay))
((/= p3 nil) 
 (setq L 0)
 (setq M (sslength ss))
(setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)
 (while (< L M)
   (setq DT (ssname ss L))
   (command ".copy" DT "" dc p2)
   (command ".rotate" "last" "" p2 p3)
   (setq L (1+ L))
)
(setvar "osmode"luubatdiem)
)
) 


(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;
(defun COPYKOQUAY()
(setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)
(command ".copy" ss "" dc p2 "")
(setvar "osmode"luubatdiem)
(princ)
)
;;;;;;;;;;;;;;