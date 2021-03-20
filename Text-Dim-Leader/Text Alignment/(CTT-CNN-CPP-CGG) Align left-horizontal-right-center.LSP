;;;************C�N L�� TRA�I*************************
(Defun c:CTT ( )
(princ "\nSuu tam CadViet.com")
(command "undo" "be")
 (Princ "\nHay chon doi tuong:")
 (setq SS (ssget '((0 . "TEXT,MTEXT"))))

 (setq diemcanhle (getpoint "\nChon diem can le:"))

 (setq i 0)
 (setq N (sslength ss))
 (while (< i N)

  (setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)

   (setq DT (ssname ss i))
   (setq DTT (entget DT))
   (setq TEXT (cdr (assoc 10 DTT)))
   (setq Xcanhle (list (car diemcanhle)(cadr text))) 
   (command ".move" DT "" TEXT Xcanhle)

  (setvar "osmode" luubatdiem) 

   (setq i (1+ i))

 )
(command "undo" "end")
(setvar "MODEMACRO" "**CADVIET.COM**")
      (Princ)
) 

;;;************C�N L�� NGANG*************************
(Defun c:CNN ( )
(princ "\nSuu tam CadViet.com")
(command "undo" "be")
 (Princ "\nHay chon doi tuong:")
 (setq SS (ssget '((0 . "TEXT,MTEXT"))))

 (setq diemcanhle (getpoint "\nChon diem can le:"))

 (setq i 0)
 (setq N (sslength ss))
 (while (< i N)

  (setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)

   (setq DT (ssname ss i))
   (setq DTT (entget DT))
   (setq TEXT (cdr (assoc 10 DTT)))
   (setq Xcanhle (list (car text)(cadr diemcanhle)))  
   (command ".move" DT "" TEXT Xcanhle)

  (setvar "osmode" luubatdiem) 

   (setq i (1+ i))

 )
(command "undo" "end")
(setvar "MODEMACRO" "**CADVIET.COM**")
      (Princ)
) 

;;;************C�N L�� PHA�I*************************
(Defun c:CPP ( )
(princ "\nSuu tam CadViet.com")
(command "undo" "be")
(setvar "mirrtext" 0)
 (Princ "\nHay chon doi tuong:")
 (setq SS (ssget '((0 . "TEXT,MTEXT"))))

 (setq diemcanhle (getpoint "\nChon diem can le:"))

 (setq i 0)
 (setq N (sslength ss))
 (while (< i N)

  (setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)

   (setq DT (ssname ss i))
   (setq DTT (entget DT))
   (setq TEXT (cdr (assoc 10 DTT)))
   (setq Xcanhle (list (car diemcanhle)(cadr text))) 
   (command ".move" DT "" TEXT Xcanhle)
   (command ".mirror" DT "" Xcanhle diemcanhle "y")

  (setvar "osmode" luubatdiem) 

   (setq i (1+ i))

 )
(command "undo" "end")
(setvar "MODEMACRO" "**CADVIET.COM**")
      (Princ)
) 

;;;************C�N L�� GI��A*************************
(Defun c:CGG ( )
(princ "\nSuu tam CadViet.com")
(command "undo" "be")
;(setvar "mirrtext" 0)
 (Princ "\nHay chon doi tuong:")
 (setq SS (ssget '((0 . "TEXT"))))

 (setq diemcanhle (getpoint "\nChon diem can le:"))

 (setq i 0)
 (setq N (sslength ss))
 (while (< i N)

  (setq luubatdiem (getvar "osmode"))
  (setvar "osmode" 0)

   (setq DT (ssname ss i))
   (setq DTT (entget DT))
   (setq TEXT (cdr (assoc 10 DTT)))
   (setq Xcanhle (list (car diemcanhle)(cadr text))) 
   (command ".move" DT "" TEXT Xcanhle)
   (command ".mirror" DT "" Xcanhle diemcanhle "y")

   (setq DTTG (entget DT))
   (setq TEXTG (cdr (assoc 10 DTTG)))
  (setq  daitextg (distance Xcanhle TEXTG))
  (setq goctextg(angle Xcanhle TEXTG))
  (setq dainuatextg (/ daitextg 2))
  (setq diemquayg (polar Xcanhle goctextg dainuatextg))
  ;(setq diemlatg (list (car diemquayg) (+ (cadr diemquayg) 100)))
   (command ".move" DT "" diemquayg Xcanhle)
   ;(command ".mirror" DT "" diemquayg diemlatg "y")

  (setvar "osmode" luubatdiem) 

   (setq i (1+ i))

 )
(command "undo" "end")
(setvar "MODEMACRO" "**CADVIET.COM**")
      (Princ)
) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---------------------------------------
(defun nstr (stri def)
 (princ stri)
 (princ "<")
 (princ " ")
 (princ def)
 (princ ">")
 (princ ":")
 (princ " ")
);defun nstr
;--------------------
(defun nstr1 (stri)
 (princ stri)
 (princ "<")
 (princ "Nhap vao")
 (princ ">")
 (princ ":")
 (princ " ")
);defun nstr1
;---------------------
(defun nint (prompt def / temp)
 (if def
  (setq temp (getint (nstr prompt def)))
  (setq def (getint (nstr1 prompt)))
 );if def
 (if temp
  (setq def temp)
  def
 );if temp
);defun nint
;---------------------
(defun dnint (prompt def / temp)
 (if def
  (setq temp (getreal (nstr prompt def)))
  (setq def (getreal (nstr1 prompt)))
 );if def
 (if temp
  (setq def temp)
  def
 );if temp
);defun nint
;--------------------
(defun ndist (po prompt def / temp) ;nhan kh/cach va luu gia tri mac dinh
 (if def
  (setq temp (getdist po (nstr prompt def)))
  (setq def (getdist po (nstr1 prompt)))
 )if def
 (if temp
    (setq def temp)
    def
 );if temp
);defun ndist
;-----------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Defun zoomduy ( )
  (command ".zoom" "")
    (Princ)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Defun thoi ()
(princ "\nSuu tam CadViet.com")
(setvar "MODEMACRO" "**CADVIET.COM**")
      (Princ)
)