;;;;;;;;;;;;;;;;;;;;;;;;;;;;PHAN VUNG THEO MAU SAC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:PVMS( / i ss Number_Min KCD e Caodo Color_Number Ename_text);;;;Phan vung mau sac
(vl-load-com)
(setvar "CMDECHO" 0)
(setq ss22 (ssget (list (cons 0  "TEXT"))))
(setq ss (ChonTextSo ss22))
(setq Number_Max (Min_Max ss "Max"))
(setq Number_Min (Min_Max ss "Min"))
(setq KCD (getreal "\n Nh\U+1EADp  kho\U+1EA3ng cao \U+0111\U+1EC1u: "))
(setq Number_Color  (fix  (/ (abs (-  Number_Max Number_Min)) KCD) ))
(foreach Ename_Text (acet-ss-to-list ss)
  	(setq Caodo (atof (cdr (assoc 1 (entget Ename_Text)))))
  	(setq Color_Number  (+  (fix (/  (abs (- Caodo Number_Min)) KCD)) (rem Caodo KCD)))
  	(vla-put-color (vlax-ename->vla-object Ename_Text) Color_Number)
	       
)
(princ)
)

(defun Min_Max  (ss ans /       ans         ss_max    ssl_max     xth       tent_max  ent_max   ent_base_max
                   num_max   ss_min    ssl_min   yth     tent_min  ent_min   ent_base_min
                   num_min   ss         ssl       index     tents       tot         ent       num
                   ave
                  )
    (cond
    ((= ans "Max")
     (setq ssl_max    (sslength ss)
           xth    0
           tent_max    0
     ) 
     (if (/= ssl_max 0)
         (progn
         (while    (< xth ssl_max)
             (setq ent_max (entget (ssname ss xth)))
             (if (= (cdr (assoc 0 ent_max)) "TEXT")
             (progn
                 (setq ent_base_max (atof (cdr (assoc 1 ent_max))))
                 (setq xth ssl_max)
             ) 
             (setq xth (1+ xth))
             ) 
         ) 
         (setq xth 0)
         (repeat ssl_max
             (setq ent_max (entget (ssname ss xth)))
             (if (= (cdr (assoc 0 ent_max)) "TEXT")
             (progn
                 (setq num_max (atof (cdr (assoc 1 ent_max))))
                 (if (> num_max ent_base_max)
                 (setq ent_base_max num_max)
                 ) 
                 (setq tent_max (1+ tent_max))
             ) 
             ) 
             (setq xth (1+ xth))
         ) 
	 (setq KQ ent_base_max)
         )
     ) 
    )
    ((= ans "Min")
     (setq ssl_min    (sslength ss)
           yth    0
           tent_min    0
     )
     (if (/= ssl_min 0)
         (progn
         (while    (< yth ssl_min)
             (setq ent_min (entget (ssname ss yth)))
             (if (= (cdr (assoc 0 ent_min)) "TEXT")
             (progn
                 (setq ent_base_min (atof (cdr (assoc 1 ent_min))))
                 (setq yth ssl_min)
             ) 
             (setq yth (1+ yth))
             ) 
         ) 
         (setq yth 0)
         (repeat ssl_min
             (setq ent_min (entget (ssname ss yth)))
             (if (= (cdr (assoc 0 ent_min)) "TEXT")
             (progn
                 (setq num_min (atof (cdr (assoc 1 ent_min))))
                 (if (< num_min ent_base_min)
                 (setq ent_base_min num_min)
                 ) 
                 (setq tent_min (1+ tent_min))
             ) 
             )
             (setq yth (1+ yth))
         ) 
	 (setq KQ ent_base_min)
         ) 
     ) 
    )
    ) 
)



(defun ChonTextSo ( ss /  i ent str ss1) 
    (progn
      (setq i	0
	    ss1	(ssadd)
      )
      (repeat (sslength ss)
	(setq ent (ssname ss i)
	      str (cdr(assoc 1 (entget ent)))
	      i	  (+ 1 i)
	)
	(if (distof str 2)
	  (ssadd ent ss1)
	)
      )
      (if (> (sslength ss1) 0)
	ss1
      )      
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;TAO MO HINH TAM GIAC;;;;;;;;;;;;;;;;;;;;
(defun c:TIN (/ I L S)
(_layer2 "TIN" 9)
(setvar "clayer" "TIN")
(setq Obj (entsel "\n Ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng c\U+1EA7n t\U+1EA1o m\U+00F4 h\U+00ECnh TIN : "))
(setq NameObj (cdr (assoc 0 (entget (car Obj)))))
(setq LayObj (cdr (assoc 8 (entget (car Obj)))))
(alert (strcat "\n H\U+00E3y ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng l\U+00E0: " NameObj 
	       "\n T\U+00EAn layer l\U+00E0 : " LayObj )
)


  
(if 	(setq i 0
              s (ssget (list (cons 0  NameObj) (cons 8 LayObj)))
    	)
  	(progn
	  	(cond
		  ((= NameObj "POINT")
			(progn
			  (repeat (sslength s)
			          (setq l (cons (cdr (assoc 10 (entget (ssname s i)))) l)
			                i (1+ i)
			          ) 
		         ) 
		         (eea-delone-triangulate i l)
		  ))
		  ((= NameObj "TEXT")
			(progn
			  (repeat (sslength s)
			          (setq l (cons (TD:Text-Base (ssname s i) ) l)
			                i (1+ i)
			          ) 
		         ) 
		         (eea-delone-triangulate i l)
		  ))
		)
	)
)
)
(defun eea-delone-triangulate (i1 L / A A1 A2 A3 I I2 L1 L2 L3 LP MA MI P S TI TR X1 X2 Y1 Y2)
 (if l
  (progn
   (setq ti (car (_VL-TIMES))
         i  1
         i1 (/ i1 100.)
         i2 0
         l  (vl-sort (mapcar (function (lambda (p)
                                        (list (/ (fix (* (car p) 1000)) 1000.)
                                              (/ (fix (* (cadr p) 1000)) 1000.)
                                              (caddr p)
                                        ) 
                                       ) 
                             ) 
                             l
                     ) 
                     (function (lambda (a b) (>= (car a) (car b))))
            )
         x2 (caar l)
         y1 (cadar l)
         y2 y1
   ) 
   (while l
    (setq a2 (car l))
    (if (<= (cadr a2) y1)
     (setq y1 (cadr a2))
     (if (> (cadr a2) y2)
      (setq y2 (cadr a2))
     )
    )
    (setq a  (fix (caar l))
          a1 (list (car l))
          l  (cdr l)
    ) 
    (while (and l (= (fix (caar l)) a))
     (setq a2 (car l))
     (if (<= (cadr a2) y1)
      (setq y1 (cadr a2))
      (if (> (cadr a2) y2)
       (setq y2 (cadr a2))
      ) 
     ) 
     (setq a1 (cons (car l) (vl-remove a2 a1))
           l  (cdr l)
     ) 
    ) 
    (foreach a a1 (setq lp (cons a lp)))
   ) 
   (setq x1 (caar lp)
         a  (list (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
         a1 (distance a (list x1 y1))
         ma (+ (car a) a1 a1)
         mi (- (car a) a1)
         s  (list (list ma (cadr a) 0)
                  (list mi (+ (cadr a) a1 a1) 0)
                  (list (- (car a) a1) (- (cadr a) a1 a1) 0)
            )
         l  (list (cons x2 (cons a (cons (+ a1 a1) s))))
         ma (1- ma)
         mi (1+ mi)
   ) 
   (while lp
    (setq p  (car lp)
          lp (cdr lp)
          l1 nil
    ) 
    (while l
     (setq tr (car l)
           l  (cdr l)
     ) 
     (cond ((< (car tr) (car p)) (setq l2 (cons (cdddr tr) l2)))
           ((< (distance p (cadr tr)) (caddr tr))
            (setq tr (cdddr tr)
                  a1 (car tr)
                  a2 (cadr tr)
                  a3 (caddr tr)
                  l1 (cons (list (+ (car a1) (car a2)) (+ (cadr a1) (cadr a2)) a1 a2)
                           (cons (list (+ (car a2) (car a3)) (+ (cadr a2) (cadr a3)) a2 a3)
                                 (cons (list (+ (car a3) (car a1)) (+ (cadr a3) (cadr a1)) a3 a1) l1)
                           ) 
                     ) 
            ) 
           )
           (t (setq l3 (cons tr l3)))
     ) 
    )
    (setq l  l3
          l3 nil
          l1 (vl-sort l1
                      (function (lambda (a b)
                                 (if (= (car a) (car b))
                                  (<= (cadr a) (cadr b))
                                  (< (car a) (car b))
                                 )
                                ) 
                      ) 
             ) 
    ) 
    (while l1
     (if (and (= (caar l1) (caadr l1)) (= (cadar l1) (cadadr l1)))
      (setq l1 (cddr l1))
      (setq l  (cons (eea-data-triangle p (cddar l1)) l)
            l1 (cdr l1)
      ) 
     ) 
    )
    (if (and (< (setq i (1- i)) 1) (< i2 100))
     (progn
      (setvar
       "MODEMACRO"
       (strcat
        "     "
        (itoa (setq i2 (1+ i2)))
        " %    "
        (substr
         "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
         1
         i2
        ) 
        (substr "..." 1 (- 100 i2))
       ) 
      ) 
      (setq i i1)
     ) 
    ) 
   ) 
   (foreach a l (setq l2 (cons (cdddr a) l2)))
   (setq l2 (vl-remove-if-not
             (function (lambda (a) (and (< mi (caadr a) ma) (< mi (caaddr a) ma))))
             l2
            ) 
   ) 
   (foreach a l2
    (entmake (list (cons 0 "3DFACE")
                   (cons 10 (car a))
                   (cons 11 (car a))
                   (cons 12 (cadr a))
                   (cons 13 (caddr a))
             ) 
    ) 
   ) 
  ) 
 )
 (setvar "MODEMACRO" "")
 (princ (strcat "\n " (rtos (/ (- (car (_VL-TIMES)) ti) 1000.) 2 4) " secs."))
 (princ)
)
(defun eea-data-triangle (P1 l / A A1 P2 P3 P4 S)
 (setq p2 (car l)
       p3 (cadr l)
       p4 (list (car p3) (cadr p3))
 ) 
 (if (not (zerop (setq s (sin (setq a (- (angle p2 p4) (angle p2 p1)))))))
  (progn (setq a  (polar p4
                         (+ -1.570796326794896 (angle p4 p1) a)
                         (setq a1 (/ (distance p1 p4) s 2.))
                  ) 
               a1 (abs a1)
         )
         (list (+ (car a) a1) a a1 p1 p2 p3)
  )
 ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LAT CANH MO HINH TIN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun C:LC(/ olmode  i Chon loop ss1 Dem n Pnt Ename_ Ds_gop P10_1 P12_1 P13_1 P10_2 P12_2 P13_2 3DF1 3DF2  )
(vl-load-com)
(setvar "CMDECHO" 0)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))
(setq Chon (getpoint "\n Ch\U+1ECDn c\U+1EA1nh \U+0111\U+1EC3 l\U+1EADt: "))
(setvar "OSMODE" 512)
(setq loop T)
(while loop
	(setq Chon (getpoint "\nCh\U+1ECDn \U+0111i\U+1EC3m : "))
  	(cond
	  (T
		(if Chon
		  (progn
			(setq i 0)
			(setq Dem 0)
		    	(setq Ds_gop (list))
			(setq ss1 (ssget "_X" (list (cons 0 "3DFACE"))))
			(setq n (sslength ss1))
			(while (< i n)
				(setq Ename_ (ssname  ss1 i))
				(if (3Dinh3DFace Ename_ Chon)
					(progn
						(setq Dem (1+ Dem))
						(setq Ds_3DD (3Dinh3DFace Ename_ Chon))
						(setq Ds_gop (append Ds_gop (list Ds_3DD)))
					)
				)
			(setq i (1+ i))
			)
			(if ( = Dem 2)
			    (progn
				(setq 3DF1 (cadddr (car Ds_gop)))
			        (setq 3DF2 (cadddr (cadr Ds_gop)))
				(setq Ds_3Dinh_1 (3Dinh3DFace 3DF1 Chon ))
				(setq P10_1 (car Ds_3Dinh_1))
				(setq P12_1 (cadr Ds_3Dinh_1))
				(setq P13_1 (caddr Ds_3Dinh_1))
				
				(setq Ds_3Dinh_2 (3Dinh3DFace 3DF2 Chon ))
				(setq P10_2 (car Ds_3Dinh_2))
				(setq P12_2 (cadr Ds_3Dinh_2))
				(setq P13_2 (caddr Ds_3Dinh_2))
			        (if  (CheckInterSecLine P10_1 P12_1 P13_1 P13_2)
				     (progn
			        	 (Make3DFace P10_1 P10_1 P13_1 P13_2)
			        	 (Make3DFace P12_1 P12_1 P13_1 P13_2)
			        	 (entdel 3DF1)
			        	 (entdel 3DF2)
				     )
				     (Alert "Kh\U+00F4ng th\U+1EC3 l\U+1EADt c\U+1EA1nh")
				)
			    )
			)		    	
		  )
		  (setq loop nil)
		)
	  )
	)
)
(setvar "OSMODE" Olmode)
(princ)
)





(defun Pnt_2_XY(Pnt)
  (setq LstPnt (list (car Pnt) (cadr Pnt)))
)


;;;;;;;;;;;;;;DA KIEM TRA;;;;;;;;;;;;;;;;;;;;;
(defun Check_Point_in_Line (P11 P22 P33 / P1 P2 P3 G12 G32 G13 X3 Y3  )
(setq OK nil)
(setq P1 (list (car P11) (cadr P11)))
(setq P2 (list (car P22) (cadr P22)))
(setq P3 (list (car P33) (cadr P33)))
(setq G12 (angle P1 P2))
(setq G13 (angle P1 P3))
(setq G32 (angle P3 P2))
(if (and (equal G13 G12 0.00001)(equal G32 G12 0.00001 ))
    (setq OK T)
)
OK
)

(defun Check_Point_in_Line (P11 P22 P33 / P1 P2 P3 G12 G2 G3 X3 Y3  )
(setq OK nil)
(setq P1 (list (car P11) (cadr P11)))
(setq P2 (list (car P22) (cadr P22)))
(setq P3 (list (car P33) (cadr P33)))
(setq G12 (angle P1 P2))
(setq G2 (angle P1 P3))
(setq G3 (angle P3 P2))
(if (and (equal G2 G12 0.00001)(equal G3 G12 0.00001 )) 
    (setq OK T)
)
OK
)



;;;;(setq kk (3Dinh3DFace (car (entsel "\nChon 3DFACE :  ")) (getpoint "\nPick diem:  ")))

(defun 3Dinh3DFace (Emane_3DF Point1 / Ds_3Dinh P10 P12 P13  )
(vl-load-com)
(setq Ds_3Dinh (3Diem3DF_3Point Emane_3DF))
(setq P10 (car Ds_3Dinh))
(setq P12 (cadr Ds_3Dinh))
(setq P13 (caddr Ds_3Dinh))
(setq SSDS (list))
(if (Check_Point_in_Line P10 P12 Point1)
    (setq SSDS (list P10 P12 P13 Emane_3DF))
    (progn
    	(if (Check_Point_in_Line P12 P13 Point1)
    	    (setq SSDS (list  P12 P13 P10 Emane_3DF))
    	    (progn
	      (if (Check_Point_in_Line P13 P10 Point1)
    		  (setq SSDS (list P13 P10 P12 Emane_3DF))
	      )
	    )
	 )
     )
)
SSDS
)

(defun Make3DFace (p1 p2 p3 p4)
	(entmakex (list (cons 0 "3DFACE")
	                (cons 10 p1)
	                (cons 11 p2)
	                (cons 12 p3)
	                (cons 13 p4)
		 )
	)
)


;;;;;; XET DIEM BEN TRAI HAY PHAI DOAN THANG;;;;;;;;;;;;;;;;;;;
(defun CCW  (P1 P2 P / )
  (setq dX  ( - (car P) (car P1))
	dY  ( - (cadr P) (cadr P1))
	dX0 ( - (car P2) (car P1))
	dY0 ( - (cadr P2) (cadr P1))
	d   ( - (* dX dY0) (* dY dX0))
  )
  (if (> d 0)
    (setq CCW1 1)
    (setq CCW1 -1)
  )
  CCW1
)



(defun CheckInterSecLine (P1 P2 P3 P4 / )
  (setq KTT nil)
  (if (and ( <  (* (CCW P1 P2 P3)  (CCW P1 P2 P4)) 0) ( <  (* (CCW P3 P4 P1)  (CCW P3 P4 P2)) 0))
      (setq KTT T)
  )
  KTT
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NOI SUY DO CAO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:3DF ( / Enamee elst Lts3D pt1 pt3 pt4 Ds_3Di Ds_3Dpoly Pnt Pnt_moi i n  )
(vl-load-com)
(setq olmode (Getvar "OSMODE"))
(setvar "OSMODE" 0)
(_layer2 "THEM" 3)
(_layer2 "POINT" 2)
(setq h (TD:GetXWithDefault getreal "\n Nh\U+1EADp chi\U+1EC1u cao ch\U+1EEF: " '*h* (atof "1")))
(setq ss1 (ssget "_X" (list (cons 0 "3DFACE"))))
(setq i 0)
(setq Ds_3Di (list))
(setq Ds_3Dpoly (list))
(setq n (sslength ss1))
(while (< i n)
  (progn
        (setq Enamee (ssname ss1 i))
	(setq elst (entget Enamee))
        (setq Lts3D (3Diem3DF_3Point Enamee))
	(setq pt1 (car Lts3D)
	      pt3 (cadr Lts3D)
	      pt4 (caddr Lts3D)
	)
	(setq Ds_3Di (list pt1 pt3 pt4))
   )
	(setq Ds_3Dpoly (append Ds_3dpoly (list Ds_3Di)))
	(setq i (1+ i))
)
(while
	(setq Pnt (getpoint "\n Ch\U+1ECDn \U+0111i\U+1EC3m c\U+1EA7n n\U+1ED9i suy: "))
	(progn
		(foreach Ds_3D_i Ds_3Dpoly
			(if (PointInTamgiac Pnt Ds_3D_i)
			    (progn
			    	(setq Pnt_moi (DoCaoDiemNoiSuy Pnt Ds_3D_i))
				(MakeText Pnt_moi (rtos (nth 2 Pnt_moi) 2 3) h 0 "C" "THEM")
			    )
			 )
		)
	 )
)
(setvar "OSMODE" olmode)
(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VE DUONG DONG MUC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DCT( / olmode i  H_KCD ss1  Ds_3Di Ds_3Dpoly elst pt1 pt3 pt4  DMC Elast lstAll );;;;Draw Contour
(vl-load-com)
(setvar "CMDECHO" 0)
(setq olmode (Getvar "OSMODE"))
(setvar "OSMODE" 0)
(progn
(setq H_KCD (getreal "\n Nh\U+1EADp kho\U+1EA3ng cao \U+0111\U+1EC1u:  "))
(setq DMC (* H_KCD 5))
(setq ss1 (ssget (list (cons 0 "3DFACE"))))
(setq i 0)
(setq Ds_3Di (list))
(setq Ds_3Dpoly (list))
(setq n (sslength ss1))
(setq Elast (entlast))
)
(while (< i n)
  (progn
    	(setq Name_3Di (ssname ss1 i))
	(setq elst (entget Name_3Di))
    	(setq Lts3P (3Diem3DF_3Point Name_3Di))
	(setq pt1 (car Lts3P)
	      pt3 (cadr Lts3P)
	      pt4 (caddr Lts3P)
	)
	(DrawContour_in_Triangular pt1 pt3 pt4 H_KCD DMC)
   )
  (setq i (1+ i))
)
(while
	          (setq Elast  (entnext Elast ))
		  (setq lstAll (reverse (cons Elast lstAll)))
)
(Join_SPLine (acet-list-to-ss lstAll))
(setvar "OSMODE" olmode)
(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Join_SPLine (ss / ope)
  (setq ope (getvar "PEDITACCEPT"))
  (if ss
    (progn
      (setvar "PEDITACCEPT" 1)
      (vl-cmdf "_.pedit" "_M" ss "" "_J" ""  "_S" "" "")
    )
  )
  (setvar "PEDITACCEPT" ope)
)

;;;;;;;;;(Noisuy (getpoint) (getpoint) 5)
(defun Noisuy (P1 P2 ZP / K XP YP  )
(setq Pns (list))
(progn
(setq P1 (getpoint))
(setq P2 (getpoint))
(setq ZP 0.5)
(setq X1 (car P1)
      Y1 (cadr P1)
      Z1 (caddr P1)
      X2 (car P2)
      Y2 (cadr P2)
      Z2 (caddr P2)
)
)
(if (/= Z2 Z1)
    (progn
	(setq K (/ (- ZP Z1) (- Z2 Z1)))
        (setq XP (+ X1 (* K (- X2 X1))))
        (setq YP (+ Y1 (* K (- Y2 Y1))))
        (if (<= (* (- ZP Z1) (- ZP Z2)) 0)
	    (setq Pns (list XP YP ZP))
        )
     )
)
Pns
)


(defun MSDDM (Pz KCDDM /  );;;Mau sac duong dong muc
  (if (<= (-  Pz (* KCDDM (fix (/ Pz KCDDM)))) 0.000001)
      (setq Mausac 1)
      (setq Mausac 4)
  )
)

(defun DrawContour_in_Triangular ( P1 P2 P3 H DMC /  P12 P23 P31 X1 Y1 Z1 X2 Y2 Z2 X3 Y3 Z3 Z_min Z_max Color_Number)
(_layer2 "CONTOUR" 2)
(setq X1 (car P1)
      Y1 (cadr P1)
      Z1 (caddr P1)
      X2 (car P2)
      Y2 (cadr P2)
      Z2 (caddr P2)
      X3 (car P3)
      Y3 (cadr P3)
      Z3 (caddr P3)
)
(setvar "OSMODE" 0)
(setq Z_min (Min Z1 Z2 Z3))
(setq Z_max (Max Z1 Z2 Z3))
(setq rZ (+ (* (fix (/ Z_min H)) H) 0.0000000001))
(while  (<= rZ Z_max)
	  (if (and (setq P12 (Noisuy P1 P2  rZ)) (setq P23 (Noisuy P2 P3  rZ)))
	      (progn
		     (setq Mausac (MSDDM (caddr P12) DMC))
		     (MakeLine P12 P23 "CONTOUR" Mausac)
	      )
	  )
	  (if (and (setq P23 (Noisuy P2 P3 rZ)) (setq P31 (Noisuy P3 P1  rZ)))
	      (progn
		     (setq Mausac (MSDDM (caddr P23) DMC))
		     (MakeLine P23 P31 "CONTOUR" Mausac)
	      )
	  )
	  (if (and (setq P31 (Noisuy P3 P1  rZ)) (setq P12 (Noisuy P1 P2  rZ)))
	      (progn
		     (setq Mausac (MSDDM (caddr P31) DMC))
		     (MakeLine P31 P12 "CONTOUR" Mausac)
	      )
	  )
  (setq rZ (+ rZ H))
)
)








;;;(defun DrawContour_in_Triangular ( P1 P2 P3 H DMC /  P12 P23 P31 X1 Y1 Z1 X2 Y2 Z2 X3 Y3 Z3 Z_min Z_max Color_Number)
;;;(_layer2 "CONTOUR" 2)
;;;(setq X1 (car P1)
;;;      Y1 (cadr P1)
;;;      Z1 (caddr P1)
;;;      X2 (car P2)
;;;      Y2 (cadr P2)
;;;      Z2 (caddr P2)
;;;      X3 (car P3)
;;;      Y3 (cadr P3)
;;;      Z3 (caddr P3)
;;;)
;;;(setvar "OSMODE" 0)
;;;(setq Z_min (Min Z1 Z2 Z3))
;;;(setq Z_max (Max Z1 Z2 Z3))
;;;(setq rZ (+ (* (fix (/ Z_min H)) H) 0.0000000001))
;;;(while  (<= rZ Z_max)
;;;	  (if (and (setq P12 (Noisuy P1 P2  rZ)) (setq P23 (Noisuy P2 P3  rZ)))
;;;	      (progn
;;;		     (setq Mausac (MSDDM (caddr (car P12)) DMC))
;;;		     (MakeLine (car P12) (car P23) "CONTOUR" Mausac)
;;;	      )
;;;	  )
;;;	  (if (and (setq P23 (Noisuy P2 P3 rZ)) (setq P31 (Noisuy P3 P1  rZ)))
;;;	      (progn
;;;		     (setq Mausac (MSDDM (caddr (car P23)) DMC))
;;;		     (MakeLine (car P23) (car P31) "CONTOUR" Mausac)
;;;	      )
;;;	  )
;;;	  (if (and (setq P31 (Noisuy P3 P1  rZ)) (setq P12 (Noisuy P1 P2  rZ)))
;;;	      (progn
;;;		     (setq Mausac (MSDDM (caddr (car P31)) DMC))
;;;		     (MakeLine (car P31) (car P12) "CONTOUR" Mausac)
;;;	      )
;;;	  )
;;;  (setq rZ (+ rZ H))
;;;)
;;;)

;;;;;;(Noisuy (getpoint)  (getpoint) 417.0)

(defun Noisuy (P1 P2 ZP / K XP YP Z1 Z2 )
(setq Pns (list))
(setq X1 (car P1)
      Y1 (cadr P1)
      Z1 (caddr P1)
      X2 (car P2)
      Y2 (cadr P2)
      Z2 (caddr P2)
)
(if (/= Z2 Z1)
    (progn
	(setq K (/ (- ZP Z1) (- Z2 Z1)))
        (setq XP (+ X1 (* K (- X2 X1))))
        (setq YP (+ Y1 (* K (- Y2 Y1))))
        (if (<= (* (- ZP Z1) (- ZP Z2)) 0)
	    (setq Pns (list XP YP ZP))
         )
      )
)
Pns
)
;;;(defun DrawContour_in_Triangular ( P1 P2 P3 H DMC /  P12 P23 P31 X1 Y1 Z1 X2 Y2 Z2 X3 Y3 Z3 Z_min Z_max Color_Number)
;;;(_layer2 "CONTOUR" 2)
;;;(setq X1 (car P1)
;;;      Y1 (cadr P1)
;;;      Z1 (caddr P1)
;;;      X2 (car P2)
;;;      Y2 (cadr P2)
;;;      Z2 (caddr P2)
;;;      X3 (car P3)
;;;      Y3 (cadr P3)
;;;      Z3 (caddr P3)
;;;)
;;;(setvar "OSMODE" 0)
;;;(setq Z_min (Min Z1 Z2 Z3))
;;;(setq Z_max (Max Z1 Z2 Z3))
;;;(setq rZ (+ (* (fix (/ Z_min H)) H) 0.0000000001))
;;;(while  (<= rZ Z_max)
;;;	  (if (and (setq P12 (Noisuy P1 P2  rZ)) (setq P23 (Noisuy P2 P3  rZ)))
;;;	      (progn
;;;		     (setq Mausac (MSDDM (caddr (car P12)) DMC))
;;;		     (MakeLine (car P12) (car P23) "CONTOUR" Mausac)
;;;	      )
;;;	  )
;;;	  (if (and (setq P23 (Noisuy P2 P3 rZ)) (setq P31 (Noisuy P3 P1  rZ)))
;;;	      (progn
;;;		     (setq Mausac (MSDDM (caddr (car P23)) DMC))
;;;		     (MakeLine (car P23) (car P31) "CONTOUR" Mausac)
;;;	      )
;;;	  )
;;;	  (if (and (setq P31 (Noisuy P3 P1  rZ)) (setq P12 (Noisuy P1 P2  rZ)))
;;;	      (progn
;;;		     (setq Mausac (MSDDM (caddr (car P31)) DMC))
;;;		     (MakeLine (car P31) (car P12) "CONTOUR" Mausac)
;;;	      )
;;;	  )
;;;  (setq rZ (+ rZ H))
;;;)
;;;)

;;;;;;;;;(DoCaoDiemNoiSuy (getpoint) (3Diem3DF_3Point (car (entsel))))
(defun DoCaoDiemNoiSuy ( Pnt Ds_3Dface / )
  (setq Pnt_New (list))
  (setq Ds_Pnt (TDXYZ Pnt))
  (progn
	(setq   P1 (nth 0 Ds_3Dface)
		P2 (nth 1 Ds_3Dface)
		P3 (nth 2 Ds_3Dface)
	)
  	(setq Ds_D1 (TDXYZ P1)
	      Ds_D2 (TDXYZ P2)
	      Ds_D3 (TDXYZ P3)
	)
  )
  (progn
  	(setq a1 (- (nth 0 Ds_D2) (nth 0 Ds_D1)))
  	(setq b1 (- (nth 1 Ds_D2) (nth 1 Ds_D1)))
  	(setq c1 (- (nth 2 Ds_D2) (nth 2 Ds_D1)))

  	(setq a2 (- (nth 0 Ds_D3) (nth 0 Ds_D1)))
  	(setq b2 (- (nth 1 Ds_D3) (nth 1 Ds_D1)))
  	(setq c2 (- (nth 2 Ds_D3) (nth 2 Ds_D1)))
  	(setq a (/ (- (* b1 c2) (* b2 c1)) (- (* a2 b1) (* a1 b2))))
  	(setq b (/ (- (* a2 c1) (* a1 c2)) (- (* a2 b1) (* a1 b2))))
    	(setq c (- (nth 2 Ds_D1) (+ (* a (nth 0 Ds_D1)) (* b (nth 1 Ds_D1)))))
   )
   (setq Z (+ (* a (nth 0 Ds_Pnt)) (* b (nth 1 Ds_Pnt)) c))
   (setq Pnt_New (list (nth 0 Ds_Pnt) (nth 1 Ds_Pnt) Z))
   Pnt_New	 
)

;;;;;; LAY TOA DO POINT ;;;;;;;;
(defun TDXYZ (Pnt / )
  (setq d_sach (list))
  (setq X (car Pnt))
  (setq Y (cadr Pnt))
  (setq Z (caddr Pnt))
  (setq d_sach (list X Y Z))
  d_sach
)

;;;;;; XET DIEM BEN TRONG HAY NGOAI TAM GIAC;;;;;;;;;;;;;;;;;;;

;;;;;;;;;(PointInTamgiac (getpoint) (3Diem3DF_3Point (car (entsel))))
(defun PointInTamgiac (P Ds_3D /)
(setq KQ nil)
(setq P1 (nth 0 Ds_3D)
      P2 (nth 1 Ds_3D)
      P3 (nth 2 Ds_3D)
)
(setq C (CCW P2 P3 P))
(if (and (= (CCW P1 P2 P) C) (= (CCW P3 P1 P) C))
  (setq KQ T)
)
 KQ
)

(defun MakeLine (PT1 PT2 Layer Color )
(entmakex (list '(0 . "LINE")
(cons 8 Layer)
(cons 62 Color)
(cons 10 PT1)	(cons 11 PT2)
))
)



;;;;;;;;;;;;(setq klkkl (3Diem3DF_3Point (car (entsel ))))

(defun 3Diem3DF_3Point ( Ename_3DF  / P10 P11 P12 P13 );;;;;;;;;;;
(vl-load-com)
(setq Danhsachdiem (list))
(setq Obj3DFace_ (entget Ename_3DF))
(setq P10 (cdr (assoc 10 Obj3DFace_)))
(setq P11 (cdr (assoc 11 Obj3DFace_)))
(setq P12 (cdr (assoc 12 Obj3DFace_)))
(setq P13 (cdr (assoc 13 Obj3DFace_)))
(setq Danhsachdiem (TD:FILTERPOINTDUPLICATE  (list P10 P11 P12 P13) 0.00000001))
Danhsachdiem
)


;;;;(KTDT (getpoint) (getpoint))
(defun KTDT (P1 P2 /);;;;KIEM TRA DIEM TRUNG
(setq KQ nil)
(if (and (equal (car P1) (car P2) 0.0000001) (equal (cadr P1) (cadr P2) 0.0000001))
    (setq KQ T)
)
KQ
)

(defun TD:FILTERPOINTDUPLICATE ( l fz )
    (if l
      (cons (car l)
        (TD:FILTERPOINTDUPLICATE
          (vl-remove-if '(lambda ( x ) (equal x (car l)  fz)) (cdr l)) fz
        )
      )
    )
)



(defun MakeText (point string Height Ang justify  Layer   / Lst)
	(setq Lst (list '(0 . "TEXT")
			(cons 8 (if Layer Layer (getvar "Clayer")))
			(cons 62 (if Color Color 256))
			(cons 10 point)
			(cons 40 Height)
			(cons 1 string)
			(cons 50 Ang)
		)
	justify (strcase justify)
      )
      (cond ((= justify "C") (setq Lst (append Lst (list (cons 72 1) (cons 11 point)))))
			((= justify "R") (setq Lst (append Lst (list (cons 72 2) (cons 11 point)))))
			((= justify "M") (setq Lst (append Lst (list (cons 72 4) (cons 11 point)))))
			((= justify "TL") (setq Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 3)))))
			((= justify "TC") (setq Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 3)))))
			((= justify "TR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 3)))))	
			((= justify "ML") (setq Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 2)))))
			((= justify "MC") (setq Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 2)))))
			((= justify "MR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 2)))))
			((= justify "BL") (setq Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 1)))))
			((= justify "BC") (setq Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 1)))))
			((= justify "BR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 1)))))
      )
     (entmakex Lst)
 )
(defun TD:GetXWithDefault ( _function _prompt _symbol _default / _toString )
	(setq _toString
		(lambda ( x )
			(cond
				( (eq getangle _function) (angtos x) )
				( (eq 'REAL (type x)) (rtos x) )
				( (eq 'INT (type x)) (itoa x) )
				( x )
			)
		)
	)

	(set _symbol
	(
	(lambda ( input ) (if (or (not input) (eq "" input)) (eval _symbol) input))
	(_function (strcat _prompt "<" (_toString (set _symbol (cond ( (eval _symbol) ) ( _default )))) "> : "))
	)
	)
)


(defun _layer2 ( name colour )
    (if (null (tblsearch "LAYER" name))
        (entmake
            (list
               '(0 . "LAYER")
               '(100 . "AcDbSymbolTableRecord")
               '(100 . "AcDbLayerTableRecord")
               '(70 . 0)
                (cons 2 name)
                (cons 62 colour)
            )
        )
    )
)

(defun TD:Text-Base (ent)
  (setq Ma10  (cdr (assoc 10 (entget ent))))
  (setq Ma11  (cdr (assoc 11 (entget ent))))
  (setq X11 (car Ma11))
  (setq Ma71  (cdr (assoc 71 (entget ent))))
  (setq Ma72  (cdr (assoc 72 (entget ent))))
  (if (or (and (= Ma71 0) (= Ma72 0) (= X11 0))
	  (and (= Ma71 0) (= Ma72 3) )
	  (and (= Ma71 0) (= Ma72 5) )
      )
    Ma10
    Ma11
   )
)