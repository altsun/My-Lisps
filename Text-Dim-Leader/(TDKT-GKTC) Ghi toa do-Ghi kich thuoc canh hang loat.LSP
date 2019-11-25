
(vl-load-com)

;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh ghi t\U+1ECDa \U+0111\U+1ED9 k\U+00EDch th\U+01B0\U+1EDBc th\U+1EE7 c\U+00F4ng theo \U+0111i\U+1EC3m Pick"
;;;	       "\n - L\U+1EC7nh tr\U+00EDch t\U+1ECDa \U+0111\U+1ED9 k\U+00EDch th\U+01B0\U+1EDBc: TDKT"
;;;	       "\n - L\U+1EC7nh ghi k\U+00EDch th\U+01B0\U+1EDBc c\U+1EA1nh tr\U+00EAn h\U+00ECnh: GKTC"
;;;	       "\n"
;;;	       "\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;	       "\nEmail: heaven2407@gmail.com"
;;;	       "\nMobile: 0972.0168.25"
;;;	       "\n" "\n" "\n"
;;;		
;;;	)
;;;)

;;;(Alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh ghi t\U+1ECDa \U+0111\U+1ED9 k\U+00EDch th\U+01B0\U+1EDBc th\U+1EE7 c\U+00F4ng theo \U+0111i\U+1EC3m Pick"
;;;	       "\n - L\U+1EC7nh tr\U+00EDch t\U+1ECDa \U+0111\U+1ED9 k\U+00EDch th\U+01B0\U+1EDBc: TDKT"
;;;	       "\n - L\U+1EC7nh ghi k\U+00EDch th\U+01B0\U+1EDBc c\U+1EA1nh tr\U+00EAn h\U+00ECnh: GKTC"
;;;	       "\n"
;;;	       "\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;	       "\nEmail: heaven2407@gmail.com"
;;;	       "\nMobile: 0972.0168.25"
;;;	       "\n" "\n" "\n"
;;;		
;;;	)
;;;)

(defun C:TDKT (/ I KCLT LOOP LTS N NDSTT NDSTT1 OLMODE P1 P2 P3 P4 PDAT_KC
	         PLINE_NGANGDAU PNT PNT_I PNT_I1 PNT_I_X PNT_I_X1 PNT_I_Y PNT_I_Y1 PNT_KC
	         P_CUOI P_DAT P_LINE P_STT P_TEXT  STT TD_POINT TD_X TD_X1 TD_Y TD_Y1
	      );;;;;TOA DO KICH THUOC
(setvar "CMDECHO" 0)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))
(or *h* (setq *h* 2))
(setq h (getreal (strcat "\nNh\U+1EADp chi\U+1EC1u cao Text <"
		  (rtos *h* 2 2)
		 "> :"
	  )
 )
)
(if (not h) (setq h *h*) (setq *h* h))
(or *Sole* (setq *Sole* 2))
(setq Sole (getint (strcat "\nNh\U+1EADp s\U+1ED1 l\U+1EBB sau s\U+1ED1 ph\U+1EA9y <"
		  (rtos *Sole* 2 2)
		 "> :"
	  )
 )
)
(if (not Sole) (setq Sole *Sole*) (setq *Sole* Sole))
(setvar "OSMODE" 9)
(setq STT 1)
(setq loop T)
(setq TD_Point (list))
(setq Lts (list))
(_layer2  "Heaven2407" 2)
(while loop
	(setq Pnt (getpoint "\nCh\U+1ECDn \U+0111i\U+1EC3m : "))
  	(cond
	  (T
		(if Pnt
		  (progn
;;;	    	  	(setq P_STT (Polar3 Pnt 0 (/ h 2.0)))
		    	(setq P_STT (Polar3 Pnt 0 0))
  			(wtxt (rtos STT 2 0) P_STT (* h 2.0) 0 "L" "Heaven2407")
		  	(setq TD_Point (list STT (list (car Pnt) (cadr Pnt))))
		    	(setq Lts (append Lts (list TD_Point)))
		    	
		  )
		  (setq loop nil)
		)
	  )
	)
  	(setq STT (1+ STT))
  	
)
(setq n (length Lts))
(setq P_dat (getpoint "\nCh\U+1ECDn \U+0111i\U+1EC3m ch\U+00E8n b\U+1EA3ng t\U+1ECDa \U+0111\U+1ED9 v\U+00E0 k\U+00EDch th\U+01B0\U+1EDBc: "))
(setq i 0)
(setq P1 (Polar3 P_dat (* 4.0 h) 0))
(setq P2 (Polar3 P1 (* h 8.0) 0))
(setq P3 (Polar3 P2 (* h 8.0) 0))
(setq P4 (Polar3 P3 (* h 6.0) 0))
(setq P_cuoi (Polar3 P_dat 0 (* -2.0 h n)))
(entmake (list (cons 0 "LINE") (cons 10 P_dat) (cons 11 P4)))
(entmake (list (cons 0 "LINE") (cons 10 P_dat) (cons 11 P_cuoi)))
(entmake (list (cons 0 "LINE") (cons 10 P_cuoi) (cons 11 (list (car P4) (cadr P_cuoi)))))
(entmake (list (cons 0 "LINE") (cons 10 (list (car P4) (cadr P_cuoi))) (cons 11 P4)))
(entmake (list (cons 0 "LINE") (cons 10 P1) (cons 11 (list (car P1) (cadr P_cuoi)))))
(entmake (list (cons 0 "LINE") (cons 10 P2) (cons 11 (list (car P2) (cadr P_cuoi)))))
(entmake (list (cons 0 "LINE") (cons 10 P3) (cons 11 (list (car P3) (cadr P_cuoi)))))
(setq P_Text (Polar3 P_dat (* h 2.0) (* h -2.0)))
(setq Pdat_KC (Polar3 P_Text (* h 21.0) (* h -1.0)))
(setq PLine_ngangdau (Polar3 P_dat 0.0 (* h -2.0)))
(while (< i (- n 1))
  	(setq P1 (car (cdr (nth i Lts))))
  	(setq P2  (car (cdr (nth (+ i 1) Lts))))
  	(setq KCLT  (distance P1 P2))
  
  
  	;;;GHI SO THU TU
  	(setq Pnt_i (Polar3 P_Text 0.0 (* i (* h -2.0))))
  	(setq NDSTT (car (nth i Lts)))
	(wtxt (rtos NDSTT 2 0) Pnt_i h 0 "BC" nil)

  	;;;GHI TOA DO X
	(setq Pnt_i_X (Polar3 P_Text (* h 6.0) (* i (* -2.0 h))))
  	(setq TD_X (car P1))
	(wtxt (rtos TD_X 2 Sole) Pnt_i_X h 0 "BC" nil)

  	;;;GHI TOA DO Y
	(setq Pnt_i_Y (Polar3 P_Text (* h 14.0) (* i (* -2.0 h))))
  	(setq TD_Y (cadr P1))
	(wtxt (rtos TD_Y 2 Sole) Pnt_i_Y h 0 "BC" nil)

  	;;;GHI KHOANG CACH
	(setq Pnt_KC (Polar3 Pdat_KC 0.0 (* i (* -2.0 h))))
	(wtxt (rtos KCLT 2 Sole) Pnt_KC h 0 "BC" nil)

  	;;KE LINE NGANG
	(setq P_Line (Polar3 PLine_ngangdau 0 (* i (* h -2.0))))
  	(entmake (list (cons 0 "LINE") (cons 10 P_Line) (cons 11 (list (car P3) (cadr P_Line) ))))
(setq i (1+ i))
)
  	;;;GHI SO THU TU
  	(setq Pnt_i1 (Polar3 P_Text 0 (* (- n 1) (* -2.0 h))))
  	(setq NDSTT1 (car (nth 0 Lts)))
	(wtxt (rtos NDSTT1 2 0) Pnt_i1 h 0 "BC" nil)

  	;;;GHI TOA DO X
	(setq Pnt_i_X1 (Polar3 P_Text (*  h 6.0) (* (- n 1) (* h -2.0))))
  	(setq TD_X1 (car (car (cdr (nth 0 Lts)))))
	(wtxt (rtos TD_X1 2 Sole) Pnt_i_X1 h 0 "BC" nil)

  	;;;GHI TOA DO Y
	(setq Pnt_i_Y1 (Polar3 P_Text (* h 14.0) (* (- n 1) (* -2.0 h))))
  	(setq TD_Y1 (cadr (car (cdr (nth 0 Lts)))))
	(wtxt (rtos TD_Y1 2 Sole) Pnt_i_Y1 h 0 "BC" nil)

  
(setvar "OSMODE" Olmode)
;;;(princ Lts1)
(princ)
)

(defun Polar3 (Pnt KC1 KC2 /  P1)
	(setq P1 (list (+ (car Pnt) KC1) (+ (cadr Pnt) KC2)))
)

(defun wtxt (string Point Height Ang justify Layer / Lst)
 (setq Lst (list '(0 . "TEXT")
   (cons 8 (if Layer Layer (getvar "Clayer")))
   (cons 62 (if Color Color 256))
   (cons 10 point)
   (cons 40 Height)
   (cons 1 string)
   (if Ang (cons 50 Ang))
   (cons 7 (if Style Style (getvar "Textstyle"))))
  justify (strcase justify))
 (cond ((= justify "C") (setq Lst (append Lst (list (cons 72 1) (cons 11 point)))))
       		((= justify "L") (setq Lst (append Lst (list (cons 72 0)(cons 73 0) (cons 10 point)))))
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
        	((= justify "BR") (setq Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 1))))))
 (entmake Lst)
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



(defun c:GKTC ( / ANS ENAME_OBJ K SS TLEBD)
(vl-load-com)
(setvar "CMDECHO" 0)
(setq k 0)
(or *h* (setq *h* 2))
(setq h (getreal (strcat "\nNh\U+1EADp chi\U+1EC1u cao Text <"
		  (rtos *h* 2 2)
		 "> :"
	  )
 )
)
(if (not h) (setq h *h*) (setq *h* h))
(or *Sole* (setq *Sole* 2))
(setq Sole (getint (strcat "\nNh\U+1EADp s\U+1ED1 l\U+1EBB sau s\U+1ED1 ph\U+1EA9y <"
		  (rtos *Sole* 2 2)
		 ">: "
	  )
 )
)
(if (not Sole) (setq Sole *Sole*) (setq *Sole* Sole))
(initget "C K")
(setq ans (getkword "\n Ban muon hien kich thuoc qui chu khong < C / K >: "))
(cond
  ( (= (strcase ans) "C")
      (progn
    	(breakall (ssget  (list (cons 0 "*LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
	(alert "Da tao nut tai cac duong giao nhau, ban chon lai tap chon")
	(setq ss (ssget  (list (cons 0 "*LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
      )
  )
  ( (= (strcase ans) "K")
      (setq ss (ssget  (list (cons 0 "*LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
  )
)

(repeat (sslength ss)  
  	(setq Ename_Obj (ssname ss k))
  	(cond
		((= (cdr (assoc 0 (entget Ename_Obj))) "LINE")
		(GKT_Line Ename_Obj)
		)
		((= (cdr (assoc 0 (entget Ename_Obj))) "LWPOLYLINE")
		(GKT_PLine Ename_Obj)
		)
	)
  	(setq k (1+ k))
)
(princ)
)


(defun GKT_PLine ( ob / DG_I GOC I KC_I N OLMODE P PG_IA PG_IB PII PVI P_2 TLE);Ghi kich thuoc
	(setq Olmode (getvar "OSMODE"))
	(setvar "OSMODE" 0)
  	(setq  n (vlax-curve-getEndParam ob)
               i 0
        )
	(while (< i n)
		(setq P (vlax-curve-getPointAtParam ob i))
	  	(setq P_2 (vlax-curve-getPointAtParam ob (+ i 1)))
	  	(setq goc (angle P P_2))
	  	(setq KC_i  (distance P P_2) )
	  	(setq DG_i (polar P goc (/ KC_i 2)))
	  	(setq Pii (polar P (/ pi 2) h))
	  	(setq PVi (Atan2 P P_2))
	  	(if (< (Rad_to_Do PVi) 180)
		  (progn
		    (setq PG_ia (polar DG_i (+ (/ pi 2) goc) (* h 0.5)))
	  	    (MakeText PG_ia (rtos KC_i 2 Sole) h goc "C")
		  )
		  (progn
		    (setq PG_ib (polar DG_i (+ (/ pi 2) (angle P_2 P)) (* h 0.5)))
		    (MakeText PG_ib (rtos KC_i 2 Sole) h  (angle P_2 P) "C")
		  )
		)
		(setq i (1+ i))
	)
(setvar "OSMODE" Olmode)
(princ )
)
;;GKT_Line
(defun GKT_Line ( Ename_Line / DG_I GOC KC_I OLMODE P PG_IA PG_IB PII PVI P_2 TEMP)
	(setq Olmode (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	(setq temp   (entget Ename_Line))
    	(setq  P (cdr (assoc 10 temp)))
  	(setq  P_2 (cdr (assoc 11 temp)))
  	(setq goc (angle P P_2))
  	(setq KC_i  (distance P P_2) )
  	(setq DG_i (polar P goc (/ KC_i 2)))
  	(setq Pii (polar P (/ pi 2) h))
  	(setq PVi (Atan2 P P_2))
  	(if (< (Rad_to_Do PVi) 180)
	  (progn
	    (setq PG_ia (polar DG_i (+ (/ pi 2) goc) (* h 0.5)))
  	    (MakeText PG_ia (rtos KC_i 2 Sole) h goc "C")
	  )
	  (progn
	    (setq PG_ib (polar DG_i (+ (/ pi 2) (angle P_2 P)) (* h 0.5)))
	    (MakeText PG_ib (rtos KC_i 2 Sole) h  (angle P_2 P) "C")
	  )
	)

(setvar "OSMODE" Olmode)
(princ )
)
(defun mid (p1 p2)
  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0)) p1 p2)
)
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(defun Area (ent)
;;;(setvar "hpgaptol" 0.1)
;;;(vla-get-area (vlax-ename->vla-object ent))
;;;)
;;;(defun mid (ent / p1 p2)
;;;	(vla-getboundingbox (vlax-ename->vla-object ent) 'p1 'p2)
;;;	(setq p1 (vlax-safearray->list p1)
;;;				p2 (vlax-safearray->list p2)
;;;				pt (mapcar '+ p1 p2)
;;;				pt (mapcar '* pt '(0.5 0.5 0.5))
;;;	)
;;;	pt
;;;)
(defun wtxt_l (txt p / D H H1 H2 STY WF) ;;;Write txt on graphic screen at p
(setq    sty (getvar "textstyle")    
d (tblsearch "style" sty)    
h1 (cdr (assoc 40 d))    
h2 (cdr (assoc 42 d))    
wf (cdr (assoc 41 d)))
(if (> h1 0) (setq h h1) (setq h h2))
(entmake (list (cons 0 "TEXT") (cons 7 sty) (cons 40 h) (cons 41 wf)(cons 72 4)(cons 11 p)(cons 62 4) (cons 1 txt) (cons 10 p)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defun DTR (Do / radian)
   (setq radian  (/ (* Do pi ) 180))
)
(defun LM:GetXWithDefault ( _function _prompt _symbol _default / _toString )
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

(defun MakeText (point string Height Ang justify     / Lst); Ang: Radial
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


;revlwpl
(defun revlwpl ( ent1 / eo el len)
(setq ent (entget  ent1))
(vl-load-com)
(setq eo ent)
(setq el (list (assoc 210 ent)))
(while (member (assoc 10 ent) ent)
  (if (= 0.0 (assoc 42 ent))
(setq el (cons (assoc 42 ent) el))
(setq el (cons (cons 42 (- (cdr (assoc 42 ent)))) el))
  )
  (setq el (cons (assoc 41 ent) el))
  (setq el (cons (assoc 40 ent) el))
  (setq el (cons (assoc 10 ent) el))
  (setq ent (member (assoc 10 ent) ent))
  (setq ent (cdr ent))
)
(setq len(- (LENGTH eo) (LENGTH (member (assoc 10 eo) eo)) 1))
(while (>= len 0)
  (setq el (cons (nth len eo) el))
  (setq len (- len 1))
)
(setq ent el)
(entmod ent)
(princ)
)

(defun Rad_to_Do(radian / Do)
   (setq Do (/ (* radian 180) pi))
)
(defun Do_to_Radian (Do / radian)
   (setq radian  (/ (* Do pi ) 180))
)
(defun R2DPG (gocR / DPG Toando Do Phut1 Phut Giay DPG)
  (setq DPG (list))
  (setq Toando (Rad_to_Do gocR))
  (setq Do (fix Toando))
  (setq Phut1  (* (- Toando Do) 60))
  (setq Phut (fix Phut1))
  (setq Giay   (atof (rtos (* (- phut1 phut) 60) 2 3)))
  (setq DPG (list Do  Phut giay))
  DPG
)
(defun DPG_to_DO (Goc)
(setq DD (nth 0 Goc))
(setq PP (/ (nth 1 Goc) 60))
(setq GG (/ (nth 2 Goc) 3600))
(setq DDD (+ DD PP GG))
DDD
)
(defun Dogoc2diem (P1 P2 /)
  (setq gocP12 (angle P1 P2))
  (setq gocP12_DPG (R2DPG gocP12))
  (setq Goc_12 (DPG_to_DO gocP12_DPG))
  Goc_12
)
(defun Do_to_DPG (Toando /)
  (setq Do (fix Toando))
  (setq Phut1 (* (- Toando Do) 60))
  (setq Phut (fix Phut1))
  (setq Giay   (atof (rtos (* (- phut1 phut) 60) 2 3)))
  (setq DPG (list Do  Phut giay))
  DPG
)

(defun Atan2 (P1 P2 / gocAtan b)
  (setq dx ( - (car P2) (car P1)))
  (setq dy ( - (cadr P2) (cadr P1)))
  (setq gocAtan (list))
  (cond
    ((and (= dx 0) (> dy 0) )
      (setq gocAtan 0)
    )
    ((and (= dx 0) (< dy 0) )
      (setq gocAtan pi)
    )
    ((and  (< dx 0) (= dy 0) )
      (setq gocAtan (/ (* 3 pi) 2))
    )
    ((and  (> dx 0) (= dy 0) )
      (setq gocAtan pi)
    )
    ((and  (= dx 0) (= dy 0) )
      (setq gocAtan 0)
    )
    ((/= dx 0)
     (progn
	(setq b (atan (/ dx dy)))
	(cond
	    ((and (> dx 0) (>= dy 0))
		  (setq gocAtan b)
	    )
	    ((and (< dx 0) (> dy 0))
		  (setq gocAtan (+ (* pi 2) b))
	    )
	    ((and (< dx 0) (< dy 0))
		  (setq gocAtan (+ pi b))
	    )
	    ((and (> dx 0) (< dy 0))
	          (setq gocAtan (+ pi b))
	    )
	)
      )
    )  
  )
  gocAtan
)




(defun breakall (a / b i p selset j ent)
  (command ".undo" "be")
  (setting)
  (setq b (findallintersction a))
  (setq i 0)
  (repeat (length b)
    (setq p (nth i b))
    (setq selset (selectatonepoint p))
    (setq j 0)
    (repeat (sslength selset)
      (setq ent (ssname selset j))
      (newbreak ent p)             
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  (resetting)
  (command ".undo" "e")
  (princ)
)
(defun selectatonepoint (a / tor p1 p2 ss)
  (setq tor 0.01)
  (setq p1 (polar a (* (/ 135 180) pi) tor))
  (setq p2 (polar a (* (/ 315 180) pi) tor))
  (setq ss (ssget "_c" p1 p2))           
  ss
)
(defun findallintersction (sset / interlst ssl i e1 j e2 l)
  (setq interlst nil)
  (setq ssl (sslength sset)
    i 0
  )
  (repeat ssl
    (setq e1 (ssname sset i))
    (setq j (1+ i))
    (repeat (- ssl (1+ i))
      (setq e2 (ssname sset j))
      (setq l (kht-intersect e1 e2))
      (setq interlst (append
               interlst
               l
             )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  interlst
)
(defun kht-intersect (en1 en2 / a b x ex ex-app c d e)
  (vl-load-com)
  (setq c (cdr (assoc 0 (entget en1)))
    d (cdr (assoc 0 (entget en2)))
  )
  (if (or
    (= c "TEXT")
    (= d "TEXT")
      )
    (setq e -1)
  )
  (setq En1 (vlax-ename->vla-object En1))
  (setq En2 (vlax-ename->vla-object En2))
  (setq a (vla-intersectwith en1 en2 acExtendNone))
  (setq a (vlax-variant-value a))
  (setq b (vlax-safearray-get-u-bound a 1))
  (if (= e -1)
    (setq b e)
  )
  (if (/= b -1)
    (progn
      (setq a (vlax-safearray->list a))
      (repeat (/ (length a) 3)
    (setq ex-app (append
               ex-app
               (list (list (car a) (cadr a) (caddr a)))
             )
    )
    (setq a (cdr (cdr (cdr a))))
      )
      ex-app
    )
    nil
  )
)
(defun kht:list->safearray (lst datatype)
  (vlax-safearray-fill (vlax-make-safearray (eval datatype) (cons 0
                                  (1-
                                      (length lst)
                                  )
                                )
               ) lst
  )
)
(defun newbreak (ent pt / obj2Break p1param p2param p2)
  (setq obj2Break (vlax-ename->vla-object ent))
  (cond
    ((and
       (= "AcDbSpline" (vla-get-objectname obj2Break)) 
                       
       (vlax-curve-isClosed obj2Break)
     )
      (setq p1param (vlax-curve-getparamatpoint obj2Break pt)
        p2param (+ p1param 0.000001)
        p2 (vlax-curve-getPointAtParam obj2Break p2param)
      )
      (command "._break" (vlax-vla-object->ename obj2Break) "non"
           (trans pt 0 1) "non" (trans p2 0 1)
      )
    )
    ((= "AcDbCircle" (vla-get-objectname obj2Break))
      (setq p1param (vlax-curve-getparamatpoint obj2Break pt)
        p2param (+ p1param 0.000001)
        p2 (vlax-curve-getPointAtParam obj2Break p2param)
      )
      (command "._break" (vlax-vla-object->ename obj2Break) "non"
           (trans pt 0 1) "non" (trans p2 0 1)
      )
      (setq en (entlast))
    )
    ((and
       (= "AcDbEllipse" (vla-get-objectname obj2Break))   
                       
       (vlax-curve-isClosed obj2Break)
     )                       
                       
      (setq p1param (vlax-curve-getparamatpoint obj2Break pt)
        p2param (+ p1param 0.000001) 
                     
        minparam (min
               p1param
               p2param
             )
        maxparam (max
               p1param
               p2param
             )
      )
      (vlax-put obj2Break 'startparameter maxparam)
      (vlax-put obj2Break 'endparameter (+ minparam (* pi 2)))
    )                      
                      
                       
    (t
      (command "._break" ent pt "@")
    )
  )
)
(defun err (s)
  (if (= s "Function cancelled")
    (princ "\nALIGNIT - cancelled: ")
    (progn
      (princ "\nALIGNIT - Error: ")
      (princ s)
      (terpri)
    )                      
  )                      
  (resetting)
  (princ "SYSTEM VARIABLES have been reset\n")
  (princ)
)
(defun setv (systvar newval / x)
  (setq x (read (strcat systvar "1")))
  (set x (getvar systvar))
  (setvar systvar newval)
)
(defun setting ()
  (setq oerr *error*)
  (setq *error* err)
  (setv "BLIPMODE" 0)
  (setv "CMDECHO" 0)
  (setv "OSMODE" 0)
)
(defun rsetv (systvar)
  (setq x (read (strcat systvar "1")))
  (setvar systvar (eval x))
)
(defun resetting ()
  (rsetv "BLIPMODE")
  (rsetv "CMDECHO")
  (rsetv "OSMODE")
  (setq *error* oerr)
)
;;; -------------------------------------------------------
