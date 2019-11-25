(vl-load-com)
;;;(Alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh tr\U+1EA3i Taluy"
;;;               "\n"
;;;               "\n- V\U+1EBD Taluy tr\U+00EAn m\U+1ED9t \U+0111\U+01B0\U+1EDDng: VTL"
;;;               "\n"
;;;               "\n- V\U+1EBD Taluy t\U+1EEB 2 \U+0111\U+01B0\U+1EDDng: VTL2"
;;;               "\n"
;;;               "\n- \U+0110\U+1EA3o chi\U+1EC1u Taluy: DCTL"
;;;               "\n"
;;;               "\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;               "\n"
;;;               "\nEmail: heaven2407@Gmail.com"
;;;               "\n"
;;;               "\nMobile: 0972.0168.25"
;;;       )
;;;)
;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh tr\U+1EA3i Taluy"
;;;               "\n"
;;;               "\n- V\U+1EBD Taluy tr\U+00EAn m\U+1ED9t \U+0111\U+01B0\U+1EDDng: VTL"
;;;               "\n"
;;;               "\n- V\U+1EBD Taluy t\U+1EEB 2 \U+0111\U+01B0\U+1EDDng: VTL2"
;;;               "\n"
;;;               "\n- \U+0110\U+1EA3o chi\U+1EC1u Taluy: DCTL"
;;;               "\n"
;;;               "\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;               "\n"
;;;               "\nEmail: heaven2407@Gmail.com"
;;;               "\n"
;;;               "\nMobile: 0972.0168.25"
;;;       )
;;;)
(defun C:VTL( /  ObjPline LtsTaluy LtsLDai LtsLNgan e1 e2 e3 e4 ang1 ang2 ang3 ang4 Pnt10N Pnt10D Pnt11N Pnt11D Chon)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))

(or *CDTLN* (setq *CDTLN* 1))
(setq CDTLN (getreal (strcat "\nChi\U+1EC1u d\U+00E0i c\U+1EE7a v\U+1EA1ch nh\U+1ECF < "
			  (rtos *CDTLN* 2 2)
			 " > :"
		    )
	 )
)
(if (not CDTLN) (setq CDTLN *CDTLN*) (setq *CDTLN* CDTLN))

(or *CDTLD* (setq *CDTLD* 2))
(setq CDTLD (getreal (strcat "\nChi\U+1EC1u d\U+00E0i c\U+1EE7a v\U+1EA1ch d\U+00E0i < "
			  (rtos *CDTLD* 2 2)
			 " > :"
		    )
	 )
)
(if (not CDTLD) (setq CDTLD *CDTLD*) (setq *CDTLD* CDTLD))

  
(or *Sovachngan* (setq *Sovachngan* 2))
(setq Sovachngan (getint (strcat "\nS\U+1ED1 v\U+1EA1ch nh\U+1ECF gi\U+1EEFa 2 v\U+1EA1ch l\U+1EDBn: < "
			  (rtos *Sovachngan* 2 0)
			 " > :"
		  )
	 )
)
(if (not Sovachngan) (setq Sovachngan *Sovachngan*) (setq *Sovachngan* Sovachngan))

(while (setq ObjPline (car (entsel "\nCh\U+1ECDn Pline: ")))
  	(setq VlaObjPL (vlax-ename->vla-object ObjPline))
        (setq LtsTaluy (VTLL CDTLN CDTLD Sovachngan ObjPline))
	(setq LtsLDai (car LtsTaluy))
	(setq LtsLNgan (cadr LtsTaluy))
  	 (setq Chon (strcase (getstring "\n(Ghi ch\U+00FA: U - L\U+00E0m l\U+1EA1i, C - \U+0110\U+1ED5i chi\U+1EC1u v\U+1EA1ch, G\U+00F5 b\U+1EA5t k\U+1EF3 \U+0111\U+1EC3 ti\U+1EBFp t\U+1EE5c) ")))
         (cond
           ((= Chon  "U")
	     (progn
	        (foreach e1 LtsLDai
			(entdel e1)
		)
	        (foreach e2 LtsLNgan
			(entdel e2)
		)
	     )
	    )
	   ((= Chon  "C")
	     (progn
	        (foreach e3 LtsLDai
		  	(progn
				(setq Pnt10D (cdr (assoc 10 (entget e3))))
			  	(setq ang3 (angle '(0 0) (Vlax-curve-getfirstderiv VlaObjPL (vlax-curve-getParamAtPoint VlaObjPL Pnt10D))))
			  	(setq Pnt11D (polar Pnt10D (- ang3 (/ pi 2) )  CDTLD))
		  		(entmod (subst (cons 11 Pnt11D) (assoc 11 (entget e3)) (entget e3) ))
			)
		)
	        (foreach e4 LtsLNgan
		  	(progn
				(setq Pnt10N (cdr (assoc 10 (entget e4))))
			  	(setq ang4 (angle '(0 0) (Vlax-curve-getfirstderiv VlaObjPL (vlax-curve-getParamAtPoint VlaObjPL Pnt10N))))
			  	(setq Pnt11N (polar Pnt10N (- ang4 (/ pi 2) ) CDTLN))
		  		(entmod (subst (cons 11 Pnt11N) (assoc 11 (entget e4)) (entget e4) ))
			)
		)
	     )
	    )
	   ((or (/= Chon  "U") (/= Chon  "C"))
	     (setq Chon nil)
	    )
	 )
   )
(setvar "OSMODE" Olmode)
(princ)
)

(defun VTLL (CDTLN CDTLD Sovachngan ObjPline / CDTLD CDDoan n d1 d2 CDaiPLine ang2 Ptd Lts1 Lts2 LtsPntNgan Pnt1  EnameLD )
(MakeLayer_ "TALUY" 7)
(setq CDDoan (* (+ Sovachngan 1) CDTLN ))
(setq VlaObjPline (vlax-ename->vla-object ObjPline))
(setq CDaiPLine (vla-get-length VlaObjPline))
(setq n (fix (/ CDaiPLine CDDoan)))
(setq d1 0)
(setq Lts1 (list))
(setq LtsEnameLD (list))
(setq LtsEnameLN (list))
(while (< d1 CDaiPLine)
	(progn
		(setq Ptd (vlax-curve-getPointAtDist VlaObjPline d1))
		(setq d1 (+ d1 CDDoan))
	  	(setq ang2 (angle '(0 0) (Vlax-curve-getfirstderiv VlaObjPline (vlax-curve-getParamAtPoint VlaObjPline Ptd))))
	  	(entmake (list (cons 0 "LINE") (cons 8 "TALUY") (cons 10  Ptd) (cons 11 (polar Ptd (+ ang2 (/ pi 2) ) CDTLD))))
	  	(setq EnameLD (entlast))
	  	(setq LtsEnameLD (append LtsEnameLD (list EnameLD)))
	  	(setq Lts1 (append Lts1 (list Ptd)))
	)
)
(setq d2 0)
(setq Lts2 (list))
(setq m (fix (/ CDaiPLine CDTLN)))
(while (< d2 CDaiPLine)
	(progn
		(setq Ptn (vlax-curve-getPointAtDist VlaObjPline d2))
		(setq d2 (+ d2 CDTLN))
	  	(setq Lts2 (append Lts2 (list Ptn)))
	)
)
(setq LtsPntNgan (LM:ListDifference Lts2 Lts1))
(foreach Pnt1 LtsPntNgan
  	(setq ang3 (angle '(0 0) (Vlax-curve-getfirstderiv VlaObjPline (vlax-curve-getParamAtPoint VlaObjPline Pnt1))))
  	(entmake (list (cons 0 "LINE") (cons 10  Pnt1) (cons 8 "TALUY") (cons 11 (polar Pnt1 (+ ang3 (/ pi 2) ) CDTLN))))
  	(setq EnameLN (entlast))
  	(setq LtsEnameLN (append LtsEnameLN (list EnameLN)))
)
(setq DsTaluy (list LtsEnameLD LtsEnameLN))
DsTaluy
)


(defun MakeLayer_ ( name colour /)
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

(defun C:VTL2 (  / Olmode Sovachngan  *Sovachngan* CDVN *CDVN*  CDDoan ObjPline1 ObjPL2 ObjPline2 VlaObjPline1 CDaiPLine1 VlaObjPline2
		   n d1 d2 LtsEnameLD LtsEnameLN Lts1 Lts2 LtsPntNgan   PntInObjPline2 PntInObjPline3 ang_1 ang_2 P3
	       )
(MakeLayer_ "TALUYN" 1)
(MakeLayer_ "TALUYD" 7)
;;;;;;;;;LUU OSNAP KHI BREAK, CANCEL, EXIT
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Olmode (getvar "OSMODE"))
;;;(setq Sovachngan 1)

(or *Sovachngan* (setq *Sovachngan* 1))
(setq Sovachngan (getint (strcat "\nNhap so vach ngan giua 2 vach dai: < "
			  (rtos *Sovachngan* 2 0)
			 " > :"
		    )
	 )
)
(if (not Sovachngan) (setq Sovachngan *Sovachngan*) (setq *Sovachngan* Sovachngan))
  
(or *CDVN* (setq *CDVN* 2.5))
(setq CDVN (getreal (strcat "\nNh\U+1EADp kho\U+1EA3ng c\U+00E1ch gi\U+1EEFa c\U+00E1c v\U+1EA1ch: < "
			  (rtos *CDVN* 2 2)
			 " > :"
		    )
	 )
)
(if (not CDVN) (setq CDVN *CDVN*) (setq *CDVN* CDVN))
(setq CDDoan (* (+ Sovachngan 1) CDVN ))


  
(setq ObjPline1 (car (entsel "\nChon duong thu nhat: ")))
(setq ObjPL2  (entsel "\nChon duong thu hai: "))
(setq ObjPline2 (car ObjPL2))
;;;(setq PickPoint (cdr ObjPL2))

(setq VlaObjPline1 (vlax-ename->vla-object ObjPline1))
(setq CDaiPLine1 (vla-get-length VlaObjPline1))


(setq VlaObjPline2 (vlax-ename->vla-object ObjPline2))
  
(setq n (fix (/ CDaiPLine1 CDDoan)))
(setq d1 0)
(setq Lts1 (list))
(setq LtsEnameLD (list))
(setq LtsEnameLN (list))
(while (< d1 CDaiPLine1)
	(progn
		(setq Ptd (vlax-curve-getPointAtDist VlaObjPline1 d1))
		(setq d1 (+ d1 CDDoan))
	  	(setq ang_1 (angle '(0 0) (Vlax-curve-getfirstderiv VlaObjPline1 (vlax-curve-getParamAtPoint VlaObjPline1 Ptd))))
	  	(if (setq PntInObjPline2 (TDKDGN Ptd ObjPline2 (polar Ptd (+ ang_1 (/ pi 2) ) CDVN)))
		    (progn
	  	    	(entmake (list (cons 0 "LINE") (cons 8 "TALUYD") (cons 10  Ptd) (cons 11 PntInObjPline2)))
	  	    	(setq EnameLD (entlast))
	  		(setq LtsEnameLD (append LtsEnameLD (list EnameLD)))
	  		(setq Lts1 (append Lts1 (list Ptd)))
		    )
		)
	)
)
(setq d2 0)
(setq Lts2 (list))
(setq m (fix (/ CDaiPLine1 CDVN)))
(while (< d2 CDaiPLine1)
	(progn
		(setq Ptn_N (vlax-curve-getPointAtDist VlaObjPline1 d2))
		(setq d2 (+ d2 CDVN))
	  	(setq Lts2 (append Lts2 (list Ptn_N)))
	)
)
(setq LtsPntNgan (LM:ListDifference Lts2 Lts1))
(foreach Pnt1 LtsPntNgan
  	(setq ang_2 (angle '(0 0) (Vlax-curve-getfirstderiv VlaObjPline1 (vlax-curve-getParamAtPoint VlaObjPline1 Pnt1))))
  	(if (setq PntInObjPline3 (TDKDGN Pnt1 ObjPline2 (polar Pnt1 (+ ang_2 (/ pi 2) ) CDVN)))
	    (progn
	  	(setq P3 (list (/ (+ (car Pnt1) (car PntInObjPline3)) 2) (/ (+ (cadr Pnt1) (cadr PntInObjPline3)) 2)))
	  	(entmake (list (cons 0 "LINE") (cons 10  Pnt1) (cons 8 "TALUYN") (cons 11 P3)))
	    )
	)
)
(princ)
)





;;;HAM LAY RA CAC PHAN TU KHAC NHAU TRONG DANH SACH 1 SO VOI DANH SACH 2 (TO - CON) (LEN L1 > LEN L2)
;;;(LM:ListDifference '(1 2 3 4 5) '(2 4 6)  )
(defun LM:ListDifference ( l1 l2 )
  (if l1
    (if (member (car l1) l2)
      (LM:ListDifference (cdr l1) l2)
      (cons (car l1) (LM:ListDifference (cdr l1) l2))
    )
  )
)
;;--------------------=={ Intersections }==-------------------;;
;;                                                            ;;
;;  Returns a list of all points of intersection between      ;;
;;  two objects for the given intersection mode.              ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright  2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  obj1, obj2 - VLA-Objects                                  ;;
;;  mode       - acextendoption enum of intersectwith method  ;;
;;------------------------------------------------------------;;
;;  Returns:  List of intersection points, or nil             ;;
;;------------------------------------------------------------;;

;;;;;;;;;;;;;;;acextendnone	Do not extend either object
;;;;;;;;;;;;;;;acextendthisentity	Extend obj1 to meet obj2
;;;;;;;;;;;;;;;acextendotherentity	Extend obj2 to meet obj1
;;;;;;;;;;;;;;;acextendboth	Extend both objects

(defun LM:Intersections ( obj1 obj2 mode / l r )
    (setq l (vlax-invoke obj1 'intersectwith obj2 mode))
    (repeat (/ (length l) 3)
        (setq r (cons (list (car l) (cadr l) (caddr l)) r)
              l (cdddr l)
        )
    )
    (reverse r)
)

(defun MakeXline (pt vec)
  (entmakex (list (cons 0 "XLINE")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbXline")
                  (cons 10 pt)
                  (cons 11 vec)
	    )
  )
)

(defun TDKDGN (P1 ObjPline1 Pnt / Vla:ObjPline1   EnameXline Vla:Xline LtsPnt ) ;;;TIM DIEM KEO DAI GAN NHAT
(setq Vla:ObjPline1 (vlax-ename->vla-object ObjPline1))
(setq P2 (mapcar '- Pnt P1))
(MakeXline P1 P2)
(setq EnameXline (entlast))
(setq Vla:Xline (vlax-ename->vla-object EnameXline))
(setq LtsPnt (LM:Intersections Vla:ObjPline1  Vla:Xline acextendboth))
(entdel EnameXline)
(setq PntNear (car (vl-sort LtsPnt '(lambda(x y) (< (distance x P1) (distance y P1))))))
PntNear
)


(defun C:DCTL( / ss LtsEnameLine P1 P2  PVG1 PVG2 CDLine);;;DAO CHIEU TALUY
(setq VLA:ObjPline (vlax-ename->vla-object (car (entsel "\nChon Polyline can dao chieu Taluy:"))))
(Alert "\nQuet chon Line")
(setq ss (ssget (list (cons 0 "LINE"))))
(setq LtsEnameLine (acet-ss-to-list ss))
(setq i 0)
(foreach EnameL LtsEnameLine
	(setq P1 (acet-dxf 10 (entget EnameL)))
	(setq P2 (acet-dxf 11 (entget EnameL)))
	(if (and (setq Pgiao (last (LM:Intersections (vlax-ename->vla-object EnameL) VLA:ObjPline acextendnone ))) (equal (LineVGtPline VLA:ObjPline EnameL) 1 0.0000000001))
	    (progn
	    	(setq CDLine (vla-get-length (vlax-ename->vla-object EnameL)))
	    	(cond ((equal P1 Pgiao 0.00000001)
		      	      (progn
			    	(setq angL10 (angle '(0 0) (Vlax-curve-getfirstderiv VLA:ObjPline (vlax-curve-getParamAtPoint VLA:ObjPline Pgiao))))
				(setq P2A (polar Pgiao (+ angL10 (* -1.0 (PointLeftRightPline VLA:ObjPline P2)  (/ pi 2))) CDLine))
			      	(entmod (subst (cons 11 P2A) (assoc 11 (entget EnameL)) (entget EnameL) ))
			      )
		      )
		      ((equal P2 Pgiao 0.00000001)
		      	      (progn
			    	(setq angL11 (angle '(0 0) (Vlax-curve-getfirstderiv VLA:ObjPline (vlax-curve-getParamAtPoint VLA:ObjPline Pgiao))))
				(setq P1A (polar Pgiao (+ angL11 (* -1.0 (PointLeftRightPline VLA:ObjPline P1)  (/ pi 2))) CDLine))
			      	(entmod (subst (cons 10 P1A) (assoc 10 (entget EnameL)) (entget EnameL) ))
			      )
		      )
		    )
		 )
	)
)
(princ)
)


  
(defun LineVGtPline (VLA:ObjPline ObjLine / PVG Pd1 Pd2 GocP1P2  VLA:ObjPline VLA:ObjLine PntGiao );;;;XET LINE VUONG GOC VOI POLYLINE HAY KHONG?
	(setq P1 (acet-dxf 10 (entget ObjLine)))
	(setq P2 (acet-dxf 11 (entget ObjLine)))
  	(setq GocP1P2 (angle P1 P2))
  	(setq GocP2P1 (angle P2 P1))
  	(setq VLA:ObjLine (vlax-ename->vla-object ObjLine))
        (setq PntGiao (last (LM:Intersections VLA:ObjLine VLA:ObjPline acextendnone)))
  	(setq Goctaidiemgiao (angle '(0 0) (Vlax-curve-getfirstderiv VLA:ObjPline (vlax-curve-getParamAtPoint VLA:ObjPline PntGiao))))
  	(setq KQVG nil)
	(if (or (equal (+ Goctaidiemgiao (/ pi 2)) GocP1P2 0.00000000001) (equal (- Goctaidiemgiao (/ pi 2)) GocP1P2 0.00000000001)
	        (equal (+ Goctaidiemgiao (/ pi 2)) GocP2P1 0.00000000001) (equal (- Goctaidiemgiao (/ pi 2)) GocP2P1 0.00000000001))
	    (setq KQVG 1)
	    (setq KQVG 0)
        )
  KQVG
)


(defun PointLeftRightPline (ObjPline Pnt / PVG Pd1 Pd2);;;;XET DIEM NAM TRAI HAY PHAI PLINE
  (setq PVG (vlax-curve-getClosestPointTo ObjPline Pnt)
         Pd1 (vlax-curve-getpointAtParam ObjPline (fix (vlax-curve-getparamatPoint ObjPline PVG)))
         Pd2 (vlax-curve-getpointAtParam ObjPline (1+ (fix (vlax-curve-getparamatPoint ObjPline PVG))))
    )
    (setq Kqua nil)
  (if (or (equal (cos (+ (/ pi 2) (angle Pd1 Pd2))) (cos (angle Pnt PVG)) 0.00000001)
 	    (equal (sin (+ (/ pi 2) (angle Pd1 Pd2))) (sin (angle Pnt Pd1)) 0.00000001)
	)
     (setq Kqua -1)
        (setq Kqua 1)
    )
   Kqua
)




