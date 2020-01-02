;Shusei Hayashi
;OffshoreCad&Management Inc.
;10F Jaka Bldg., 6780 Ayala Ave.,
;Makati, Philippines
;http://www.offshorecad.com.ph/
;http://www.offshore-management.com.ph/
; Slightly modified to work with AutoCAD 2014 & 2015
; Greg Battin www.autocadtips.wordpress.com
; 

(defun c:JD( /  Flag ObjName1 ObjName2 Ang Ang2 Pt1 Pt2 Pt3 Pt4)
	(princ "\n convert two dimensions to total Dimension on the same position")
	(princ "\n **********************************")
	(setq *error* *myerror*)   
	(SD1028)
	(setq LegLen 7.0)
	(setq Flag nil)
	(get_layer&ltype&color)
	(RegistryRead_1001)
	(setvar "Clayer" Lay1)
	(setvar "Cecolor" Col1)
	(setvar "Celtype" LT1)
	(while (= Flag nil)
	   (setq ObjName1 (car (entsel "\n Select 1st Dimension :")))
		(if (and ObjName1(= (cdr (assoc 0 (setq Data1 (entget ObjName1)))) "DIMENSION"))
			(setq Flag T)
		)
	)
	(redraw ObjName1 3)
	(setq theStyle (SD3511 3 ObjName1))
	(setq Flag nil)
	(while (= Flag nil)
		(setq ObjName2 (car (entsel "\n Select 2nd Dimension :")))
		(if 	(and 	ObjName2 
				(= (cdr (assoc 0 (setq Data2 (entget ObjName2)))) "DIMENSION")
				(or	(equal (abs (- (cdr (assoc 50 Data1))(cdr (assoc 50 Data2)))) 0.0 0.0001)
					(equal (abs (- (cdr (assoc 50 Data1))(cdr (assoc 50 Data2)))) pi 0.0001)
				)
			)
			(setq Flag T)
				(princ "\n two dimensions should have same angle")
		)
	)
	(redraw ObjName1 4)
	
	;
	(setq PList (list (cdr (assoc 13 Data1))(cdr (assoc 14 Data1))(cdr (assoc 13 Data2))(cdr (assoc 14 Data2))))
	;
	(setq PList2 (list (cdr (assoc 10 Data1)) (cdr (assoc 10 Data2))))
	;
	(if (/= (distance (cdr (assoc 14 Data1))(cdr (assoc 10 Data1))) 0)
		(setq Ang (angle (cdr (assoc 14 Data1))(cdr (assoc 10 Data1))))
		(setq Ang (+ (angle (cdr (assoc 13 Data1))(cdr (assoc 14 Data1))) (* 0.5 pi)))
	)
	
	(setq Ang2 (+ Ang (* -0.5 pi)))
	;ƒŒƒCƒ„[
	(setq ItsLayer (cdr (assoc 8 Data1)))
	
	;
	(setq PList_n (mapcar '(lambda(x)(SD1862 x Ang2))PList))
	(setq PList2_n (mapcar '(lambda(x)(SD1862 x Ang2))PList2))
	;
	(setq PList_n_x (mapcar 'car PList_n))
	;
	(setq PList2_n_y (mapcar 'cadr PList2_n))
	
	;
	(setq Position1 (vl-position (apply 'min PList_n_x) PList_n_x))
	;
	(setq Position2 (vl-position (apply 'max PList_n_x) PList_n_x))
	;
	(setq Position3 (vl-position (apply 'max PList2_n_y) PList2_n_y))
	
	
	(setq Pt1 (nth Position1 PList))
;	(checkcircle Pt1 1.0 "A21")
	(setq Pt2 (nth Position2 PList))
;	(checkcircle Pt2 1.0 "A31")
	(setq Pt3 (nth Position3 PList2))
;	(checkcircle Pt3 1.0 "A51")
;	(setq Pt4 (polar Pt3 Ang (* LegLen Scale)))
;	(checkcircle Pt4 1.0 "A21")
	
	(setq 	Pt1 (trans Pt1 0 1)
			Pt2 (trans Pt2 0 1)
			Pt3 (trans Pt3 0 1)
;			Pt4 (trans Pt4 0 1)
	)
	(setq UAng (angle '(0 0) (getvar "UCSXDIR")))
	(setq Ang2 (- Ang2 UAng))
	(command "._dimstyle" "RE" theStyle)
	(command "dimrotated" (* 180 (/ Ang2 pi)) Pt1 Pt2 Pt3)
	(command "change" (entlast) "" "P" "LA" ItsLayer "")
	(command "_matchprop" ObjName1 (entlast) "")
	(entdel ObjName1)(entdel ObjName2)
	
	(SD2056)
	(setq *error* nil)
	(princ)
)
;-----------------------------------------
(defun RegistryRead_1001()
	(setq Path1001 "HKEY_CURRENT_USER\\Software\\SpeedDraftLT\\SD_1001")
	(if (vl-registry-read Path1001 "LegLen" )
		(progn 	(set_tile "LegLen" (vl-registry-read Path1001 "LegLen"))
				(setq LegLen (atof (vl-registry-read Path1001 "LegLen")))
		)
		(setq LegLen 7.0)
	)
	(if (and (vl-registry-read Path1001 "Lay1" )(member (vl-registry-read Path1001 "Lay1") Laylist1))
		(progn 	(set_tile "Lay1" (itoa (vl-position (vl-registry-read Path1001 "Lay1") Laylist1)))
				(setq Lay1 (vl-registry-read Path1001 "Lay1"))
		)
		(progn 	(setq Lay1 (getvar "Clayer"))(set_tile "Lay1" (itoa (vl-position Lay1 Laylist1))))
	)
	(if (and (vl-registry-read Path1001 "LT1" )(member (vl-registry-read Path1001 "LT1") Laylist3))
		(progn 	(set_tile "LT1" (itoa (vl-position (vl-registry-read Path1001 "LT1") Laylist3)))
				(setq LT1 (vl-registry-read Path1001 "LT1"))
		)
		(progn 	(setq LT1 "ByLayer")(set_tile "LT1" "0"))
	)
	(if (and (vl-registry-read Path1001 "Col1" )(member (vl-registry-read Path1001 "Col1") Laylist2))
		(progn 	(set_tile "Col1" (itoa (vl-position (vl-registry-read Path1001 "Col1") Laylist2)))
				(setq Col1 (vl-registry-read Path1001 "Col1"))
		)
		(progn 	(setq Col1 "ByLayer")(set_tile "Col1" "0"))
	)
)
;----------------
(defun SD1028 ()
  (setq OldCmdEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "undo" "be")
  (setq OldOsmode (getvar "OSMODE"))
  (setq OldLayer (getvar "CLAYER"))
  (setq OldLType (getvar "CeLType"))
  (setq OldCeLWeight (getvar "CeLWeight"))
  (setq OldColor (getvar "CeColor"))
  (setq OldOrtho (getvar "ORTHOMODE"))
  (setq OldDStyle  (getvar "DIMSTYLE"))
  (setq OldExpert (getvar "Expert"))
  (setvar "EXPERT" 0)
  (setq Path_Lang "HKEY_CURRENT_USER\\Software\\SpeedDraftLT")
;  (if (vl-registry-read Path_Lang "SD_Language" )
;  	(setq SD:Lang (vl-registry-read Path_Lang "SD_Language" ))
;  	(progn	(setq SD:Lang "J")
;  			(vl-registry-write Path_Lang "SD_Language" "J")
;  	)
;  )
  (princ)
)
;********************************
(defun SD2056 ()
  (setvar "OSMODE" OldOsmode)
  (command "undo" "end")
  (setvar "CLAYER" OldLayer)
  (setvar "CeLType" OldLType)
  (setvar "CeLWeight" OldCeLWeight)
  (setvar "CeColor" OldColor)
  (setvar "ORTHOMODE" OldOrtho)
  (setvar "Expert" OldExpert)
  (if (and (/= (getvar "DIMSTYLE") OldDStyle)(tblsearch "DIMSTYLE" OldDStyle))
  	(command "-dimstyle" "Restore" OldDStyle)
  )
  (princ "\n (C)OffshoreCad&Management")
  (setvar "CMDECHO" OldCmdEcho)
  (princ)
)

;********************************
(defun get_layer&ltype&color()
	(setq 	Lay (tblnext "LAYER" T)
			LT (tblnext "LTYPE" T)
			Laylist1 (list)
			Laylist2 (list "ByLayer" "Red" "Yellow" "Green" "Cyan" "Blue" "Magenta" "B/W")
			Laylist3 (list"ByLayer")
	)
	(While Lay
		(setq lay1 (list (cdr (assoc 2 Lay)))
			  lay2 (cdr (assoc 62 Lay))
			  lay3 (list (cdr (assoc 6 Lay)))
			  Laylist1 (append Laylist1 lay1)
			  Laylist3 (append Laylist3 lay3)
			  Lay (tblnext "LAYER")
		 )
	  	(if (> lay2 7)(setq  lay2 (list (itoa lay2)) Laylist2 (append Laylist2 lay2)))
	)
	(While LT
		(setq lay3 (list (cdr (assoc 2 LT)))
			  Laylist3 (append Laylist3 lay3)
			  LT (tblnext "LTYPE")
		 )
	)
	(setq 	Laylist1 (RemoveOverlap Laylist1)
			Laylist2 (RemoveOverlap Laylist2)
			Laylist3 (RemoveOverlap Laylist3))
)
;************************
(defun RemoveOverlap (	List2	/	List1	)
	(while List2
		(setq List1 (append List1 (list (car List2))))
		(setq List2 (vl-remove (car List2) List2))
	)
	List1
)
;;;---------Rotate----------------------------

(defun SD8446 ( PointA PointB Ang / XA YA XB YB PointC)

	(setq 	XA2(- (car PointA) (car PointB))
			YA2(- (cadr PointA) (cadr PointB))
	)
	(setq PointC (list (- (* XA2 (cos Ang))(* YA2 (sin Ang))) (+ (* XA2 (sin Ang))(* YA2 (cos Ang)))))
	(setq PointC (mapcar '+ PointC PointB))
	PointC
)
;****************************************************
(defun SD1862 (OldPt Ang / NewCs)
	(setq NewCs (SD8446 '(1 0) '(0 0) Ang))
	(setq NewPt (trans OldPt 0 NewCs))
	(setq NewPt (list (nth 2 NewPt)(nth 0 NewPt)))
	NewPt
)
;**********************
(defun SD3511 (g e)
	(cond
		((= (type e) 'ename) (cdr (assoc g (entget e))))
		((= (type e) 'list) (cdr (assoc g e)))
	)
)
;********************************
(defun *myerror* (msg)
	(setq *error* nil)
	(SD2056)
	(princ "\n Error in SpeedDraftLT")
	(princ)
)
	(princ "\n Command Name: JDIMS")
(princ)