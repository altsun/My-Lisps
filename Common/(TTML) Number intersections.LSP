(vl-load-com)
;;;(Alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh t\U+1EA1o Text t\U+1EA1i \U+0111\U+1EC9nh Line v\U+00E0 Pline"
;;;               "\n"
;;;               "\nD\U+00F9ng l\U+1EC7nh TTML \U+0111\U+1EC3 \U+0111\U+00E1nh s\U+1ED1 hi\U+1EC7u \U+0111\U+1EC9nh"
;;;               "\n"
;;;               "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;               "\n"
;;;               "\nMobile: 0972.0168.25"
;;;               "\n"
;;;        )
;;;)
;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh t\U+1EA1o Text t\U+1EA1i \U+0111\U+1EC9nh Line v\U+00E0 Pline"
;;;               "\n"
;;;               "\nD\U+00F9ng l\U+1EC7nh TTML \U+0111\U+1EC3 \U+0111\U+00E1nh s\U+1ED1 hi\U+1EC7u \U+0111\U+1EC9nh"
;;;               "\n"
;;;               "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;               "\n"
;;;               "\nMobile: 0972.0168.25"
;;;               "\n"
;;;        )
;;;)
(defun c:TTML ( / Lts_Line Lts-Pnt-by-Line  Lts_new1 Lts-Pnt-Sort ss_list-ID-Emane Lts_EdgeTopo Lts_Sort_ID_TOPOEDGE
               Dem Lts_ID_TOPOEDGE  LtsP1 LtsP2 LtsPoint PntTT LstT1 LstT2  LtsDong3   Lts1Dong            );;TAO TEXT MAT LUOI
(setvar "CMDECHO" 0)

;;;;;;QUET CHON CA LINE VA PLINE
;;;(Alert "\nQu\U+00E9t ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng LINE, POLYLINE")
(setq ss (ssget '((0 . "*POLYLINE,LINE"))))

(or *BKLOC* (setq *BKLOC* 0.01))
(setq BKLOC (getdist (strcat "\nNh\U+1EADp b\U+00E1n k\U+00EDnh l\U+1ECDc 2 \U+0111i\U+1EC3m g\U+1EA7n nhau \U+0111\U+1EC3 lo\U+1EA1i b\U+1ECF <"
		  (rtos *BKLOC* 2 2)
		 "> :"
	  )
 )
)
(if (not BKLOC) (setq BKLOC *BKLOC*) (setq *BKLOC* BKLOC))
  
(or *chieucao* (setq *chieucao* 1))
(setq chieucao (getdist (strcat "\nNh\U+1EADp chi\U+1EC1u cao ch\U+1EEF <"
		  (rtos *chieucao* 2 2)
		 "> :"
	  )
 )
)
(if (not chieucao) (setq chieucao *chieucao*) (setq *chieucao* chieucao))

(_layer2 "Text_ML" 2)
;;;;;LOC RA TAP HOP DOI TUONG LINE;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Lts_EnameLine (vl-remove nil (mapcar '(lambda(x) (if (= (acet-dxf 0 (entget x)) "LINE") x nil)) (acet-ss-to-list ss))))
;;;;;LOC RA TAP HOP DOI TUONG PLINE
(setq Lts_EnamePLine (vl-remove nil (mapcar '(lambda(x) (if (= (acet-dxf 0 (entget x)) "LWPOLYLINE") x nil)) (acet-ss-to-list ss))))
;;;;CONVERT LINE TO POINT;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq LtsP1 (ConverLine2Point Lts_EnameLine))
;;;;CONVERT PLINE TO POINT;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq LtsP2 (ConverPline2Point Lts_EnamePLine))
;;;;;;;;;LAY TOA DO DIEM GIAO;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq LtsIntersec (LM:IntersectionsinList (acet-ss-to-list ss) ))
;;;;;;;;;GOP TAT CA CAC GIAO DIEM VA TOA DO DIEM DAU MUT;;;;;;;;;;;;;
(setq LtsPoint (append LtsP1 LtsP2 LtsIntersec ))
;;;;;;;;;;;;;LOC DIEM TRUNG;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Lts_EnamePoint (TD:Remove-Point-duplicates LtsPoint BKLOC))
;;;	;;;;;;;;;;SAP XEP LAI TAP DIEM;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Lts-EnamePnt-Sort (SortXY Lts_EnamePoint))
;;;	;;;;;;;;;;;;;DANH SO HIEU POINT THEO THU TU TANG DAN
(setq ss_list-ID-Emane (Get-ID-to-Point Lts-EnamePnt-Sort))
(foreach Lts ss_list-ID-Emane
  	(setq ID (car Lts))
	(setq PntTT (cadr Lts))
  	(MakeText  (rtos ID  2 0) PntTT chieucao 0 "L" "Text_ML")
)
(setq LstT1 (mapcar '(lambda (x) (list (car x) (car (cadr x)) (cadr (cadr x)) (caddr (cadr x)))) ss_list-ID-Emane))
(setq Dem (length ss_list-ID-Emane))
(setq Lts1Dong (list (list "TENDIEM" "TD_X" "TD_Y" "TD_Z")))
(setq LtsDong3 (vl-sort LstT1 '(lambda(x y) (<  (nth 0 x)  (nth 0 y)))))
(setq LtsDong3 (append Lts1Dong LtsDong3  ))
(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 " (rtos Dem 2 0) " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"))
(if (vlax-get-or-create-object "Excel.Application")
			(WriteToExcel LtsDong3)
			(WriteToCSV LtsDong3)
)
(princ)
)




(defun ConverLine2Point (Lts_EnameLine / Lst1  )
(setq Lts1 (list))
(setq Lts2 (list))
(foreach EnameLine Lts_EnameLine
  	(setq Lts1 (list (cdr (assoc 10 (entget  EnameLine))) (cdr (assoc 11 (entget  EnameLine)))))
  	(setq Lts2 (append Lts2  Lts1))
)
Lts2
)
;;;;CHUYEN PLINE THANH CAC DIEM (Lts_EnamePLine LA DANG DANH SACH ENAME)
(defun ConverPline2Point (Lts_EnamePLine / Dsdinh EnamePLine )
(setq Dsdinh (list))
(setq LtsPl (list))
(foreach EnamePLine Lts_EnamePLine
  	(setq Dsdinh (acet-geom-vertex-list EnamePLine))
  	(setq LtsPl (append LtsPl  Dsdinh))
)
LtsPl
)





(defun LM:Intersections ( obj1 obj2 mode / l r ) 
    (setq l (vlax-invoke obj1 'intersectwith obj2 mode))
    (repeat (/ (length l) 3)
        (setq r (cons (list (car l) (cadr l) (caddr l)) r)
              l (cdddr l)
        )
    )
    (reverse r)
)

(defun LM:IntersectionsinList ( lst / a l )
    (while (setq a (car lst))
        (foreach b (setq lst (cdr lst))
            (setq l (cons (LM:Intersections (vlax-ename->vla-object a) (vlax-ename->vla-object b) acextendnone) l))
        )
    )
    (apply 'append (reverse l))
)
;;;;;;;;;;;;;;;;THANHDUAN;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;LAY RA DANH SACH DOI TUONG TRUNG VOI KHOANG CACH NHAP VAO;;;;;;;;;;;;;;;;
(defun LM:_UniqueFuzz ( l fz )
    (if l
      (cons (car l)
        (LM:_UniqueFuzz
          (vl-remove-if '(lambda ( x ) (equal x (car l)  fz)) (cdr l)) fz
        )
      )
    )
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


;;HAM LAY RA CAC PHAN TU GIONG NHAU TU 2 DANH SACH (CON - TO) (LEN L1 < LEN L2)
;;;;(LM:RemoveOnce '(2 4 6) '(1 2 3 4 5) )
(defun LM:RemoveOnce ( l1 l2 )
  (if l1
    (if (equal (car l1) l2)
      (LM:RemoveOnce (cdr l1) l2)
      (cons (car l1) (LM:RemoveOnce (cdr l1) l2))
    )
  )
)
;;;;;;XOA NHUNG DOI TUONG TRUNG NHAU
(defun TD:Remove-Point-duplicates (ss_list BK_LOC /  Lts1 Lts2 )
(vl-load-com)
(setq Lts1  (LM:_UniqueFuzz ss_list BK_LOC))
(setq Lts2 (LM:ListDifference ss_list Lts1))
(setq Lts3 (LM:RemoveOnce Lts1 ss_list))
Lts3
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;SAP XEP THEO X , NEU X BANG NHAU THI SAP XEP THEO Y
(defun SortXY (lst /)
(setq Lts-Sort (vl-sort (vl-sort lst '(lambda(e1 e2) (< (cadr e1) (cadr e2)))) '(lambda(e1 e2) (< (car e1) (car e2)))))
Lts-Sort
)


;;;;;;(GetPntId (getpoint) (acet-ss-to-list (ssget "_X" (list (cons 0 "POINT")))))
;;;;GAN SO HIEU CHO POINT (SAU KHI SAP XEP);;;DAU VAO LA DANG DANH SACH CAC ENAME, DANH SACH TRA VE  LA DANH SACH TUNG PHAN TU (SOHIEU1 ENAME)(SOHIEU2 ENAME)(SOHIEU3 ENAME)(SOHIEU4 ENAME)
(defun Get-ID-to-Point (ss_list / i  ID_Ename )
(setq i 0)
(setq ID_Ename (list))
(setq Lts_ID_Ename (list))
(while (< i (length ss_list))
	(progn
		(setq ID_Ename (list (+ i 1) (nth i ss_list)))
		(setq Lts_ID_Ename (append Lts_ID_Ename (list ID_Ename)))
	)
	(setq i (1+ i))
)
Lts_ID_Ename
)

;;;;;;'==========TIM SO HIEU ID CHO 1 POINT (POINT TO ID) TRONG 1 DANH SACH GOM (SOHIEU ENAME) ====DANH SACH TRA VE DANG ID) =====
(defun Get-Pnt-Id (Pnt ss_list-ID-Emane /  i  )
(setq i 0)
(setq Lts_ID_Ename (list))
(while (< i (length ss_list-ID-Emane))
	(if  (<  (distance Pnt (cadr (nth i ss_list-ID-Emane))) 0.00000001)
	     (setq ID  (+ i 1) )
        )
	(setq i (1+ i))
)
ID
)

;;;;;;TIM TOA DO POINT DUA VAO ID
(defun Get-ID-to-Pnt (ID ss_list_ID-Ename /  i Lts1 )
(setq i 0)
(while (< i (length ss_list_ID-Ename))
	(if  (equal ID (car (nth i ss_list_ID-Ename)))
	     (setq Pnt (cadr (nth i ss_list_ID-Ename)))
        )
	(setq i (1+ i))
)
Pnt
)


(defun GET_ID_DUP (l fz / )
    (if l
      (cons (car l)
        (GET_ID_DUP
          (vl-remove-if '(lambda (x) (and (equal (car (car x)) (car (car (car  l))) fz) (equal (cadr (car x)) (cadr (car (car l))) fz) )) (cdr l)) fz
        )
      )
    )
)


;;;LOC NHUNG TOPOEDGE BI TRUNG ID1 VA ID2 (2 TOPOEDGE LIEN TIEP)

(defun MakeText (string Point Height Ang justify Layer / Lst)
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



(defun WriteToExcel (lst_data / col row x xlApp xlCells)
(setq xlApp (vlax-get-or-create-object "Excel.Application")
	xlCells (vlax-get-property
			(vlax-get-property
		  		(vlax-get-property
			    		(vlax-invoke-method
				   		(vlax-get-property xlApp "Workbooks")
			   						 "Add"
					)
					"Sheets"
				)
	  			"Item" 1
			)
			"Cells"
		)
)
(setq row 1)
(foreach pt lst_data
	(setq col 1)
	(foreach coor pt
  	(vlax-put-property xlCells 'Item row col coor)
  	(setq col (1+ col)))
	(setq row (1+ row))
)
(vla-put-visible xlApp :vlax-true)
(mapcar
	(function (lambda (x)
		  (vl-catch-all-apply (function (lambda ()(if x (vlax-release-object x))))))
        )
        (list xlCells xlApp)
)
(gc)
(gc)
)
 
(defun WriteToCSV (lst_data / fl)
(if (setq fl (getfiled "Output File" "" "csv" 1))
	(if (setq fl (open fl "w"))
		(progn
			(foreach pt lst_data
				(write-line (strcat (rtos (car pt)) "," (rtos (cadr pt)) "," (rtos (caddr pt))) fl)
			)
			(close fl)
		)
	)
)
(princ)
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


