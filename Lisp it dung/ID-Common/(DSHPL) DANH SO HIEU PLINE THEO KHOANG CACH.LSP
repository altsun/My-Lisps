(defun c:DSHPL( / CDPLINE I KCL N OLMODE PNT1 SS)
(vl-load-com)
(setvar "CMDECHO" 0)
(setq olmode (getvar "OSMODE"))
(setvar "OSMODE" 0)
(_layer2 "TEXT_XOA" 3)
(setq ss (ssget  (list (cons 0 "*POLYLINE,LWPOLYLINE"))))
(setq KCL (getdist "\nNh\U+1EADp kho\U+1EA3ng c\U+00E1ch c\U+1EA7n ghi s\U+1ED1 hi\U+1EC7u:  "))
(or *h* (setq *h* 1))
(setq h (getreal (strcat "\nNh\U+1EADp chi\U+1EC1u cao Text cao \U+0111\U+1ED9 <"
		  (rtos *h* 2 3)
		 ">: "
	  )
 )
)
(if (not h) (setq h *h*) (setq *h* h))
  
(foreach en (acet-ss-to-list ss)
  	(progn
		(setq CDPline (Length1  en))
		(setq n (fix (/ CDPline KCL)))
		(setq i 0)
		(while (<= i n)
			(setq pnt1 (vlax-curve-getPointAtDist en (* i KCL)))
			(MakeText pnt1 (rtos (+ i 1) 2 0) h 0 "L" "TEXT_XOA")
		  	(setq i (1+ i))
		)
	  )
)
(setvar "OSMODE" olmode)
(princ)
)
(defun Length1 (e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
  
(defun MakeText (point string Height Ang justify Layer / Lst); Ang: Radial
(setq Lst (list '(0 . "TEXT")
								(cons 10 point)
								(cons 40 Height)
								(cons 8 Layer)
								(cons 1 string)
								(if Ang (cons 50 Ang))
								
								
		)
	justify (strcase justify)
)
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
(entmakex Lst)
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

(defun C:XTPL( / obj ss EnameT LtsEnameText Pnt PVG  );;;Xoa Text tren Pline
(Alert "\nCh\U+1ECDn polyline")
(setq obj (vlax-ename->vla-object (car (entsel "\nChon Polyline:"))))
(Alert "\nQu\U+00E9t ch\U+1ECDn Text")
(setq ss (ssget (list (cons 0 "TEXT"))))
(setq LtsEnameText (acet-ss-to-list ss))
(foreach EnameT LtsEnameText
	(setq Pnt (TD:Text-Base EnameT))
	(setq PVG (vlax-curve-getClosestPointTo obj Pnt ))
  	(if (and (equal (car Pnt) (car PVG) 0.00000001) (equal (cadr Pnt) (cadr PVG) 0.00000001))
	    (entdel EnameT)
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




(defun C:CCPL( / olmode ss KCL h en CDPline n i Pnt1 chieucao);;;CHEN COC PLINE
(vl-load-com)
(setvar "CMDECHO" 0)
(setq CDaiC 5)
(or *KCC* (setq *KCC* 20))
(setq KCC (getreal (strcat "\nNh\U+1EADp kho\U+1EA3ng c\U+00E1ch gi\U+1EEFa c\U+00E1c c\U+1ECDc <"
                            (rtos *KCC* 2 2)
                           ">: "
                   )
          )
)
(if (not KCC) (setq KCC *KCC*) (setq *KCC* KCC))
(setq ss (acet-ss-to-list (ssget  (list (cons 0 "*POLYLINE,LWPOLYLINE")))))
(mapcar '(lambda(x) (PS1PL x CDaiC KCC)) ss)
(princ)
)


(defun PS1PL (ObjTuyen  CdaiCoc KCC1   / Cdai VlaObj n d Pt   )
(setq Cdai (Length1 ObjTuyen))
(setq VlaObj (vlax-ename->vla-object ObjTuyen))
(setq n (/ Cdai KCC1))
(setq d 0)
(while (< d (vla-get-length VlaObj))
	(setq Pt (vlax-curve-getPointAtDist ObjTuyen d))
	(setq d (+ d (/ (vla-get-length VlaObj) n)))
	(CPS ObjTuyen Pt CdaiCoc)
)
)

(defun CPS (Curve Pt CdaiCoc / ang );;;;COC PHAT SINH
(MakeLayer_ "LINE_COC" 3)
(progn
	(setq Pt (vlax-curve-getClosestPointTo Curve (trans pt 1 0)) ;;;;TIM DIEM GAN NHAT TREN curve TU DIEM PICK
	     ang (angle '(0 0) (Vlax-curve-getfirstderiv Curve (vlax-curve-getParamAtPoint Curve pt)));;;;TINH GOC TAI 1 DIEM TREN DUONG BAT KY
	                                                                                              ;;;;HAM Vlax-curve-getfirstderiv LA DAO HAM BAC 1 CUA curve
	)
	(entmake (list (cons 0 "LINE")(cons 8 "LINE_COC") (cons 10  (polar pt (+ ang (/ pi 2) ) CdaiCoc)) (cons 11 (polar pt (- ang (/ pi 2) ) CdaiCoc))))
)
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