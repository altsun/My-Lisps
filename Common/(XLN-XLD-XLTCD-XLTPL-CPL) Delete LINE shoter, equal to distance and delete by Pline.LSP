;;;(Alert (strcat "\n X\U+00F3a Line c\U+00F3 chi\U+1EC1u d\U+00E0i nh\U+1ECF h\U+01A1n gi\U+00E1 tr\U+1ECB nh\U+1EADp v\U+00E0o: XLN "
;;;	       "\n X\U+00F3a Line c\U+00F3 chi\U+1EC1u d\U+00E0i  b?ng v\U+1EDBi gi\U+00E1 tr\U+1ECB nh\U+1EADp v\U+00E0o: XLTCD"
;;;	       "\n X\U+00F3a Line c\U+00F3 \U+0111\U+1EC9nh n?m tr\U+00EAn Polyline: XLTPL"
;;;	)
;;;)
;;;(Prompt (strcat "\n X\U+00F3a Line c\U+00F3 chi\U+1EC1u d\U+00E0i nh\U+1ECF h\U+01A1n gi\U+00E1 tr\U+1ECB nh\U+1EADp v\U+00E0o: XLN "
;;;	       "\n X\U+00F3a Line c\U+00F3 chi\U+1EC1u d\U+00E0i  b?ng v\U+1EDBi gi\U+00E1 tr\U+1ECB nh\U+1EADp v\U+00E0o: XLTCD"
;;;	       "\n X\U+00F3a Line c\U+00F3 \U+0111\U+1EC9nh n?m tr\U+00EAn Polyline: XLTPL"
;;;	)
;;;)
(defun Length1(e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
;;;--------------------------------------------------------------------
(defun C:XLN( / ss L e );;;Xoa line ngan
(or *KCNGAN* (setq *KCNGAN* 1.0))
(setq KCNGAN (getdist (strcat "\nNh\U+1EADp chi\U+1EC1u d\U+00E0i nh\U+1ECF h\U+01A1n s\U+1EBD b\U+1ECB x\U+00F3a:  <"
		  (rtos *KCNGAN* 2 2)
		 ">:  "
	  )
 )
)
(if (not KCNGAN) (setq KCNGAN *KCNGAN*) (setq *KCNGAN* KCNGAN))
(setq
    ss (ssget  (list (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE")))
    L 0.0
)
(vl-load-com)
(setq n 0)
(foreach e (acet-ss-to-list ss)
    (if (< (length1 e) KCNGAN)
      	(progn
        	(setq n (+ n 1))
        	(entdel e)
	)
    )
)
(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3: " (rtos n 2 0) " c\U+1EA1nh b\U+1ECB x\U+00F3a"))
(princ)
)

(defun C:XLD( / ss L e );;;Xoa line ngan
(or *KCDAI* (setq *KCDAI* 1.0))
(setq KCDAI (getdist (strcat "\nNh\U+1EADp chi\U+1EC1u d\U+00E0i l\U+1EDBn h\U+01A1n s\U+1EBD b\U+1ECB x\U+00F3a:  <"
		  (rtos *KCDAI* 2 2)
		 ">:  "
	  )
 )
)
(if (not KCDAI) (setq KCDAI *KCDAI*) (setq *KCDAI* KCDAI))
(setq
    ss (ssget  (list (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE")))
    L 0.0
)
(vl-load-com)
(setq n 0)
(foreach e (acet-ss-to-list ss)
    (if (> (length1 e) KCDAI)
      	(progn
        	(setq n (+ n 1))
        	(entdel e)
	)
    )
)
(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3: " (rtos n 2 0) " c\U+1EA1nh b\U+1ECB x\U+00F3a"))
(princ)
)


(defun C:XLTCD( / ss L e);;;Xoa line theo chieu dai
(setq KC (getdist "\nNhap chieu dai LINE: "))
(setq
    ss (ssget  (list (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE")))
    L 0.0
)
(vl-load-com)
(foreach e (acet-ss-to-list ss)
    (if (equal (length1 e) KC 0.01)
        (entdel e)
    )
)
(princ)
)



(defun c:XLTPL();;;xoa line tren Pline
(setq obj (vlax-ename->vla-object (car (entsel "\nChon Line hoac Polyline:"))))
(Alert "\nQuet chon Line")
(setq ss (ssget '((0 . "LINE"))))
(setq LtsEnameLine (acet-ss-to-list ss))
(foreach EnameL LtsEnameLine
	(setq P1 (cdr (assoc 10 (entget EnameL))))
  	(setq P2 (cdr (assoc 11 (entget EnameL))))
	(setq PVG1 (vlax-curve-getClosestPointTo obj P1 T))
	(setq PVG2 (vlax-curve-getClosestPointTo obj P2 T))
  	(if (or  (equal P1 PVG1 0.0001)  (equal P2 PVG1 0.0001))
	    (entdel EnameL)
	)
)
(princ)
)



(defun C:CPL( / ss L e);;Chon Pline
(MakeLayer_ "PlVang" 2)
(or *KC* (setq *KC* 10.0))
(setq KC (getreal (strcat "\nNhap chieu dai: <"
			  (rtos *KC* 2 2)
			 "> :"
		  )
	 )
)
(if (not KC) (setq KC *KC*) (setq *KC* KC))

(setq
    ss (ssget  (list (cons 0 "*POLYLINE,LWPOLYLINE,LINE")(cons 62 2 )))
    L 0.0
)
(vl-load-com)
(foreach e (acet-ss-to-list ss)
    (if (> (length1 e) KC)
        (PUTLAYER e "PlVang")
    )
)
(princ)
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

(defun PUTLAYER (ent NameLayer / s)
   (setq s (vlax-ename->vla-object ent) )
   (vla-put-layer s NameLayer )
)