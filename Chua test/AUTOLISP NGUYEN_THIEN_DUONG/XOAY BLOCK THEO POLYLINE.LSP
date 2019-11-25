(VL-LOAD-COM)
(defun c:XBL ( / Olmode VlaObj ssBlk EnameBlk Pnt PntVG D1 D2 P1 P2 ang Ent1 Ent2 TenBlock);;;XOAY BLOCK THEO PLINE
(setvar "CMDECHO" 0)
(setq ent1  (entsel "\nCh\U+1ECDn Polyline \U+0111\U+1EC3 xoay theo h\U+01B0\U+1EDBng: "))
;;;(while
;;;	(or (null ent1) (/= "LWPOLYLINE"  (cdr (assoc 0 (entget (car ent1))))) (/= "POLYLINE"  (cdr (assoc 0 (entget (car ent1))))))
;;;	(setq ent1 (entsel "\n\U+0110\U+00E2y kh\U+00F4ng ph\U+1EA3i POLYLINE! B\U+1EA1n ch\U+1ECDn l\U+1EA1i! "))
;;;)
(if ent1
  (progn
        (setq VlaObj (vlax-ename->vla-object (car ent1)))
	(setq Ent2 (entsel "\n Chon Block :"))
        (while
		(or (null Ent2) (/= "INSERT"  (cdr (assoc 0 (entget (car Ent2))))))
		(setq Ent2 (entsel "\n\U+0110\U+00E2y kh\U+00F4ng ph\U+1EA3i BLOCK! B\U+1EA1n ch\U+1ECDn l\U+1EA1i! "))
	)
    	(if Ent2
	  (progn
	  	(setq EnameBlk (car Ent2))
		(setq TenBlock (cdr (assoc 2 (entget EnameBlk))))
		(setq Pnt (vlax-get (vlax-ename->vla-object EnameBlk) 'InsertionPoint))
		(setq PntVG (vlax-curve-getClosestPointTo VlaObj Pnt))
		(setq P1 (vlax-curve-getpointatparam VlaObj  (fix (vlax-curve-getParamAtPoint VlaObj PntVG))))
		(setq P2 (vlax-curve-getpointatparam VlaObj  (+ (fix (vlax-curve-getParamAtPoint VlaObj PntVG)) 1)))
		(setq ang (angle P1 P2))
		(entmod (subst (cons 50 ang) (assoc 50 (entget EnameBlk)) (entget EnameBlk) ))
		(entupd EnameBlk)
	   )
	 )
   )
 )
(princ)
 )

(defun c:XNBL ( / Olmode VlaObj ssBlk EnameBlk Pnt PntVG D1 D2 P1 P2 ang Ent1 Ent2 TenBlock);;;XOAY BLOCK THEO PLINE
(setvar "CMDECHO" 0)
(setq ent1  (entsel "\nCh\U+1ECDn Polyline \U+0111\U+1EC3 xoay theo h\U+01B0\U+1EDBng: "))
;;;(while
;;;	(or (null ent1) (/= "LWPOLYLINE"  (cdr (assoc 0 (entget (car ent1))))) (/= "POLYLINE"  (cdr (assoc 0 (entget (car ent1))))))
;;;	(setq ent1 (entsel "\n\U+0110\U+00E2y kh\U+00F4ng ph\U+1EA3i POLYLINE! B\U+1EA1n ch\U+1ECDn l\U+1EA1i! "))
;;;)
(if ent1
  (While
        (setq VlaObj (vlax-ename->vla-object (car ent1)))
	(setq Ent2 (entsel "\n Chon Block :"))
        (while
		(or (null Ent2) (/= "INSERT"  (cdr (assoc 0 (entget (car Ent2))))))
		(setq Ent2 (entsel "\n\U+0110\U+00E2y kh\U+00F4ng ph\U+1EA3i BLOCK! B\U+1EA1n ch\U+1ECDn l\U+1EA1i! "))
	)
    	(if Ent2
	  (progn
	  	(setq EnameBlk (car Ent2))
		(setq TenBlock (cdr (assoc 2 (entget EnameBlk))))
		(setq Pnt (vlax-get (vlax-ename->vla-object EnameBlk) 'InsertionPoint))
		(setq PntVG (vlax-curve-getClosestPointTo VlaObj Pnt))
		(setq P1 (vlax-curve-getpointatparam VlaObj  (fix (vlax-curve-getParamAtPoint VlaObj PntVG))))
		(setq P2 (vlax-curve-getpointatparam VlaObj  (+ (fix (vlax-curve-getParamAtPoint VlaObj PntVG)) 1)))
		(setq ang (angle P1 P2))
		(entmod (subst (cons 50 ang) (assoc 50 (entget EnameBlk)) (entget EnameBlk) ))
		(entupd EnameBlk)
	   )
	 )
   )
 )
(princ)
 )



(defun c:XBLPL ( / Olmode VlaObj ssBlk EnameBlk Pnt PntVG D1 D2 P1 P2 ang Dem TenBlock);;;XOAY BLOCK THEO PLINE
(setvar "CMDECHO" 0)
(setq Dem 0)
(setq VlaObj (vlax-ename->vla-object (car (entsel "\nCh\U+1ECDn Polyline \U+0111\U+1EC3 xoay theo h\U+01B0\U+1EDBng: "))))
(setq TenBlock (getstring "\n Nh\U+1EADp t\U+00EAn Block / Enter \U+0111\U+1EC3 ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng:  "))
(if (= TenBlock "") (setq TenBlock (cdr (assoc 2 (entget (car (entsel "\n Chon Block :")))))))
(Alert (strcat "\nQu\U+00E9t ch\U+1ECDn Block c\U+00F3 t\U+00EAn l\U+00E0: "  TenBlock  " \U+0111\U+1EC3 xoay"))
(setq ssBlk (acet-ss-to-list (ssget (list (cons 0 "INSERT")(cons 2 TenBlock)))))
(foreach EnameBlk ssBlk
	(setq Pnt (vlax-get (vlax-ename->vla-object EnameBlk) 'InsertionPoint))
  	(setq PntVG (vlax-curve-getClosestPointTo VlaObj Pnt))
	(setq P1 (vlax-curve-getpointatparam VlaObj  (fix (vlax-curve-getParamAtPoint VlaObj PntVG))))
  	(setq P2 (vlax-curve-getpointatparam VlaObj  (+ (fix (vlax-curve-getParamAtPoint VlaObj PntVG)) 1)))
	(setq ang (angle P1 P2))
	(entmod (subst (cons 50 ang) (assoc 50 (entget EnameBlk)) (entget EnameBlk) ))
  	(entupd EnameBlk)
  	(setq Dem (1+ Dem))
)
(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 " (rtos Dem 2 0) " \U+0111\U+01B0\U+1EE3c xoay"))
(princ)
 )


(defun c:TBL0( /  ent1   ssT1  TenBlock  Layer   Dem  loop Lt4 Lt5  );;;TIM BLOCK GOC XOAY BANG 0
(setvar "CMDECHO" 0)
(defun *error* ( msg )
		(if Olmode (setvar 'osmode Olmode))
		(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
		    (princ (strcat "\nError: " msg))
		)
		(princ)
)

(setq Olmode (getvar "osmode"));;;LUU CHE DO BAT DIEM OSNAP
(command "undo" "begin")
(or *BKR1* (setq *BKR1* 5.0))
(setq BKR1 (getreal (strcat "\nNh\U+1EADp b\U+00E1n k\U+00EDnh v\U+00F2ng tr\U+00F2n \U+0111\U+00E1nh d\U+1EA5u <"
	                      (rtos *BKR1* 2 2)
	                       ">: "
                      )
            )
)
(if (not BKR1) (setq BKR1 *BKR1*) (setq *BKR1* BKR1))

(setq Loop nil)
(setq Dem 0)
(setq ent1  (entsel "\nCh\U+1ECDn BLOCK m\U+1EABu \U+0111\U+1EC3 l\U+1ECDc:  "))
(while
	(or (null ent1) (/= "INSERT"  (cdr (assoc 0 (entget (car ent1))))))
        (progn
		(setq ent1 (entsel "\n\U+0110\U+00E2y kh\U+00F4ng ph\U+1EA3i BLOCK B\U+1EA1n ch\U+1ECDn l\U+1EA1i! "))
	  	(setq Loop T)
	)	
)
(if ent1
    (progn
      	(Alert (strcat "\nT\U+1EADp BLOCK s\U+1EBD \U+0111\U+01B0\U+1EE3c ch\U+1ECDn c\U+00F3 t\U+00EAn l\U+00E0: " (cdr (assoc 2 (entget (car ent1))))
		       "\n\U+0110\U+1EC3 ti\U+1EBFp t\U+1EE5c, m\U+1EDDi b\U+1EA1n qu\U+00E9t ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng"
	       )
	)
        (MakeLayer_ "CIRCLE_G0" 3)
	(setq ssT1 (acet-ss-to-list (ssget (list (cons 0 "INSERT") (cons 50 0.0) (cons 2 (cdr (assoc 2 (entget (car ent1)))))))))
        (if (not ssT1) (setq ssT1 (acet-ss-to-list (ssget"_X" (list (cons 0 "INSERT")(cons 50 0.0) (cons 2 (cdr (assoc 2 (entget (car ent1))))))))))
        (setq Dem (length ssT1))
	(mapcar '(lambda (x)  (MakeCircle (vlax-get (vlax-ename->vla-object x) 'InsertionPoint) BKR1 "CIRCLE_G0")) ssT1)
     )
)

(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 " (rtos Dem 2 0) " \U+0111\U+01B0\U+1EE3c \U+0111\U+00E1nh d\U+1EA5u"))
(setvar "OSMODE" Olmode)
(princ)
)


(defun c:XBL0( /  ent1   ssT1  TenBlock  Layer   Dem  loop Lt4 Lt5  );;;XOA BLOCK GOC XOAY BANG 0
(setvar "CMDECHO" 0)
(defun *error* ( msg )
		(if Olmode (setvar 'osmode Olmode))
		(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
		    (princ (strcat "\nError: " msg))
		)
		(princ)
)

(setq Olmode (getvar "osmode"));;;LUU CHE DO BAT DIEM OSNAP
(command "undo" "begin")
(setq Loop nil)
(setq Dem 0)
(setq ent1  (entsel "\nCh\U+1ECDn BLOCK m\U+1EABu \U+0111\U+1EC3 l\U+1ECDc:  "))
(while
	(or (null ent1) (/= "INSERT"  (cdr (assoc 0 (entget (car ent1))))))
        (progn
		(setq ent1 (entsel "\n\U+0110\U+00E2y kh\U+00F4ng ph\U+1EA3i BLOCK B\U+1EA1n ch\U+1ECDn l\U+1EA1i! "))
	  	(setq Loop T)
	)	
)
(if ent1
    (progn
      	(Alert (strcat "\nT\U+1EADp BLOCK s\U+1EBD \U+0111\U+01B0\U+1EE3c ch\U+1ECDn c\U+00F3 t\U+00EAn l\U+00E0: " (cdr (assoc 2 (entget (car ent1))))
		       "\n\U+0110\U+1EC3 ti\U+1EBFp t\U+1EE5c, m\U+1EDDi b\U+1EA1n qu\U+00E9t ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng"
	       )
	)
        
	(setq ssT1 (acet-ss-to-list (ssget (list (cons 0 "INSERT") (cons 50 0.0) (cons 2 (cdr (assoc 2 (entget (car ent1)))))))))
        (if (not ssT1) (setq ssT1 (acet-ss-to-list (ssget"_X" (list (cons 0 "INSERT")(cons 50 0.0) (cons 2 (cdr (assoc 2 (entget (car ent1))))))))))
        (setq Dem (length ssT1))
	(mapcar '(lambda (x) (entdel x)) ssT1)
     )
)

(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3: " (rtos Dem 2 0) " Block c\U+00F3 g\U+00F3c xoay b?ng 0 \U+0111\U+01B0\U+1EE3c x\U+00F3a"  ))
(setvar "OSMODE" Olmode)
(princ)
)


(defun MakeCircle (Pnt R Layer )
	(entmakex (list (cons 0 "CIRCLE")
			(cons 8 (if Layer Layer (getvar "Clayer")))
			(cons 10 Pnt)
			(cons 40 R)
		  )
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


