(vl-load-com)
;;;;(prompt "\nXDTPL")
(defun c:XDTPL (/  ssPline   );;;XOA DIEM TRUNG PLINE
(setvar "CMDECHO" 0)
(or *KCXOA1* (setq *KCXOA1* 1))
(setq KCXOA1 (getreal (strcat "\nNh\U+1EADp kho\U+1EA3ng c\U+00E1ch gi\U+1EEFa hai \U+0111\U+1EC9nh Polyline nh\U+1ECF h\U+01A1n s\U+1EBD b\U+1ECB x\U+00F3a m\U+1ED9t trong \U+0111\U+1EC9nh <"
			  (rtos *KCXOA1* 2 2)
			 ">: "
		  )
	 )
)
(if (not KCXOA1) (setq KCXOA1 *KCXOA1*) (setq *KCXOA1* KCXOA1))

(Prompt "\nQu\U+00E9t ch\U+1ECDn c\U+00E1c Polyline: ")
(setq ssPline (acet-ss-to-list (ssget (list (cons 0 "POLYLINE,LWPOLYLINE")))))
(mapcar '(lambda(x)(XDT1PL x KCXOA1)) ssPline)
(princ)
)

(defun XDT1PL (ObjTuyen KCXOA /  LtsVer  LtsVerNew );;;XOA DIEM TRUNG PLINE
(setvar "CMDECHO" 0)
(setq VlaObj (vlax-ename->vla-object ObjTuyen))
(setq LtsVer (TD:Filter1 (acet-geom-vertex-list ObjTuyen) KCXOA))
(setq LtsVerNew (vl-sort LtsVer '(lambda(x y) (< (vlax-curve-getdistAtpoint VlaObj x) (vlax-curve-getdistAtpoint VlaObj y)))))
(EntmakePolyline LtsVerNew)
(entdel ObjTuyen)
(princ)
)



(defun TD:Filter1 ( l fz )
    (if l
      (cons (car l)
        (TD:Filter1
          (vl-remove-if '(lambda ( x ) (equal (list (car x) (cadr x)) (list (car (car l)) (cadr (car l)))  fz)) (cdr l)) fz
        )
      )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun EntmakePolyline (ptlst / )
(setq ptlst (apply 'append
              (mapcar
                (function
                  (lambda (x)
                    (list (car x) (cadr x)))
		 )
		 ptlst
	       )
	      )
 )
(vla-AddLightWeightPolyline
	(vla-get-ModelSpace
		(vla-get-ActiveDocument
			(vlax-get-acad-object)
		)
	)
	(vlax-make-variant
		(vlax-safearray-fill
			(vlax-make-safearray
				vlax-VBDouble (cons 0 (1- (length ptlst)))
		        )
			ptlst
		)
	)
)
)
