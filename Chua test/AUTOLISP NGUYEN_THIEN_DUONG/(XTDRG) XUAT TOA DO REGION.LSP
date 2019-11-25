(defun C:XTDRG (/ L1 L1A L2 LTSREG X)
;;;;XUAT TOA DO REGION
  (setq LtsReg (ss2list (ssget (list (cons 0 "REGION")))))
  (or *Caochu* (setq *Caochu* 2.0))
  (setq	Caochu
	 (getreal
	   (strcat
	     "\nNh\U+1EADp chi\U+1EC1u cao Text ghi di\U+1EC7n t\U+00EDch <"
	     (rtos *Caochu* 2 2)
	     ">: "
	   )
	 )
  )
  (if (not Caochu)
    (setq Caochu *Caochu*)
    (setq *Caochu* Caochu)
  )
  (setq l2 (list))
  (foreach OneRg LtsReg
    (setq l1 (list))
    (setq l1a (list))
    (setq l1a (CVREG2PL OneRg Caochu))
    (setq l1
	   (append
	     (list
	       (list (strcat "Vung thu "
			     (rtos (+ (vl-position OneRg LtsReg) 1) 2 0)
			     " co "
			     (rtos (length l1a) 2 0)
			     " dinh, do la: "
		     )
	       )
	     )
	     (mapcar
	       '(lambda (x) (list (rtos (car x) 2 3) (rtos (cadr x) 2 3)))
	       l1a
	     )
	   )
    )
    (setq l2 (append l2 l1))
  )
  (if l2
    (progn
      (if (vlax-get-or-create-object "Excel.Application")
	(WriteToExcel l2)
	(WriteToCSV l2)
      )
    )
  )
  (princ)
)

(defun ss2list (ssSelect /)
  (setq	Lts (vl-remove-if
	      'listp
	      (mapcar '(lambda (x) (cadr x)) (ssnamex ssSelect))
	    )
  )
  Lts
)


(defun CVREG2PL	(ename cchu / DTICH OBJPLINE OLD_CMDECHO OLMODE PNTTAM)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (setq old_cmdecho (getvar 'cmdecho))
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq old_cmdecho (getvar 'cmdecho))
  (setvar "OSMODE" 0)
  (setq Olmode (getvar "OSMODE"))
  (setvar 'cmdecho 0)
  (if (= (cdr (assoc 0 (entget ename))) "REGION")
    (progn
      (command "_.undo" "begin")
      (command "_.explode" ename)
      (command "_.pedit" "_m" (ssget "_p") "" "_y" "_j" 0.0 "")
      (setq ObjPline (entlast))
      (setq Dtich (Area ObjPline))
      (setq PntTam (_cen ObjPline))
      (wtxt (rtos Dtich 2 3) PntTam cchu 0 "L" 2)
      (setq LtsPnt_Pline
	     (TD:FilterDeldup
	       (acet-geom-vertex-list ObjPline)
	       1e-8
	     )
      )
      (command "_.undo" "_end")
      (setvar "OSMODE" Olmode)
      (setvar 'cmdecho old_cmdecho)
    )
  )
  LtsPnt_Pline
)

(defun TD:FilterDeldup (l fz)
  (if l
    (cons (car l)
	  (TD:FilterDeldup
	    (vl-remove-if '(lambda (x) (equal x (car l) fz)) (cdr l))
	    fz
	  )
    )
  )
)





(defun _cen (v / p1 p2 p u)
  (vla-getboundingbox
    (setq v (vlax-ename->vla-object v))
    'p1
    'p2
  )
  (setq	p (mapcar '*
		  (mapcar '+
			  (vlax-safearray->list p1)
			  (vlax-safearray->list p2)
		  )
		  '(0.5 0.5 0.5)
	  )
  )
  (setq	u (entmakex (list '(0 . "LINE")
			  (cons 10 p)
			  (cons 11 (polar p (/ pi 2) 1))
		    )
	  )
  )
  (setq p (vlax-invoke v 'IntersectWith (vlax-ename->vla-object u) 2))
  (entdel u)
  (list (car p) (/ (+ (cadr p) (nth 4 p)) 2) (caddr p))
)

(defun Area (ent)
  (setvar "hpgaptol" 0.1)
  (vla-get-area (vlax-ename->vla-object ent))
)


(defun wtxt (string Point Height Ang justify Color / Lst)
  (setq	Lst	(list '(0 . "TEXT")
		      (cons 10 point)
		      (cons 40 Height)
		      (cons 1 string)
		      (cons 62 Color)
		      (if Ang
			(cons 50 Ang)
		      )
		      (cons 7
			    (if	Style
			      Style
			      (getvar "Textstyle")
			    )
		      )
		)
	justify	(strcase justify)
  )
  (cond	((= justify "C")
	 (setq Lst (append Lst (list (cons 72 1) (cons 11 point))))
	)
	((= justify "L")
	 (setq
	   Lst (append Lst (list (cons 72 0) (cons 73 0) (cons 10 point)))
	 )
	)
	((= justify "R")
	 (setq Lst (append Lst (list (cons 72 2) (cons 11 point))))
	)
	((= justify "M")
	 (setq Lst (append Lst (list (cons 72 4) (cons 11 point))))
	)
	((= justify "TL")
	 (setq
	   Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 3)))
	 )
	)
	((= justify "TC")
	 (setq
	   Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 3)))
	 )
	)
	((= justify "TR")
	 (setq
	   Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 3)))
	 )
	)
	((= justify "ML")
	 (setq
	   Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 2)))
	 )
	)
	((= justify "MC")
	 (setq
	   Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 2)))
	 )
	)
	((= justify "MR")
	 (setq
	   Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 2)))
	 )
	)
	((= justify "BL")
	 (setq
	   Lst (append Lst (list (cons 72 0) (cons 11 point) (cons 73 1)))
	 )
	)
	((= justify "BC")
	 (setq
	   Lst (append Lst (list (cons 72 1) (cons 11 point) (cons 73 1)))
	 )
	)
	((= justify "BR")
	 (setq
	   Lst (append Lst (list (cons 72 2) (cons 11 point) (cons 73 1)))
	 )
	)
  )
  (entmake Lst)
)


(defun WriteToExcel (lst_data / col row x xlApp xlCells)
  (setq	xlApp	(vlax-get-or-create-object "Excel.Application")
	xlCells	(vlax-get-property
		  (vlax-get-property
		    (vlax-get-property
		      (vlax-invoke-method
			(vlax-get-property xlApp "Workbooks")
			"Add"
		      )
		      "Sheets"
		    )
		    "Item"
		    1
		  )
		  "Cells"
		)
  )
  (setq row 1)
  (foreach pt lst_data
    (setq col 1)
    (foreach coor pt
      (vlax-put-property xlCells 'Item row col coor)
      (setq col (1+ col))
    )
    (setq row (1+ row))
  )
  (vla-put-visible xlApp :vlax-true)
  (mapcar
    (function (lambda (x)
		(vl-catch-all-apply
		  (function (lambda ()
			      (if x
				(vlax-release-object x)
			      )
			    )
		  )
		)
	      )
    )
    (list xlCells xlApp)
  )
  (gc)
  (gc)
)

(defun WriteToCSV (lst_data / fl)
  (if (setq fl (getfiled "Output File" "" "csv" 1))
    (if	(setq fl (open fl "w"))
      (progn
	(foreach pt lst_data
	  (write-line
	    (strcat (rtos (car pt))
		    ","
		    (rtos (cadr pt))
		    ","
		    (rtos (caddr pt))
	    )
	    fl
	  )
	)
	(close fl)
      )
    )
  )
  (princ)
)
