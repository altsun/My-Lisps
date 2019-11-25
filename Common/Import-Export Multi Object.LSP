
(defun replace_str (str)
  (setq	rs (acet-str-replace "," " " str)
	rs (acet-str-replace "	" " " rs)
  )
  (split_space rs)
)
(defun split_space (str)
  (vl-remove-if
    '(lambda (x) (= x ""))
    (acet-str-to-list " " str)
  )
)




(defun c:XTC (/	CODE1 DEM FH FN	I ITEM LTSDONG LTSDONG1	SS TDO TEMP X Y
	      Z)
;;;;;;; XUAT TOPCON
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq ss (ssget (list (cons 0 "TEXT"))))
  (setq	ss (vl-remove-if-not
	     '(lambda (x) (= (type x) 'ENAME))
	     (mapcar 'cadr (ssnamex ss))
	   )
  )
  (setq LtsDong1 (list))
  (setq LtsDong (list))
  (setq i 0)
  (setq Dem 0)
  (setq fn (getfiled "Export XY" "" "txt" 1))
  (if (/= fn nil)
    (progn
      (setq fh (open fn "w"))
      (while (setq item (nth i ss))
	(setq temp (entget item))
	(setq Tdo (TD:Text-Base item))
	(setq Code1 (cdr (assoc 1 temp))
	      X	    (rtos (car Tdo) 2 3)
	      Y	    (rtos (cadr Tdo) 2 3)
	      Z	    (rtos (caddr Tdo) 2 3)
	)
	(write-line (strcat Code1 "," X "," Y "," Z) fh)
	(setq i (+ i 1))
      )
    )
  )
  (alert
    (strcat
      "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
      (rtos (length ss) 2 0)
      " Text \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)


(defun c:XTDBL (/ DEM ENTOBJ I LTSDONG LTSDONG1	OBJ SS SSBL TDO	TENBLOCK
		X Y Z)
;;;;;;;XUAT TOA DO BLOCK
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (Alert
    "Ch\U+1ECDn lo\U+1EA1i \U+0111\U+1ED1i t\U+01B0\U+1EE3ng c\U+1EA7n xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
  )
  (setq	TenBlock
	 (cdr
	   (assoc
	     2
	     (entget
	       (car
		 (entsel
		   "\nCh\U+1ECDn lo\U+1EA1i \U+0111\U+1ED1i t\U+01B0\U+1EE3ng c\U+1EA7n xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9: "
		 )
	       )
	     )
	   )
	 )
  )
  (setq ssBl (ssget (list (cons 0 "INSERT") (cons 2 TenBlock))))
  (setq	ss (vl-remove-if-not
	     '(lambda (x) (= (type x) 'ENAME))
	     (mapcar 'cadr (ssnamex ssBl))
	   )
  )
  (setq LtsDong (list))
  (setq i 0)
  (setq Dem 0)
  (if ssBl
    (while (setq EntObj (nth i ss))
      (setq LtsDong1 (list))
      (setq obj (vlax-ename->vla-object EntObj))
      (setq Tdo (vlax-get obj 'InsertionPoint))
      (setq X (rtos (car Tdo) 2 3))
      (setq Y (rtos (cadr Tdo) 2 3))
      (setq Z (rtos (caddr Tdo) 2 3))
      (setq LtsDong1 (List (rtos (1+ i) 2 0) Y X Z))
      (setq LtsDong (append LtsDong (list LtsDong1)))
      (setq i (+ i 1))
    )
  )
  (setq Dem (length LtsDong))
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (if (/= Dem 0)
    (progn
      (if (vlax-get-or-create-object "Excel.Application")
	(WriteToExcel LtsDong)
	(WriteToCSV LtsDong)
      )
    )
  )
  (princ)
)

(defun c:PBL (/ DATA DEM F LINE OLMDE OLMODE PNT S1 TEN TENBLOCK X Y)
;;;;;;;;;;;;;;;;;;;;PHUN BLOCK
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Olmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq Dem 0)
  (setq	TenBlock
	 (getstring
	   "\n Nh\U+1EADp t\U+00EAn Block / Enter \U+0111\U+1EC3 ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng:  "
	 )
  )
  (if (= TenBlock "")
    (setq TenBlock
	   (cdr (assoc 2 (entget (car (entsel "\n Chon Block :")))))
    )
  )

  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (setq s1 ((lambda (sec) (+ (* 86400 (- sec (fix sec))) 60))
		 (getvar "DATE")
	       )
      )
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq X (atof (nth 1 data)))
		(setq Y (atof (nth 2 data)))
		(setq Pnt (list Y X))
		(MakeInsertBL TenBlock Pnt 1 0)
	      )
	    )
	 )
	 (setq Dem (1+ Dem))
      )
    )
  )
  (setvar "OSMODE" olmde)
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c phun t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)


(defun c:XNDT (/ CODE1 DEM I ITEM LTSDONG LTSDONG1 SS TDO TEMP X Y Z)
;;;;;;;Xuat noi dung Text
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq ss (ssget (list (cons 0 "TEXT"))))
  (setq	ss (vl-remove-if-not
	     '(lambda (x) (= (type x) 'ENAME))
	     (mapcar 'cadr (ssnamex ss))
	   )
  )
  (setq LtsDong1 (list))
  (setq LtsDong (list))
  (setq i 0)
  (setq Dem 0)
  (while (setq item (nth i ss))
    (setq temp (entget item))
    (setq Tdo (TD:Text-Base item))
    (setq Code1	(cdr (assoc 1 temp))
	  X	(rtos (car Tdo) 2 3)
	  Y	(rtos (cadr Tdo) 2 3)
	  Z	(rtos (caddr Tdo) 2 3)
    )
    (setq LtsDong1 (List (rtos (+ i 1) 2 0) Y X Z Code1))
    (setq LtsDong (append LtsDong (list LtsDong1)))
    (setq i (+ i 1))
  )
  (setq Dem (length LtsDong))
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (if (/= Dem 0)
    (progn

      (if (vlax-get-or-create-object "Excel.Application")
	(WriteToExcel LtsDong)
	(WriteToCSV LtsDong)
      )
    )
  )
  (princ)
)

(defun c:XDT (/ DEM DONG DONG1 E PNT SS TEMP X Y Z)
;;;;;Xuat duong tron
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq ss (ssget (list (cons 0 "CIRCLE"))))
  (setq e 1)
  (setq dong1 (list))
  (setq dong (list))
  (setq Dem 0)
  (foreach item	(acet-ss-to-list ss)
    (setq temp (entget item))
    (setq Pnt (cdr (assoc 10 temp)))
    (setq X (rtos (car Pnt) 2 3))
    (setq Y (rtos (cadr Pnt) 2 3))
    (setq Z (rtos (caddr Pnt) 2 3))
    (setq dong1 (list (rtos e 2 0) Y X Z))
    (setq dong (append dong (list dong1)))
    (setq e (1+ e))
  )
  (setq Dem (length dong))
  (if (vlax-get-or-create-object "Excel.Application")
    (WriteToExcel dong)
    (WriteToCSV dong)
  )
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)

(defun c:XTDL (/ DEM DONG DONG1 E P1 P2 SS TEMP X1 X2 Y1 Y2 Z1 Z2)
;;;;;;;;;;;;;;;;XUAT TOA DO LINE
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq ss (ssget (list (cons 0 "LINE"))))
  (setq dong1 (list))
  (setq dong (list))
  (setq Dem 0)
  (setq e 1)
  (foreach item	(acet-ss-to-list ss)
    (setq temp (entget item))
    (setq P1 (cdr (assoc 10 temp)))
    (setq P2 (cdr (assoc 11 temp)))
    (setq X1 (rtos (car P1) 2 3))
    (setq Y1 (rtos (cadr P1) 2 3))
    (setq Z1 (rtos (caddr P1) 2 3))
    (setq X2 (rtos (car P2) 2 3))
    (setq Y2 (rtos (cadr P2) 2 3))
    (setq Z2 (rtos (caddr P2) 2 3))
    (setq dong1 (list (rtos e 2 0) Y1 X1 Z1 Y2 X2 Z2))
    (setq dong (append dong (list dong1)))
    (setq e (1+ e))
  )
  (setq Dem (length dong))
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (if (/= dem 0)
    (progn
      (if (vlax-get-or-create-object "Excel.Application")
	(WriteToExcel dong)
	(WriteToCSV dong)
      )
    )
  )

  (princ)
)

(defun c:XTDP (/ DEM DONG DONG1 E PNT SS TEMP X Y Z)
;;;;;;;;;;Xuat toa do Point
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq ss (ssget (list (cons 0 "POINT"))))
  (setq e 1)
  (setq Dem 0)
  (foreach item	(acet-ss-to-list ss)
    (setq temp (entget item))
    (setq Pnt (cdr (assoc 10 temp)))
    (setq X (rtos (car Pnt) 2 3))
    (setq Y (rtos (cadr Pnt) 2 3))
    (setq Z (rtos (caddr Pnt) 2 3))
    (setq dong1 (list (rtos e 2 0) Y X Z))
    (setq dong (append dong (list dong1)))
    (setq e (1+ e))
  )
  (setq Dem (length dong))
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (if (/= dem 0)
    (progn
      (if (vlax-get-or-create-object "Excel.Application")
	(WriteToExcel dong)
	(WriteToCSV dong)
      )
    )
  )

  (princ)
)





(defun c:PDCAO (/ DATA DEM F LINE PNT TEN X Y Z) ;PHUN DO CAO

  (vl-load-com)
  (setvar "CMDECHO" 0)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Olmode (getvar "OSMODE"))
  (alert "Dinh dang file la: STT X Y Z [CODE]")
  (setq Dem 0)
  (or *h* (setq *h* 1))
  (setq	h (getreal
	    (strcat "\nNh\U+1EADp chi\U+1EC1u cao ch\U+1EEF <"
		    (rtos *h* 2 3)
		    ">: "
	    )
	  )
  )
  (if (not h)
    (setq h *h*)
    (setq *h* h)
  )
  (setvar "OSMODE" 0)
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (setq X (atof (nth 1 data)))
	      (setq Y (atof (nth 2 data)))
	      (setq Z (atof (nth 3 data)))
	      (setq Pnt (list Y X Z))
	      (MakeText Pnt (rtos Z 2 3) h 0 "L")
	    )
	 )
	 (setq Dem (1+ Dem))
      )
    )
  )
  (setvar "OSMODE" Olmode)
  (princ "\n")
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c phun t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)


(defun c:PCODE (/ CODE DATA DEM F LINE PNT TEN X Y Z)
;;;;;;;;;;;;;;;;;;;;PHUN CODE
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Olmode (getvar "OSMODE"))
  (alert "Dinh dang file la: STT X Y Z CODE")
  (setq Dem 0)
  (or *h* (setq *h* 0.20))
  (setq	h (getreal
	    (strcat "\nNh\U+1EADp chi\U+1EC1u cao ch\U+1EEF   <"
		    (rtos *h* 2 2)
		    "> :"
	    )
	  )
  )
  (if (not h)
    (setq h *h*)
    (setq *h* h)
  )
  (setvar "OSMODE" 0)
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq X (atof (nth 1 data)))
		(setq Y (atof (nth 2 data)))
		(setq Z (atof (nth 3 data)))
		(if (nth 4 data)
		  (setq Code (nth 4 data))
		  (setq Code "NO_CODE")
		)
		(setq Pnt (list Y X Z))
		(MakeText Pnt Code h 0 "L")
	      )
	    )
	 )
	 (setq Dem (1+ Dem))
      )
    )
  )
  
  (princ "\n")
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c phun t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (setvar "OSMODE" Olmode)
  (princ)
)

(defun c:PPNT (/ DATA DEM F LINE OLMODE PNT TEN X Y Z)
;;;;;;;;;;;;;;;;;;;;;;PHUN POINT
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Olmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq Dem 0)
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq X (atof (nth 1 data)))
		(setq Y (atof (nth 2 data)))
		(setq Z (atof (nth 3 data)))
		(setq Pnt (list Y X Z))
		(entmakex (list (cons 0 "POINT") (cons 10 Pnt)))
	      )
	    )
	 )
	 (setq Dem (1+ Dem))
      )
    )
  )
  (setvar "OSMODE" olmode)
  (princ "\n")
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c phun t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)





(defun c:PDT (/ DATA DEM F LINE OLMODE PNT TEN X Y Z)
;;;;;;;;;;;;;;;;;;;;PHUN DUONG TRON
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Olmode (getvar "OSMODE"))
  (setq Dem 0)
  (or *R* (setq *R* 0.5))
  (setq	R
	 (getdist
	   (strcat
	     "\nNh\U+1EADp b\U+00E1n k\U+00EDnh \U+0111\U+01B0\U+1EDDng tr\U+00F2n  <"
	     (rtos *R* 2 2)
	     "> :"
	   )
	 )
  )
  (if (not R)
    (setq R *R*)
    (setq *R* R)
  )

  (setvar "OSMODE" 0)
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq X (atof (nth 1 data)))
		(setq Y (atof (nth 2 data)))
		(setq Z (atof (nth 3 data)))
		(setq Pnt (list Y X Z))
		(entmakex
		  (list (cons 0 "CIRCLE") (cons 10 Pnt) (cons 40 R))
		)
	      )
	    )
	 )
	 (setq Dem (1+ Dem))
      )
    )
  )
  (setvar "OSMODE" olmode)
  (princ "\n")
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c phun t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)

(defun c:XTDPL (/ DEM I ITEM J L1 L2 L3 L4 L5 LTSVER P SS)
;;;;;;;;;;;;;;;;;XUAT TOA DO PLINE
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq L5 (list))
  (setq j 0)
  (setq Dem 0)
  (setq ss (ssget (list (cons 0 "*POLYLINE,LWPOLYLINE"))))
  (while
    (setq item (nth j (acet-ss-to-list ss)))
     (setq i 0)
     (setq L1 (list))
     (setq L2 (list))
     (setq L3 (list))
     (setq L4 (list))
     (setq LtsVer (acet-geom-vertex-list item))
     (setq L1 (list (strcat "Polyline co tat ca: "
			    (rtos (length LtsVer) 2 0)
			    " dinh"
		    )
	      )
     )
     (while (setq P (nth i LtsVer))
       (setq L2	(list (rtos (+ i 1) 2 0)
		      (rtos (cadr p) 2 3)
		      (rtos (car p) 2 3)
		      (rtos (caddr p) 2 3)
		)
       )
       (setq L3 (append L3 (list L2)))
       (setq i (1+ i))
     )
     (setq L3 (append (list L1) L3))
     (setq L5 (append L5 L3))
     (setq j (1+ j))
     (setq Dem (1+ Dem))
  )
  (if (vlax-get-or-create-object "Excel.Application")
    (WriteToExcel L5)
    (WriteToCSV L5)
  )
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c phun t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)

(defun c:PPL2D (/ DATA F LINE LTSPNT OLMODE PNT TEN X Y Z)
;;;;;;;;;;;;;;;;;;;;;;PHUN POLYLINE 2D
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Olmode (getvar "OSMODE"))
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq X (atof (nth 0 data)))
		(setq Y (atof (nth 1 data)))
		(setq Z (atof (nth 2 data)))
		(setq Pnt (list Y X Z))
		(setq LtsPnt (append LtsPnt (list Pnt)))
	      )
	    )
	 )
      )
    )
  )
  (MakeLWPolyline LtsPnt nil nil nil nil)
  (setvar "OSMODE" Olmode)
  (princ)
)

(defun c:PPL3D (/ DATA F LINE LTSPNT PNT TEN X Y Z)
;;;;;;;;;;;;;;;;;;;;;;PHUN POLYLINE 3D
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq X (atof (nth 0 data)))
		(setq Y (atof (nth 1 data)))
		(setq Z (atof (nth 2 data)))
		(setq Pnt (list Y X Z))
		(setq LtsPnt (append LtsPnt (list Pnt)))
	      )
	    )
	 )
      )
    )
  )
  (MakePolyline3D LtsPnt)
  (princ "\n")
  (princ)
)


(defun C:PTP (/ ENTLST NUM NUM_NEW SS TEMP) ;PHAN THAP PHAN
  (vl-load-com)
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (or *Sle* (setq *Sle* 1))
  (setq
    Sle	(getint
	  (strcat
	    "\nL\U+1EA5y sau s\U+1ED1 ph\U+1EA9y m\U+1EA5y s\U+1ED1 <"
	    (rtos *Sle* 2 0)
	    ">: "
	  )
	)
  )
  (if (not Sle)
    (setq Sle *Sle*)
    (setq *Sle* Sle)
  )
  (if (and Sle
	   (setq ss (ssget (list (cons 0 "TEXT"))))
      )
    (progn
      (setq ss (ChonTextSo ss))
      (setq entlst (vl-remove-if-not
		     '(lambda (x) (= (type x) 'ENAME))
		     (mapcar 'cadr (ssnamex ss))
		   )
      )
      (foreach item entlst
	(setq temp (entget item))
	(setq Num (atof (cdr (assoc 1 temp))))
	(setq Num_new (Lamtron Num Sle))
	(setq temp (subst (cons 1 (rtos (atof Num_new) 2 Sle))
			  (assoc 1 temp)
			  temp
		   )
	)
	(entmod temp)
      )
    )
  )
  (princ)
)


(defun c:PTDATT	(/	   ANS	     CODE      DATA	 DEM
		 F	   LINE	     LIST_ATT1 MACODE	 OLMODE
		 PNT	   SOTT	     SSATT_DECRP	 SSATT_ELE
		 SSATT_NAME	     SSATT_STT SSGROUP	 SSPOINTATT
		 STT	   TEN	     X	       Y	 Z
		)			;PHUN ATTRIBUTE
  (vl-load-com)
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq Dem 0)
  (setq Olmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (MakeLayer_ "STT" 1)
  (MakeLayer_ "NAME" 1)
  (MakeLayer_ "ELE" 2)
  (MakeLayer_ "DECRP" 3)
  (setvar "CECOLOR" "BYLAYER")
  (if (not (tblsearch "BLOCK" "D_chitiet"))
    (progn
      (setvar "OSMODE" 0)
      (setvar "CLAYER" "0")
      (setvar "CECOLOR" "BYLAYER")
      (setq ssPointATT (ssadd))
      (entmake (list (cons 0 "POINT") (cons 10 '(0 0 0))))
      (ssadd (entlast) ssPointATT)
      (setvar "CLAYER" "STT")
      (setq ssAtt_STT (ssadd))
      (command "ATTDEF" "" "STT" "STT" "" "ML" '(1.0 2.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_STT)
      (setvar "CLAYER" "ELE")
      (setq ssAtt_ELE (ssadd))
      (command "ATTDEF" "" "ELE" "ELE" "" "ML" '(1.0 0.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_ELE)
      (setvar "CLAYER" "DECRP")
      (setq ssAtt_DECRP (ssadd))
      (command "ATTDEF"	"" "DECRP" "DECRP" "" "ML" '(1.0 -2.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_DECRP)
      (setvar "CLAYER" "0")
      (command "BLOCK"	     "D_chitiet"   '(0 0)	 ssPointATT
	       ssAtt_STT     ssAtt_ELE	   ssAtt_DECRP	 ""
	      )
      (setvar "CLAYER" "0")
    )
  )

  (if (not (tblsearch "BLOCK" "D_KV"))
    (progn
      (setvar "OSMODE" 0)
      (setvar "CLAYER" "0")
      (setq ssGroup (ssadd))
      (MakeDonut '(0 0 0) 0.1)
      (ssadd (entlast) ssGroup)
      (MakeCircle '(0 0 0) 0.5)
      (ssadd (entlast) ssGroup)
      (MakeLine '(0.5 0.0) '(1.0 0.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(0.0 -0.5) '(0.0 -1.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(-0.5 0.0) '(-1.0 0.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(0.0 0.5) '(0.0 1.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(1.5 0.0) '(6.0 0.0))
      (ssadd (entlast) ssGroup)
      (setvar "CLAYER" "NAME")
      (setq ssAtt_NAME (ssadd))
      (command "ATTDEF"	"" "NAME" "NAME" "" "ML" '(1.5 1.0) 1.0	0.0)
      (ssadd (entlast) ssAtt_NAME)

      (setvar "CLAYER" "ELE")
      (setq ssAtt_ELE (ssadd))
      (command "ATTDEF"	"" "ELE" "ELE" "" "ML" '(1.5 -1.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_ELE)
      (setvar "CLAYER" "0")
      (command "BLOCK" "D_KV" '(0 0) ssGroup ssAtt_NAME	ssAtt_ELE "")
      (setvar "CLAYER" "0")
    )
  )

  (if (not (tblsearch "BLOCK" "D_DC"))
    (progn
      (setvar "OSMODE" 0)
      (setvar "CLAYER" "0")
      (setq ssGroup (ssadd))
      (MakeDonut '(0 0 0) 0.1)
      (ssadd (entlast) ssGroup)
      (command "Rectang" '(-0.5 0.5) '(0.5 -0.5))
      (ssadd (entlast) ssGroup)
      (MakeLine '(0.5 0.0) '(1.0 0.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(0.0 -0.5) '(0.0 -1.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(-0.5 0.0) '(-1.0 0.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(0.0 0.5) '(0.0 1.0))
      (ssadd (entlast) ssGroup)
      (MakeLine '(1.5 0.0) '(6.0 0.0))
      (ssadd (entlast) ssGroup)

      (setvar "CLAYER" "NAME")
      (setq ssAtt_NAME (ssadd))
      (command "ATTDEF"	"" "NAME" "NAME" "" "ML" '(1.5 1.0) 1.0	0.0)
      (ssadd (entlast) ssAtt_NAME)

      (setvar "CLAYER" "ELE")
      (setq ssAtt_ELE (ssadd))
      (command "ATTDEF"	"" "ELE" "ELE" "" "ML" '(1.5 -1.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_ELE)
      (setvar "CLAYER" "0")
      (command "BLOCK" "D_DC" '(0 0) ssGroup ssAtt_NAME	ssAtt_ELE "")
      (setvar "CLAYER" "0")
    )
  )

  (if (not (tblsearch "BLOCK" "D_chitiet"))
    (progn
      (setvar "OSMODE" 0)
      (setvar "CLAYER" "0")
      (setvar "CECOLOR" "BYLAYER")
      (setq ssPointATT (ssadd))
      (entmake (list (cons 0 "POINT") (cons 10 '(0 0 0))))
      (ssadd (entlast) ssPointATT)
      (setvar "CLAYER" "STT")
      (setq ssAtt_STT (ssadd))
      (command "ATTDEF" "" "STT" "STT" "" "ML" '(1.0 2.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_STT)
      (setvar "CLAYER" "ELE")
      (setq ssAtt_ELE (ssadd))
      (command "ATTDEF" "" "ELE" "ELE" "" "ML" '(1.0 0.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_ELE)
      (setvar "CLAYER" "DECRP")
      (setq ssAtt_DECRP (ssadd))
      (command "ATTDEF"	"" "DECRP" "DECRP" "" "ML" '(1.0 -2.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_DECRP)
      (setvar "CLAYER" "0")
      (command "BLOCK"	     "D_chitiet"   '(0 0)	 ssPointATT
	       ssAtt_STT     ssAtt_ELE	   ssAtt_DECRP	 ""
	      )
      (setvar "CLAYER" "0")
    )
  )

  (if (not (tblsearch "BLOCK" "D_TDNN"))
    (progn
      (setvar "OSMODE" 0)
      (setvar "CLAYER" "0")
      (setq ssGroup (ssadd))
      (MakeDonut '(0 0 0) 0.1)
      (ssadd (entlast) ssGroup)
      (command "Polygon" "3" '(0.0 0.0) "" "1")
      (ssadd (entlast) ssGroup)
      (MakeLine '(1.5 0.0) '(6.0 0.0))
      (ssadd (entlast) ssGroup)
      (setvar "CLAYER" "NAME")
      (setq ssAtt_NAME (ssadd))
      (command "ATTDEF"	"" "NAME" "NAME" "" "ML" '(1.5 1.0) 1.0	0.0)
      (ssadd (entlast) ssAtt_NAME)

      (setvar "CLAYER" "ELE")
      (setq ssAtt_ELE (ssadd))
      (command "ATTDEF"	"" "ELE" "ELE" "" "ML" '(1.5 -1.0) 1.0 0.0)
      (ssadd (entlast) ssAtt_ELE)
      (setvar "CLAYER" "0")
      (command "BLOCK" "D_TDNN"	'(0 0) ssGroup ssAtt_NAME ssAtt_ELE "")
      (setvar "CLAYER" "0")
    )
  )



  (initget "CT KV DC NN")
  (setq	ans
	 (getkword
	   "\n B\U+1EA1n mu\U+1ED1n phun \U+0111i\U+1EC3m d\U+1EA1ng: CT - \U+0110i\U+1EC3m chi ti\U+1EBFt/ KV - \U+0110i\U+1EC3m Kinh v\U+0129 / DC - \U+0110i\U+1EC3m \U+0111\U+01B0\U+1EDDng chuy\U+1EC1n / NN - \U+0110i\U+1EC3m t\U+1ECDa \U+0111\U+1ED9 Nh\U+00E0 N\U+01B0\U+1EDBc     : "
	 )
  )
  (command "ATTDISP" "ON")
  (command "ATTREQ" 1)
  (command "INSUNITS" 1)
  (if (not Code)
    (setq Code "NO_CODE")
  )
  (if (not SoTT)
    (setq SoTT "1")
  )

  (setvar "OSMODE" 0)
  (if (setq ten (getfiled "Select File" (getvar "dwgprefix") "*" 8))
    (progn
      (setq f (open (findfile ten) "r"))
      (while
	(setq Line (read-line f))
	 (if
	   (wcmatch Line
		    (strcat "*" (chr 9) "*,*" (chr 32) "*,*" (chr 44) "*")
	   )
	    (progn
	      (setq data (replace_str Line))
	      (progn
		(setq STT (nth 0 data))
		(setq X (atof (nth 1 data)))
		(setq Y (atof (nth 2 data)))
		(setq Z (atof (nth 3 data)))
		(if (nth 4 data)
		  (setq MaCode (nth 4 data))
		  (setq MaCode "NO_CODE")
		)
		(setq Pnt (list Y X Z))
		(if (distof STT 2)
		  (setq SoTT STT)
		  (setq SoTT (rtos STT 2 0))
		)
		(setq Code MaCode)
		(cond
		  ((= (strcase ans) "CT")
		   (progn
		     (setq list_att1 (list SoTT (rtos Z 2 3) Code))
		     (MakeInsertBlock
		       "D_CHITIET" Pnt 1 0 list_att1 nil nil nil)
		   )
		  )
		  ((= (strcase ans) "KV")
		   (progn
		     (setq list_att1 (list Code (rtos Z 2 3)))
		     (MakeInsertBlock
		       "D_KV" Pnt 1 0 list_att1	nil nil	nil)
		   )
		  )
		  ((= (strcase ans) "DC")
		   (progn
		     (setq list_att1 (list Code (rtos Z 2 3)))
		     (MakeInsertBlock
		       "D_DC" Pnt 1 0 list_att1	nil nil	nil)
		   )
		  )
		  ((= (strcase ans) "NN")
		   (progn
		     (setq list_att1 (list Code (rtos Z 2 3)))
		     (MakeInsertBlock
		       "D_TDNN"	Pnt 1 0	list_att1 nil nil nil)
		   )
		  )
		)
	      )
	      (setq Dem (1+ Dem))
	    )

	 )
      )
    )
  )
  (setvar "OSMODE" Olmode)
  (princ "\n")
  (alert
    (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	    (rtos Dem 2 0)
	    " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
    )
  )
  (princ)
)


(defun c:XTDATT	(/	DECRP  DEM    ELE    ELE1   ENT	   ENT1
		 I	J      LTS1   LTS1A  LTS2A  LTS2B  LTS2C
		 NAME	OBJ    OBJ1   POS    POS1   SS	   SSCT
		 SSTH	STT    STT1   X
		)
  (vl-load-com)
  (setq Lts1 (list))
  (setq Lts1a (list))
  (setq Lts2a (list))
  (setq Lts2b (list))
  (setq Lts2c (list))
  (setq i 0)
  (setq Dem 0)
  (setq STT1 0)
  (setq j 0)
  (Alert
    "\nQu\U+00E9t ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng Attribute :"
  )
  (if (setq ss (ssget (list (cons 0 "INSERT"))))
    (progn
      (setq ssCT
	     (vl-remove	nil
			(mapcar	'(lambda (x)
				   (if (= (vla-get-effectivename
					    (vlax-ename->vla-object x)
					  )
					  "D_CHITIET"
				       )
				     x
				     nil
				   )
				 )
				(acet-ss-to-list ss)
			)
	     )
      )
      (setq ssTH
	     (vl-remove
	       nil
	       (mapcar '(lambda	(x)
			  (if (or (= (acet-dxf 2 (entget x)) "D_KV")
				  (= (acet-dxf 2 (entget x)) "D_DC")
				  (= (acet-dxf 2 (entget x)) "D_TDNN")
			      )
			    x
			    nil
			  )
			)
		       (acet-ss-to-list ss)
	       )
	     )
      )
      (progn
	(if ssCT
	  (while
	    (setq ent (nth i ssCT))
	     (setq i (1+ i))
	     (setq obj (vlax-ename->vla-object ent)
		   pos (mapcar 'rtos (vlax-get obj 'InsertionPoint))
	     )
	     (foreach att (vlax-invoke obj 'GetAttributes)
	       (cond
		 ((= (vla-get-TagString att) "STT")
		  (setq STT (vla-get-TextString att))
		 )
		 ((= (vla-get-TagString att) "ELE")
		  (setq ELE (vla-get-TextString att))
		 )
		 ((= (vla-get-TagString att) "DECRP")
		  (setq DECRP (vla-get-TextString att))
		 )
	       )
	     )
	     (setq Lts1 (list STT (cadr pos) (car pos) ELE DECRP))
	     (setq Lts2a (append Lts2a (list Lts1)))
	  )
	)
	(if ssTH
	  (while
	    (setq ent1 (nth j ssTH))
	     (setq j (1+ j))
	     (setq obj1	(vlax-ename->vla-object ent1)
		   pos1	(mapcar 'rtos (vlax-get obj1 'InsertionPoint))
	     )
	     (foreach att1 (vlax-invoke obj1 'GetAttributes)
	       (cond
		 ((= (vla-get-TagString att1) "NAME")
		  (setq NAME (vla-get-TextString att1))
		 )
		 ((= (vla-get-TagString att1) "ELE")
		  (setq ELE1 (vla-get-TextString att1))
		 )
	       )
	     )
	     (setq STT1 (+ STT1 1))
	     (setq Lts1a (list (rtos STT1 2 0)
			       (cadr pos1)
			       (car pos1)
			       ELE1
			       NAME
			 )
	     )
	     (setq Lts2b (append Lts2b (list Lts1a)))
	  )
	)
	(cond ((and ssCT ssTH)
	       (setq Lts2c (append Lts2a Lts2b))
	      )
	      ((and ssCT)
	       (setq Lts2c Lts2a)
	      )
	      ((and ssTH)
	       (setq Lts2c Lts2b)
	      )
	)
      )
      (setq Dem (length Lts2c))
      (alert
	(strcat
	  "\nC\U+00F3 t\U+1EA5t c\U+1EA3 "
	  (rtos Dem 2 0)
	  " \U+0111\U+01B0\U+1EE3c xu\U+1EA5t t\U+1ECDa \U+0111\U+1ED9"
	)
      )
      (if (/= dem 0)
	(progn
	  (if (vlax-get-or-create-object "Excel.Application")
	    (WriteToExcel Lts2c)
	    (WriteToCSV Lts2c)
	  )
	)
      )
    )
  )

  (princ)
)





(defun ChonTextSo (ss / ss i ent str ss1)
  (progn
    (setq i   0
	  ss1 (ssadd)
    )
    (repeat (sslength ss)
      (setq ent	(ssname ss i)
	    str	(cdr (assoc 1 (entget ent)))
	    i	(+ 1 i)
      )
      (if (distof str 2)
	(ssadd ent ss1)
      )
    )
    (if	(> (sslength ss1) 0)
      ss1
    )
  )
)

(defun rnd (x)
  (if (>= x 0)
    (fix (+ x 0.5))
    (fix (- x 0.5))
  )
)


(defun LM:PadBetween (s1 s2 ch ln)
  (
   (lambda (a b c)
     (repeat (- ln (length b) (length c)) (setq c (cons a c)))
     (vl-list->string (append b c))
   )
    (ascii ch)
    (vl-string->list s1)
    (vl-string->list s2)
  )
)


;;;;;;;;;(LM:PadBetween "a" "" "a" 5)
(defun Lamtron (x dp / I J S S1 S1A S2 SLETP)
  (setq SleTP (CountDel x))
  (setq S (itoa (rnd (* (abs x) (expt 10 dp)))))
  (setq i (strlen S))
  (setq j (- i dp))
  (if (< j 0)
    (progn
      (setq S1a (strcat (LM:PadBetween "0" "" "0" SleTP) S))
      (setq S1 "0")
      (setq S2 (substr S1a 1 dp))
    )
    (progn
      (setq S1 (substr S 1 j))
      (if (equal S1 "")
	(setq S1 "0")
      )
      (setq S2 (substr S (1+ j) dp))
    )
  )
  (if (>= x 0)
    (strcat S1 "." S2)
    (strcat "-" S1 "." S2)
  )
)
;;;dem so 0 dang sau so phay
;;;;(CountDel 0.0000345) = 4
(defun CountDel	(NUMBER /)
  (setq i -1)
  (while (equal (fix Number) 0)
    (setq Number (* Number 10))
    (setq i (1+ i))
  )
  i
)

(defun MakeText	(point string Height Ang justify / Lst) ; Ang: Radial
  (setq	Lst	(list '(0 . "TEXT")
		      (cons 10 point)
		      (cons 40 Height)
		      (cons 1 string)
		      (cons 50 Ang)

		)
	justify	(strcase justify)
  )
  (cond
    ((= justify "C")
     (setq Lst (append Lst (list (cons 72 1) (cons 11 point))))
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
  (entmakex Lst)
)










(defun MakeDonut (CenterPoint Width / Radius)
  (setq OuterRadius 0)
  (setq	Radius (- OuterRadius (/ Width 2.0))
  )
  (entmake
    (list '(0 . "LWPOLYLINE")
	  '(100 . "AcDbEntity")
	  '(100 . "AcDbPolyline")
	  '(90 . 2)
	  '(70 . 1)
	  (cons 38 (caddr CenterPoint))
	  (cons 43 width)
	  (list 10 (- (car CenterPoint) radius) (cadr CenterPoint))
	  '(42 . 1.0)
	  (list 10 (+ (car CenterPoint) radius) (cadr CenterPoint))
	  '(42 . 1.0)
    )
  )
)

(defun MakeCircle (point R)
  (entmakex (list '(0 . "CIRCLE")
		  (cons 10 point)
		  (cons 40 R)
	    )
  )
)

(defun MakeLine	(PT1 PT2)
  (entmakex (list '(0 . "LINE")
		  (cons 10 PT1)
		  (cons 11 PT2)
	    )
  )
)

(defun TD:Text-Base (ent)
  (setq Ma10 (cdr (assoc 10 (entget ent))))
  (setq Ma11 (cdr (assoc 11 (entget ent))))
  (setq X11 (car Ma11))
  (setq Ma71 (cdr (assoc 71 (entget ent))))
  (setq Ma72 (cdr (assoc 72 (entget ent))))
  (if (or (and (= Ma71 0) (= Ma72 0) (= X11 0))
	  (and (= Ma71 0) (= Ma72 3))
	  (and (= Ma71 0) (= Ma72 5))
      )
    Ma10
    Ma11
  )
)


(defun MakeInsert (Blkname  inspoint scale    ang      list_att
		   layer    color    xdata    /	       lst
		   obj	    i
		  )
  (setq	lst '()
	i   -1
	en  (cdr (last (tblsearch "block" Blkname)))
	obj (entget en)
  )
  (if (= (cdr (assoc 0 obj)) "ATTDEF")
    (setq lst (list (list
		      (cdr (assoc 10 obj))
		      (cdr (assoc 11 obj))
		      (cdr (assoc 50 obj))
		      (cdr (assoc 8 obj))
		      (cdr (assoc 70 obj))
		      (cdr (assoc 62 obj))
		      (cdr (assoc 40 obj))
		      (assoc 7 obj)
		      (assoc 71 obj)
		      (assoc 72 obj)
		      (assoc 2 obj)
		    )
	      )
    )
  )
  (while (setq en (entnext en))
    (if	(= (cdr (assoc 0 (setq obj (entget en)))) "ATTDEF")
      (setq lst	(cons (list
			(cdr (assoc 10 obj))
			(cdr (assoc 11 obj))
			(cdr (assoc 50 obj))
			(cdr (assoc 8 obj))
			(cdr (assoc 70 obj))
			(cdr (assoc 62 obj))
			(cdr (assoc 40 obj))
			(assoc 7 obj)
			(assoc 71 obj)
			(assoc 72 obj)
			(assoc 2 obj)
		      )
		      lst
		)
      )
    )
  )
  (entmakex (list
	      '(0 . "INSERT")
	      '(100 . "AcDbEntity")
	      (cons 8
		    (if	Layer
		      Layer
		      (getvar "Clayer")
		    )
	      )
	      (cons 62
		    (if	Color
		      Color
		      256
		    )
	      )
	      '(100 . "AcDbBlockReference")
	      (if list_att
		'(66 . 1)
		'(66 . 0)
	      )
	      (cons 2 Blkname)
	      (cons 10 (trans inspoint 1 0))
	      (cons 41 scale)
	      (cons 42 scale)
	      (cons 43 scale)
	      (cons 50 Ang)
	      (cons -3
		    (if	xdata
		      (list xdata)
		      nil
		    )
	      )
	    )
  )
  (if lst
    (foreach LL	(reverse lst)
      (entmake
	(list
	  '(0 . "ATTRIB")
	  '(100 . "AcDbEntity")
	  (cons 8 (cadddr LL))
	  (cons 60 (nth 4 LL))
	  (if (nth 5 LL)
	    (cons 62 (nth 5 LL))
	    '(62 . 256)
	  )
	  '(100 . "AcDbText")
	  (cons	10
		(mapcar	'+
			(trans inspoint 1 0)
			(mapcar	'(lambda (x) (* scale x))
				(polar '(0 0 0)
				       (+ (angle '(0 0 0) (car LL)) ang)
				       (distance '(0 0 0) (car LL))
				)
			)
		)
	  )
	  (cons 40 (* scale (nth 6 LL)))
	  (cons 1 (nth (setq i (1+ i)) list_att))
	  (cons 50 (+ ang (caddr LL)))
	  '(41 . 1.0)
	  (nth 7 LL)
	  (nth 8 LL)
	  (nth 9 LL)
	  (if (= 0 (cdr (nth 8 LL)) (cdr (nth 9 LL)))
	    (cons 11 (list 0 0 0))
	    (cons
	      11
	      (mapcar '+
		      (trans inspoint 1 0)
		      (mapcar '(lambda (x) (* scale x))
			      (polar '(0 0 0)
				     (+ (angle '(0 0 0) (cadr LL)) ang)
				     (distance '(0 0 0) (cadr LL))
			      )
		      )
	      )
	    )
	  )
	  '(100 . "AcDbAttribute")
	  '(280 . 0)
	  (last LL)
	  '(70 . 0)
	  '(280 . 1)
	)
      )
    )
  )
  (dxf 330
       (entmakex (list '(0 . "SEQEND")
		       (cons 8
			     (if Layer
			       Layer
			       (getvar "Clayer")
			     )
		       )
		 )
       )
  )
)




(defun MakeInsertBlock (Blkname	 inspoint scale	   ang	    list_att
			layer	 color	  xdata	   /	    lst
			obj	 i
		       )

  (setq	lst '()
	i   -1
	en  (cdr (last (tblsearch "block" Blkname)))
	obj (entget en)
  )
  (if (= (cdr (assoc 0 obj)) "ATTDEF")
    (setq lst (list (list
		      (cdr (assoc 10 obj))
		      (cdr (assoc 11 obj))
		      (cdr (assoc 50 obj))
		      (cdr (assoc 8 obj))
		      (cdr (assoc 62 obj))
		      (cdr (assoc 40 obj))
		      (assoc 7 obj)
		      (assoc 71 obj)
		      (assoc 72 obj)
		      (assoc 2 obj)
		    )
	      )
    )
  )
  (while (setq en (entnext en))
    (if	(= (cdr (assoc 0 (setq obj (entget en)))) "ATTDEF")
      (setq lst	(cons (list
			(cdr (assoc 10 obj))
			(cdr (assoc 11 obj))
			(cdr (assoc 50 obj))
			(cdr (assoc 8 obj))
			(cdr (assoc 62 obj))
			(cdr (assoc 40 obj))
			(assoc 7 obj)
			(assoc 71 obj)
			(assoc 72 obj)
			(assoc 2 obj)
		      )
		      lst
		)
      )
    )
  )
  (entmakex (list
	      '(0 . "INSERT")
	      '(100 . "AcDbEntity")
	      (cons 8
		    (if	Layer
		      Layer
		      (getvar "Clayer")
		    )
	      )
	      (cons 62
		    (if	Color
		      Color
		      256
		    )
	      )
	      '(100 . "AcDbBlockReference")
	      (if list_att
		'(66 . 1)
		'(66 . 0)
	      )
	      (cons 2 Blkname)
	      (cons 10 (trans inspoint 1 0))
	      (cons 41 scale)
	      (cons 42 scale)
	      (cons 43 scale)
	      (cons 50 Ang)
	      (cons -3
		    (if	xdata
		      (list xdata)
		      nil
		    )
	      )
	    )
  )
  (if lst
    (foreach LL	(reverse lst)
      (entmake
	(list
	  '(0 . "ATTRIB")
	  '(100 . "AcDbEntity")
	  (cons 8 (cadddr LL))
	  (if (nth 4 LL)
	    (cons 62 (nth 4 LL))
	    (cons 62 256)
	  )
	  '(100 . "AcDbText")
	  (cons	10
		(mapcar	'+
			(trans inspoint 1 0)
			(mapcar	'(lambda (x) (* scale x))
				(polar '(0 0 0)
				       (+ (angle '(0 0 0) (car LL)) ang)
				       (distance '(0 0 0) (car LL))
				)
			)
		)
	  )
	  (cons 40 (* scale (nth 5 LL)))
	  (cons 1 (nth (setq i (1+ i)) list_att))
	  (cons 50 (+ ang (caddr LL)))
	  '(41 . 1.0)
	  (nth 6 LL)
	  (nth 7 LL)
	  (nth 8 LL)
	  (if (= 0 (cdr (nth 7 LL)) (cdr (nth 8 LL)))
	    (cons 11 (list 0 0 0))
	    (cons
	      11
	      (mapcar '+
		      (trans inspoint 1 0)
		      (mapcar '(lambda (x) (* scale x))
			      (polar '(0 0 0)
				     (+ (angle '(0 0 0) (cadr LL)) ang)
				     (distance '(0 0 0) (cadr LL))
			      )
		      )
	      )
	    )
	  )
	  '(100 . "AcDbAttribute")
	  '(280 . 0)
	  (last LL)
	  '(70 . 0)
	  '(280 . 1)
	)
      )
    )
  )
  (dxf 330
       (entmakex (list '(0 . "SEQEND")
		       (cons 8
			     (if Layer
			       Layer
			       (getvar "Clayer")
			     )
		       )
		 )
       )
  )
)
(defun DXF (code en) (cdr (assoc code (entget en))))

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

(defun MakeLayer_ (name colour /)
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



(defun MakeInsertBL (Blkname inspoint scale ang / lst obj i)
  (setq	lst '()
	i   -1
	en  (cdr (last (tblsearch "block" Blkname)))
	obj (entget en)
  )
  (entmakex (list
	      '(0 . "INSERT")
	      '(100 . "AcDbEntity")
	      '(100 . "AcDbBlockReference")
	      (cons 2 Blkname)
	      (cons 10 (trans inspoint 1 0))
	      (cons 41 scale)
	      (cons 42 scale)
	      (cons 43 scale)
	      (cons 50 Ang)
	    )
  )
)

(defun MakeLWPolyline (listpoint Linetype LTScale Layer Color / Lst)
  (setq	Lst (list (cons 0 "LWPOLYLINE")
		  (cons 100 "AcDbEntity")
		  (cons	8
			(if Layer
			  Layer
			  (getvar "Clayer")
			)
		  )
		  (cons	6
			(if Linetype
			  Linetype
			  "bylayer"
			)
		  )
		  (cons	48
			(if LTScale
			  LTScale
			  1
			)
		  )
		  (cons	62
			(if Color
			  Color
			  256
			)
		  )
		  (cons 100 "AcDbPolyline")
		  (cons 90 (length listpoint))
	    )
  )
  (foreach PP listpoint
    (setq Lst (append Lst (list (cons 10 PP))))
  )
  (entmakex Lst)
)

(defun MakePolyline3D (vtcs)
  (entmake
    (list
      '(0 . "POLYLINE")
      '(66 . 1)
      '(70 . 8)
    )
  )
  (foreach vtx vtcs
    (entmake
      (list
	'(0 . "VERTEX")
	(cons 10 vtx)
	'(70 . 32)
      )
    )
  )
  (entmake '((0 . "SEQEND")))
)