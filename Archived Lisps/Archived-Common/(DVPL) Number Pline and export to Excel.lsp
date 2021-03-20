(vl-load-com)
(defun c:DVPL ( / Olmode  ObjTuyen  KCC h LtsVer Cdai VlaObj d pt STT Lts1 Lts2 n h );;;;DIVIDE POLYLINE
(prompt "\nNguyen Thien Duong")
(setvar "CMDECHO" 0)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))

(setq ObjTuyen (car (entsel "\nChon Polyline can chia: ")))
(setvar "OSMODE" 1)
(or *KCC* (setq *KCC* 0.5))
(setq KCC (getreal (strcat "\nNhap khoang cach can chia: <"
	  (rtos *KCC* 2 2)
	 "> :"
  )
)
)
(if (not KCC) (setq KCC *KCC*) (setq *KCC* KCC))
  
(or *h* (setq *h* 1))
(setq h (getreal (strcat "\nNhap chieu cao chu: <"
	  (rtos *h* 2 2)
	 "> :"
  )
)
)
(if (not h) (setq h *h*) (setq *h* h))
  
(setq Cdai (Length1 ObjTuyen))
(setq VlaObj (vlax-ename->vla-object ObjTuyen))
(MakeLayer_  "TUYEN" 1)
(MakeLayer_  "STT" 2)
(PUTLAYER ObjTuyen  "TUYEN")
(setq n (/ Cdai KCC))
(setq STT 1)
(setq d 0)
(setq Lts1 (list))
(setq Lts2 (list))  
(while (< d (vla-get-length VlaObj))
	(setq Pt (vlax-curve-getPointAtDist ObjTuyen d))
	(setq d (+ d (/ (vla-get-length VlaObj) n)))
  	(entmake (list (cons 0 "TEXT") (cons 10 pt) (cons 8 "STT") (cons 1 (rtos STT 2 0)) (cons 40 h)))
  	(setq Lts1 (list STT (car pt) (cadr pt)))
  	(setq Lts2 (append Lts2 (list Lts1)))
  	(setq STT (1+ STT))
)



(if (vlax-get-or-create-object "Excel.Application")
	(WriteToExcel Lts2)
	(WriteToCSV Lts2)
)




  
(setvar "OSMODE" Olmode)
(princ)
)



(defun Length1 (e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))



(defun PUTLAYER (ent NameLayer / s)
   (setq s (vlax-ename->vla-object ent) )
   (vla-put-layer s NameLayer )
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


(defun WriteToExcel (lst_data / col row x xlApp xlCells)
 
  (setq xlApp (vlax-get-or-create-object "Excel.Application")
xlCells (vlax-get-property
	(vlax-get-property
  	(vlax-get-property
    	(vlax-invoke-method
   (vlax-get-property xlApp "Workbooks")
   "Add")
    	"Sheets")
  	"Item" 1)
	"Cells"))
  (setq row 1)
  (foreach pt lst_data
	(setq col 1)
	(foreach coor pt
  	(vlax-put-property xlCells 'Item row col coor)
  	(setq col (1+ col)))
	(setq row (1+ row)) )
  (vla-put-visible xlApp :vlax-true)
  (mapcar
	(function (lambda (x)
  (vl-catch-all-apply (function (lambda ()(if x (vlax-release-object x)))))))
	(list xlCells xlApp))
  (gc) (gc) )
 
(defun WriteToCSV (lst_data / fl)
  (if (setq fl (getfiled "Output File" "" "csv" 1))
	(if (setq fl (open fl "w"))
  	(progn
(foreach pt lst_data
   (write-line (strcat (rtos (car pt)) "," (rtos (cadr pt)) "," (rtos (caddr pt))) fl) )
(close fl) ) ) )
  (princ))