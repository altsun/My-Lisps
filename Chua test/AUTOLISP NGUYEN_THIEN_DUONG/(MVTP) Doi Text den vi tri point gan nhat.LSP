(defun c:MVTP (/ ss ss1a ss1 ss2 L1 L2 v EnameTGN );;;MOVE TEXT TO POINT
(setq ss (ssget '((0 . "TEXT,POINT"))))
(setq ss1a (vl-remove nil (mapcar '(lambda(x) (if (= (acet-dxf 0 (entget x)) "TEXT") x nil)) (acet-ss-to-list ss))))
(setq ss1 (acet-ss-to-list (ChonTextSo (acet-list-to-ss ss1a))))
(setq ss2 (vl-remove nil (mapcar '(lambda(x) (if (= (acet-dxf 0 (entget x)) "POINT")  (acet-dxf 10 (entget x)) nil)) (acet-ss-to-list ss))));;;loai bo cac doi tuong khong co thuoc tinh ra khoi tap hop
(setq L1 (list))
(setq L2 (list))
(foreach v ss2
  	(setq EnameTGN  (car (vl-sort ss1 '(lambda(x y) (< (distance (TD:Text-Base x) v) (distance (TD:Text-Base y) v))))))
  	(setq L1 (list (cadr v) (car v) (atof (acet-dxf 1 (entget EnameTGN)))))
  	(setq L2 (append L2 (list L1)))
  	(setq ss1 (vl-remove EnameTGN  ss1))
)
(if (vlax-get-or-create-object "Excel.Application")
			(WriteToExcel L2)
			(WriteToCSV L2)
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


(defun ChonTextSo (ss / ss i ent str ss1) 
    (progn
      (setq i	0
	    ss1	(ssadd)
      )
      (repeat (sslength ss)
	(setq ent (ssname ss i)
	      str (cdr(assoc 1 (entget ent)))
	      i	  (+ 1 i)
	)
	(if (distof str 2)
	  (ssadd ent ss1)
	)
      )
      (if (> (sslength ss1) 0)
	ss1
      )      
    )
)


(defun c:test(/ os ss ss1 ss2 cd) 
  (defun layxy(a) (list (car a) (cadr a)))
  (defun leftL(L n / l1 i) (setq l1 nil i -1) (while (and (< (setq i (1+ i)) n) (nth i L)) (setq l1 (append l1 (list (nth i L))))))
  (defun doi (id tri v)   (entmod (subst (cons id tri) (assoc id (entget v)) (entget v))))
  
  ;;;chuong trinh chinh
  (setq ss (ssget '((0 . "TEXT,POINT")))
ss1 (vl-remove nil (mapcar '(lambda(x) (if (= (acet-dxf 0 (entget x)) "TEXT") (cons (layxy (acet-dxf 11 (entget x))) x) nil)) (acet-ss-to-list ss)))
ss1 (vl-sort (vl-sort ss1 '(lambda(x y) (< (cadar x) (cadar y)))) '(lambda(x y) (< (caar x) (caar y))))
ss2 (vl-remove nil (mapcar '(lambda(x) (if (= (acet-dxf 0 (entget x)) "POINT") (layxy (acet-dxf 10 (entget x))) nil)) (acet-ss-to-list ss)))
ss2 (vl-sort (vl-sort ss2 '(lambda(x y) (< (cadr x) (cadr y)))) '(lambda(x y) (< (car x) (car y))))
  )
  (foreach v ss1    
    (doi 11 (setq cd (car (vl-sort (leftL ss2 10) '(lambda(x y) (< (distance x (car v)) (distance y (car v))))))) (cdr v))
    (setq ss2 (vl-remove cd  ss2))
  )
)
