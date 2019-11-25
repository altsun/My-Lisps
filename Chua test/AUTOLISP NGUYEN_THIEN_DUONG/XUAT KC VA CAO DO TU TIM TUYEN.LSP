;;;(alert (strcat "\n L\U+1EC7nh ch\U+01B0\U+01A1ng tr\U+00ECnh xu\U+1EA5t kho\U+1EA3ng c\U+00E1ch c\U+1ED9ng d\U+1ED3n t\U+1EEB tim tuy\U+1EBFn l\U+00E0: KCT"
;;;		"\n Ng\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;		"\n Mobile: 0972.0168.25"
;;;	)
;;;)
;;;(Prompt (strcat "\n L\U+1EC7nh ch\U+01B0\U+01A1ng tr\U+00ECnh xu\U+1EA5t kho\U+1EA3ng c\U+00E1ch c\U+1ED9ng d\U+1ED3n t\U+1EEB tim tuy\U+1EBFn l\U+00E0: KCT"
;;;		"\n Ng\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;		"\n Mobile: 0972.0168.25"
;;;	)
;;;)
(defun C:KCT(/ fn fh CTM Pnt_TM DDH Pnt_DH GocAB Name_TIM ss1 ss item temp ND_text Tdo Pnt_ct KCBC GocBeta) ; Khoang cach tim
(vl-load-com)
(setvar "CMDECHO" 0)
(setq Lst1 (list))
(setq Lst2 (list))
(Prompt (strcat "\n Ch\U+1ECDn Text t\U+1EA1i  tim tuy\U+1EBFn"))
(Alert (strcat "\n Ch\U+1ECDn Text t\U+1EA1i  tim tuy\U+1EBFn"))
(setq D_TIM (entsel "\nCh\U+1ECDn Text t\U+1EA1i  tim tuy\U+1EBFn: "))
(if (and D_TIM (= (cdr (assoc 0 (entget (car D_TIM)))) "TEXT"))
  (progn
	(progn
		(setq Ten_TIM (cdr (assoc 10 (entget (car D_TIM)))))
	  	(setq Name_TIM (cdr (assoc 1 (entget (car D_TIM)))))
		(setq Pnt_TIM (TD:Text-Base  (car D_TIM)))
	)
	(Alert "\n Qu\U+00E9t ch\U+1ECDn Text \U+0111\U+1EC3 xu\U+1EA5t kho\U+1EA3ng c\U+00E1ch t\U+1EEB tim tuy\U+1EBFn")
	(Prompt "\n Qu\U+00E9t ch\U+1ECDn Text \U+0111\U+1EC3 xu\U+1EA5t kho\U+1EA3ng c\U+00E1ch t\U+1EEB tim tuy\U+1EBFn")
	(setq ss1 (ssget (list (cons 0  "TEXT"))))
	(command "_.select" ss1 "r" D_TIM "")
	(setq ss2A (ssget "P"))
	(setq ss2 (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex ss2A))))
	(progn
		(foreach item ss2
			(progn
				(setq temp  (entget item)
				    ND_text (cdr (assoc 1 temp))
				        Tdo (TD:Text-Base item )
				      Pnt_ct   (list (car Tdo)(cadr Tdo))
				 )
			  	(setq KCBC  (atof (rtos (distance Pnt_TIM Pnt_ct) 2 3)))
			  	(setq Lst1 (list KCBC ND_text))
			  	(setq Lst2 (append Lst2 (list Lst1)))
				
			 )
		)
	  	(setq Lst3a (vl-sort Lst2 '(lambda (e1 e2) (< (car e1) (car e2)))))
;;;	  	(setq Lst3  (append (list (list 0 Name_TIM )) Lst3a))
		(if (vlax-get-or-create-object "Excel.Application")
			(WriteToExcel Lst3a)
			(WriteToCSV Lst3a)
		)
	)
   )
(alert (strcat "\n Ch\U+01B0a ch\U+1ECDn Text tim tuy\U+1EBFn ho?c ch\U+01B0a ch\U+1ECDn \U+0111\U+00FAng \U+0111\U+1ED1i t\U+01B0\U+1EE3ng l\U+00E0 Text"))
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




