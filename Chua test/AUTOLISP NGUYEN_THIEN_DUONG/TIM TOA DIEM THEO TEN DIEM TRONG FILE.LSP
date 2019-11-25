;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh t\U+00ECm t\U+1ECDa \U+0111\U+1ED9 \U+0111i\U+1EC3m theo file t\U+00EAn \U+0111i\U+1EC3m"
;;;	       "\nL\U+1EC7nh s\U+1EED d\U+1EE5ng: TIMDIEM"
;;;	       "\n"
;;;	       "\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;	       "\nEmail: heaven2407@gmail.com"
;;;	       "\nMobile: 0972.0168.25"
;;;	       "\n" "\n" "\n"
;;;		
;;;	)
;;;)
;;;(Alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh t\U+00ECm t\U+1ECDa \U+0111\U+1ED9 \U+0111i\U+1EC3m theo file t\U+00EAn \U+0111i\U+1EC3m"
;;;	       "\nL\U+1EC7nh s\U+1EED d\U+1EE5ng: TIMDIEM"
;;;	       "\n"
;;;	       "\nNg\U+01B0\U+1EDDi vi\U+1EBFt: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;	       "\nEmail: heaven2407@gmail.com"
;;;	       "\nMobile: 0972.0168.25"
;;;	       "\n" "\n" "\n"
;;;		
;;;	)
;;;)
(defun c:TIMDIEM ( /	 DATA	F      I      L1     L1A    L2A
		         L2B	 L3	L4     LINE   LOOP   NAME   NAMEPNT
		         PNT	 SSGTEXT       TEN
		 )
  (defun *error* (msg)
    (if	Olmode
      (setvar 'osmode Olmode)
    )
    (if	(not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setq ssgText (acet-ss-to-list (ssget (list (cons 0 "TEXT")))))
  (setq L2a (list))
  (foreach e1 ssgText
    (setq L1a (list))
    (setq Name (cdr (assoc 1 (entget e1))))
    (setq Pnt (TD:Text-Base e1))
    (setq L1a (list Name Pnt))
    (setq L2a (append L2a (list L1a)))
  )
  (setq L2b (FilterName (sortAlPhabyList L2a)))

  (setq l1 (list))
  (if (setq ten (getfiled "Chon File" (getvar "dwgprefix") "*" 8))
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
	      (setq NAMEPNT (nth 0 data))
	      (setq l1 (append l1 (list NAMEPNT)))
	    )
	 )
      )
    )
  )

  (setq l4 (list))
  (foreach e2 l1
    (setq i 0)
    (setq loop T)
    (setq l3 (list))
    (while (< i (length L2b))
      (setq TD_XY (cadr (nth i L2b)))
      (setq NDT (car (nth i L2b)))
      (if (equal e2 NDT)
	(progn
	  (setq l3 (list e2 (car TD_XY) (cadr TD_XY)))
	  (setq loop nil)
	)

      )
      (setq i (1+ i))
    )
    (if	(not l3)
      (setq l3 (list e2 "Not Found"))
    )
    (setq l4 (append l4 (list l3)))
  )



  (if (vlax-get-or-create-object "Excel.Application")
    (WriteToExcel l4)
    (WriteToCSV l4)
  )
  (princ)
)

(defun sortAlPhabyList (listpp /)
  (setq	return (vl-sort	listpp
			(function
			  (lambda (e1 e2)
			    (< (car e1) (car e2))
			  )
			)
	       )
  )
  return
)




(defun my_sort (lst / foo)

  (defun foo (str / l1 l2)
    (setq l1 (vl-string->list str))
    (while (< 47 (car l1) 58)
      (setq l2 (cons (car l1) l2)
	    l1 (cdr l1)
      )
    )
    (vl-list->string (reverse l2))
  )

  (vl-sort lst
	   '(lambda (x1 x2 / n1 n2)
	      (if (and (/= (setq n1 (foo x1)) "")
		       (/= (setq n2 (foo x2)) "")
		       (/= n1 n2)
		  )
		(< (atoi n1) (atoi n2))
		(< x1 x2)
	      )
	    )
  )
)

(defun FilterName (l)
  (if l
    (cons (car l)
	  (FilterName
	    (vl-remove-if
	      '(lambda (x) (equal (car x) (car (car l))))
	      (cdr l)
	    )
	  )
    )
  )
)



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