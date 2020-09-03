;|						;;
	TAPER TRANSITION - 2008-10-18		;;
	Par Andrea Andreetti			;;
	rel. 17.361				;;
						|;
						;;
(vl-load-com)

(defun c:taper () (taper nil))

(defun taper (val / gdc2 gdc2demi l1 l2 entl1 entl2 l1a l1b l2a l2b)
  
  (setq gdc2 (getreal "Duct size : "))
  (setq gdc2demi (/ gdc2 2.0))
  (setq l1 (nentsel "Select first line..."))
  (setq l2 (nentsel "Parallel line..."))
  (setq entl1 (entget (car l1)))
  (setq entl2 (entget (car l2)))
  (setq l1a (cdr (assoc 10 (entget (car l1)))))
  (setq l1b (cdr (assoc 11 (entget (car l1)))))
  (setq l2a (cdr (assoc 10 (entget (car l2)))))
  (setq l2b (cdr (assoc 11 (entget (car l2)))))
  (if (< (distance (cadr l1) l1a) (distance (cadr l1) l1b))
    (progn (setq l1 l1a) (setq ductway (angle l1b l1a)))
    (progn (setq l1 l1b) (setq ductway (angle l1a l1b)))
  )
  (setq l2 (polar l1 (+ (dtr 90) (angle l1a l1b)) 5))
  (setq il1 (inters l1 l2 l1a l1b nil))
  (setq il2 (inters l1 l2 l2a l2b nil))
  (if (< (distance il2 (cdr (assoc 10 entl2)))
         (distance il2 (cdr (assoc 11 entl2)))
      )
    (progn (setq toreplace (assoc 10 entl2))
           (setq bythis (cons 10 il2))
    )
    (progn (setq toreplace (assoc 11 entl2))
           (setq bythis (cons 11 il2))
    )
  )
  (entmod (subst bythis toreplace entl2))
  (setq lunit (getvar "lunits"))
  (if (< lunit 3)
    (setq 6po 150)
    (setq 6po 3)
  )
  (setq gdc1 (distance il1 il2))
  (if (< (abs (* 3.0 (- gdc1 gdc2))) 6po)
    (setq len 6po)
    (setq len (abs (* 3.0 (- gdc1 gdc2))))
  )
  
  (setq mid1 (polar il1 (angle il1 il2) (/ (distance il1 il2) 2.0)))
  (setq mid2 (polar mid1 ductway len))
  (setq midl2 (polar mid2 (angle il1 il2) gdc2demi))
  (setq midl1 (polar mid2 (angle il2 il1) gdc2demi))
  (setq fixmid2 mid2)
  (setq midlist (list mid2 midl2 midl1))
  
  (setq midl1 (polar il1 ductway len))
  (setq midl2 (polar midl1 (angle il1 il2) gdc2))
  (setq mid2 (polar midl1 (angle il1 il2) gdc2demi))
  (setq fixmidl2 midl2)
  (setq l1list (list mid2 midl2 midl1))
  
  (setq midl2 (polar il2 ductway len))
  (setq midl1 (polar midl2 (angle il2 il1) gdc2))
  (setq mid2 (polar midl2 (angle il2 il1) gdc2demi))
  (setq fixmidl1 midl1)
  (setq l2list (list mid2 midl2 midl1))

  
  (while (or (= (car (setq grr (grread t 5 0))) 5)
             (= (car (setq grr (grread t 5 0))) 2)
         )
    (redraw)
    (if (< (distance (cadr grr) fixmid2)
           (distance (cadr grr) fixmidl2)
        )
      (setq goodlist midlist)
    )
    (if (< (distance (cadr grr) fixmidl2)
           (distance (cadr grr) fixmid2)
        )
      (setq goodlist l2list)
    )
    (if (< (distance (cadr grr) fixmidl1)
           (distance (cadr grr) fixmid2)
        )
      (setq goodlist l1list)
    )
    (grdraw (nth 1 goodlist) (nth 2 goodlist) 147 0)
    (grdraw (nth 1 goodlist) il2 147 0)
    (grdraw (nth 2 goodlist) il1 147 0)
    (grdraw il1 il2 147 0)
    (setq intp2t (polar (nth 0 goodlist)
                        ductway
                        (distance fixmid2 (cadr grr))
                 )
    )
    (setq intp2z (polar (cadr grr) (angle midl1 midl2) gdc2demi))
    (setq p2 (inters (nth 0 goodlist) intp2t (cadr grr) intp2z nil))
    (setq p21 (polar p2 (angle midl2 midl1) gdc2demi))
    (setq p22 (polar p2 (angle midl1 midl2) gdc2demi))
    (grdraw (nth 1 goodlist) p22 147 0)
    (grdraw (nth 2 goodlist) p21 147 0)

(if (= val "S2RT")
  (progn
    (grdraw il1 (nth 0 goodlist) 1 1)
    (grdraw il2 (nth 0 goodlist) 1 1)
      (setq Jonc1  (polar (nth 1 goodlist) ductway 6po))
      (setq Jonc2  (polar (nth 2 goodlist) ductway 6po))
    (grdraw jonc1 jonc2 8 1)
  )
)

    
(if (= val "R2ST")
  (progn
    (grdraw mid1 (nth 1 goodlist) 1 1)
    (grdraw mid1 (nth 2 goodlist) 1 1)
      (setq Jonc1  (polar il1 (+ (dtr 180) ductway) 6po))
      (setq Jonc2  (polar il2 (+ (dtr 180) ductway) 6po))
    (grdraw jonc1 jonc2 8 1)
  )
)

(if (= val "R2RT")
  (progn    
      (setq Jonc1  (polar il1 (+ (dtr 180) ductway) 6po))
      (setq Jonc2  (polar il2 (+ (dtr 180) ductway) 6po))
    (grdraw jonc1 jonc2 8 1)
      (setq Jonc1  (polar (nth 1 goodlist) ductway 6po))
      (setq Jonc2  (polar (nth 2 goodlist) ductway 6po))
    (grdraw jonc1 jonc2 8 1)
  )
)    

    
  )
  (taperentmake (cons 10 (nth 1 goodlist))
                (cons 11 (nth 2 goodlist))
                nil
                nil
  )
  (taperentmake (cons 10 il2)
                (cons 11 (nth 1 goodlist))
                nil
                nil
  )
  (taperentmake (cons 10 il1)
                (cons 11 (nth 2 goodlist))
                nil
                nil
  )
  (taperentmake (cons 10 il1) (cons 11 il2) nil nil)
  (taperentmake (cons 10 (nth 1 goodlist))
                (cons 11 p22)
                nil
                nil
  )
  (taperentmake (cons 10 (nth 2 goodlist))
                (cons 11 p21)
                nil
                nil
  )
  
  (redraw)
)
						;;
;|						|;







;|						;;
	Degre To Radian				;;
						|;
						;;
(defun dtr (a)
  (* pi (/ a 180.0))
)
						;;
;|						|;







;|						;;
	Square To Round TRANSITION		;;
						|;
						;;
(defun c:s2r (/ Jonc1 Jonc2 S2RT)
(resetTAPERvar)
  (taper "S2RT")
  (setq Jonc1  (polar (nth 1 goodlist) ductway 6po))
  (setq Jonc2  (polar (nth 2 goodlist) ductway 6po))
  (Jonction_create Jonc1 Jonc2)
  
  (taperentmake (cons 10 il1)
                (cons 11 (nth 0 goodlist))
                1
                nil
  )
  (taperentmake (cons 10 il2)
                (cons 11 (nth 0 goodlist))
                1
                nil
  )
  (setq ductway nil
	6po nil)
(princ)
)
						;;
;|						|;




(defun resetTAPERvar ()
  (setq S2RT Nil
	R2ST Nil
	R2RT Nil)
)





;|						;;
	Round To Square TRANSITION		;;
						|;
						;;
(defun c:r2s (/ jonc1 jonc2 R2ST)
(resetTAPERvar)
 (taper "R2ST")
 (setq Jonc1  (polar il1 (+ (dtr 180) ductway) 6po))
 (setq Jonc2  (polar il2 (+ (dtr 180) ductway) 6po))
 (Jonction_create Jonc1 Jonc2)
  
  (taperentmake (cons 10 mid1)
                (cons 11 (nth 1 goodlist))
                1
                nil
  )
  (taperentmake (cons 10 mid1)
                (cons 11 (nth 2 goodlist))
                1
                nil
  )
  (setq ductway nil
	6po nil)
  (princ)
)
						;;
;|						|;








;|						;;
	Round To Square TRANSITION		;;
						|;
						;;
(defun c:r2r (/ jonc1 jonc2 R2RT)
(resetTAPERvar)
(taper "R2RT")
(setq Jonc1  (polar il1 (+ (dtr 180) ductway) 6po))
(setq Jonc2  (polar il2 (+ (dtr 180) ductway) 6po))
(Jonction_create Jonc1 Jonc2)

(setq Jonc1  (polar (nth 1 goodlist) ductway 6po))
(setq Jonc2  (polar (nth 2 goodlist) ductway 6po))
(Jonction_create Jonc1 Jonc2)
  (setq ductway nil
	6po nil)
  (princ)
)
						;;
;|						|;









;|						;;
	ENTMAKE FUNCTION			;;
						|;
						;;
(defun jonction_create (J1 J2)
;;	Assuming that ACAD.LIN Exist and contain "ACAD_ISO03W100" Linetype
(if (not (member "ACAD_ISO03W100" (mapcar 'strcase (ai_table "LTYPE" 0))))
  (if (findfile "acad.lin")
  (vl-cmdf "._-linetype" "_L" "ACAD_ISO03W100" (findfile "acad.lin") "")
))
  
  (taperentmake (cons 10 J1)
                (cons 11 J2)
                8
                "ACAD_ISO03W100"
  )
)
						;;
;|						|;






;|						;;
	ENTMAKE FUNCTION			;;
						|;
						;;
(defun taperentmake (start end color linetype / lllist)
  (setq lllist (list '(0 . "LINE") start end))
  (if color
    (setq lllist (append lllist (list (cons 62 color))))
  )
  (if linetype
    (setq lllist (append lllist (list (cons 6 linetype))))
  )
  (entmake lllist)
)
						;;
;|						|;



(princ "\nTAPER - By Andrea Andreetti
commands: TAPER,R2S,S2R and R2R    - activated.\n")