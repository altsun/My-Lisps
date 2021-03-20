(defun c:DUCT(/ actDoc ang1 ang2 ang3 ptLst enDist
	       fPt lEnt lObj lPln oldVars oldWd
	       plEnd plStart1 plStart2 prDir
	       segLst Start stDist stLst tAng
	       vlaPln cFlg *error*)
  
  (vl-load-com)

  (defun GetPlineVer(plObj)
    (mapcar 'cdr
     (vl-remove-if-not
      '(lambda(x)(=(car x)10))
      (entget plObj)))
    ); end of GetPLineVer

  (defun asmi-PlineSegmentDataList(plObj / cLst outLst)
   (setq cLst
    (vl-remove-if-not
      '(lambda(x)(member(car x) '(10 40 41 42)))
      (entget plObj))
   	 outLst '()
    ); end setq
  (while cLst
    (if(assoc 40 cLst)
      (progn
       (setq outLst
	     (append outLst
	      (list
		(list
		  (cdr(assoc 10 cLst))
		  (cdr(assoc 40 cLst))
		  (cdr(assoc 41 cLst))
		  (cdr(assoc 42 cLst))
		 ); end list
	); end list
      ); end if
); end setq
       (repeat 4
 (setq cLst(cdr cLst))
 ); end repeat
       ); end progn
      (setq outLst
	     (append outLst
	     (list
	       (list
	 	(cdr(assoc 10 cLst))
		); end list
	       ); end list
     ); end append
    cLst nil
    ); end setq
      ); end if
    ); end while
  outLst
    ); end of asmi-GetPlineSegmentData


  (defun asmi-LayersUnlock(/ restLst)
  (setq restLst '())
  (vlax-for lay
    (vla-get-Layers
             (vla-get-ActiveDocument
               (vlax-get-acad-object)))
    (setq restLst
     (append restLst
       (list
         (list
          lay
           (vla-get-Lock lay)
   (vla-get-Freeze lay)
          ); end list
         ); end list
       ); end append
    ); end setq
    (vla-put-Lock lay :vlax-false)
    (if
      (vl-catch-all-error-p
(vl-catch-all-apply
  'vla-put-Freeze(list lay :vlax-false)))
      t)
    ); end vlax-for
  restLst
  ); end of asmi-LayersUnlock

  (defun asmi-LayersStateRestore(StateList)
  (foreach lay StateList
    (vla-put-Lock(car lay)(cadr lay))
     (if
      (vl-catch-all-error-p
(vl-catch-all-apply
  'vla-put-Freeze(list(car lay)(nth 2 lay))))
      t)
    ); end foreach
  (princ)
     ); end of asmi-LayersStateRestore

  (defun PipeMLineStyle(/ dxfLst mlDict)
  (setq dxfLst
   (list'(0 . "MLINESTYLE")'(102 . "{ACAD_REACTORS")'(102 . "}")
    '(100 . "AcDbMlineStyle") '(2 . "DUCT_PIPE")
    '(70 . 274)'(3 . "")'(62 . 256)'(51 . 1.5708)'(52 . 1.5708)
    '(71 . 2)'(49 . 0.5)'(62 . 256)'(6 . "BYBLOCK")
    '(49 . -0.5)'(62 . 256)'(6 . "BYBLOCK"))); end setq
    (if
     (null
      (member
       (assoc 2 dxfLst)
         (dictsearch
   (namedobjdict)
   "ACAD_MLINESTYLE")))
    (progn
      (setq mlDict
       (cdr
         (assoc -1
   (dictsearch
     (namedobjdict)
     "ACAD_MLINESTYLE"))))
      (dictadd mlDict
         (cdr(assoc 2 dxfLst))(entmakex dxfLst))
      ); end progn
    ); end if
); end of PipeMLineStyle

(defun SideCalculate(Wdth Ang / Rad)
  (setq Ang(- pi Ang))
  (setq Rad(* (if (= dpipeelb "Mitered") 0.5 dpiper/w) Wdth))
  (+ (if (= dpipeelb "Mitered") dpipetan  0)
   (*
    (/
      (sqrt(-(* 2(expt Rad 2))(* 2(expt Rad 2)(cos Ang))))
      (sin(- pi Ang)))(sin(/(- pi(- pi Ang))2.0)
     )
    )
   )
  ); end of SideCalculate


  (defun BodyFunction()
  (if
    (not
      (equal lObj(entlast)))
(progn
  (setq lEnt(entlast)
        stLst(asmi-LayersUnlock)
  	segLst(asmi-PlineSegmentDataList lEnt)
  	vlaPln(vlax-ename->vla-object lEnt)
  ); end setq
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (while (/= 1(length segLst))
       (setq stDist
	      (vlax-curve-getDistAtPoint vlaPln
		(caar segLst))
	     enDist
	      (vlax-curve-getDistAtPoint vlaPln
		(caadr segLst))
	     ); end setq
    (if(< 2(length segLst))
      (progn
       (setq ang1
	      (+(/ pi 2)(angle(caar segLst)(caadr segLst)))
	     ang2
	      (+(/ pi 2)(angle(caadr segLst)(car(nth 2 segLst))))
	     ); end setq
       ); end progn
      ); end if
    (if
      (or
	(not Start)
	prDir
	);end or
       (setq plStart1
	       (vlax-curve-getPointAtDist vlaPln
		 stDist)
	     Start T); end setq
       (setq plStart1
	       (vlax-curve-getPointAtDist vlaPln
		 (+ stDist(SideCalculate(cadar segLst)ang3)))); end setq
      ); end if
    (if(and ang1 ang2)
      (progn
      (if(> ang1 ang2)
	 (setq ang3(- ang1 ang2))
	 (setq ang3(- ang2 ang1))
	 ); end if
       (setq ang3(- pi ang3)
	     tAng ang3)
       (if(minusp ang3)(setq ang3(- ang3)))
       ); end progn
      ); end if

    (if
      (or
        (equal ang1 ang2 0.000001)
	(= 2(length segLst))
       ); end or
      	  (setq plEnd
	           (vlax-curve-getPointAtDist vlaPln
		   enDist)
		prDir T); end setq
          (setq plEnd
	           (vlax-curve-getPointAtDist vlaPln
		    (- enDist(SideCalculate(cadar segLst)ang3)))
		prDir nil); end setq
      ); end if
    (if
      (< 2(length segLst))
       (setq plStart2
	       (vlax-curve-getPointAtDist vlaPln
		 (+ enDist(SideCalculate(cadar segLst)ang3)))); end setq
	     ); end if
       (if(< 2(length segLst))
	       (if
		 (=(cadar segLst)(nth 2(car segLst)))
		  (setq ptLst
                     (mapcar
		      '(lambda(x)(trans x 0 1)); end lambda
                          (append
                             (if (not (/= "Segmented" dpipeelb "Mitered"))
                               (progn
                                  (setq ang4 (apply '(lambda(x)(atan x (sqrt (abs (1- (* x x))))))
                                    (list (sin (- ang1 (/ pi 2.0) (angle plEnd plStart2)))))
                                    )
                                  (setq SegNum (cond ((or (= dpipeelb "Mitered") (< (abs ang4)
                                    (* (/ 35 360.0) pi))) 2) ((< (abs ang4) (* (/ 55 360.0) pi)) 3)
                                    ((< (abs ang4) (* (/ 75 360.0) pi)) 4) (T 5))
                                    )
                                  (setq tan4 (+ (if (= dpipeelb "Mitered") dpipetan 0.0) (*
                                    (if (= dpipeelb "Mitered") 0.5 dpiper/w) (cadar segLst)
                                    (abs (apply '(lambda(x) (/ (sin x) (cos x))) (list (/ ang4
                                    0.5 (1- SegNum) 2.0))))))
                                    )
                                  (setq mllst (list plEnd (polar plEnd (- ang1 (/ pi 2.0)) tan4)))
                                  (setq SegCnt 0)
                                  (while (< (+ SegCnt 2) SegNum)
                                    (setq mllst (append mllst (list (polar (last mllst)
                                                (+ (angle (cadr (reverse mllst)) (last mllst))
                                                (/ ang4 -0.5 (1- SegNum))) (* tan4 2.0)))
                                                )
                                          SegCnt (1+ SegCnt)
                                          )
                                       )
                                  (setq mllst (append mllst (list PlStart2)))
                                  (setq SegCnt (- (length mllst) 2))
                                  (setq pllst nil)
                                  (while (> SegCnt 0)
                                    (setq pllst (append pllst (list (polar (nth SegCnt mllst)
                                                (+ (angle (nth (1- SegCnt) mllst) (nth SegCnt mllst))
                                                (/ pi 2.0) (/ ang4 0.5 (1- SegNum) -2.0))
                                                (/(cadar segLst)2(cos (/ ang4 0.5 (1- SegNum) 2.0)))))
                                                )
                                          SegCnt (1- SegCnt)
                                          )
                                       )
                                  pllst
                                  )
                                )
	                     (list(polar plEnd ang1 (/(cadar segLst)2)))
			     (list(polar plEnd (+ pi ang1)(/(cadar segLst)2)))
                             (if (not (/= "Segmented" dpipeelb "Mitered"))
                               (progn
                                  (setq SegCnt 1)
                                  (setq pllst nil)
                                  (while (< SegCnt (1- (length mllst)))
                                    (setq pllst (append pllst (list (polar (nth SegCnt mllst)
                                                (+ (angle (nth (1- SegCnt) mllst) (nth SegCnt mllst))
                                                (* pi 1.5) (/ ang4 0.5 (1- SegNum) -2.0))
                                                (/(cadar segLst)2(cos (/ ang4 0.5 (1- SegNum) 2.0)))))
                                                )
                                          SegCnt (1+ SegCnt)
                                          )
                                       )
                                 (setq mllst (mapcar '(lambda(x)(trans x 0 1)) mllst))
                                 pllst
                                 )
                               )
			     (list(polar plStart2 (+ pi ang2)(/(cadar segLst)2)))
			     (list(polar plStart2 ang2 (/(cadar segLst)2)))
		      	); end append
		          ); end mapcar
			); end setq
		 (setq ptLst
		   (mapcar
		    '(lambda(x)(trans x 0 1)); end lambda
		       (list (polar plStart1 ang1 (/(cadar segLst)2))
			     (polar plStart1 (+ pi ang1)(/(cadar segLst)2))
			     (polar(caadr segLst)(+ pi ang2)(/(nth 2(car segLst))2))
			     (polar(caadr segLst)ang2(/(nth 2(car segLst))2))
			     ); end list
		          ); end mapcar
			); end setq
	       ); end if
	 ); end if
      (setq plStart1(trans plStart1 0 1)
	      plEnd(trans plEnd 0 1)
	  ); end setq
    	(if plStart2
      		(setq plStart2(trans plStart1 0 1))
      	); end if
       (if (< 2(length segLst))
	  (if (or (/=(cadar segLst)(nth 2(car segLst)))
	        (and (/= "Segmented" dpipeelb) (not(equal ang1 ang2 0.000001))
                ); end and
               ); end or
             (progn
	       	(setvar "PLINEWID" 0.0)
	       	(command "_.pline")
		(mapcar 'command ptLst)(command "_c")
	       	(setvar "PLINEWID" dpipepWd)
	        ); end progn
	     (if (and (= "Segmented" dpipeelb) (not(equal ang1 ang2 0.000001)))
                (progn
                   (command "_.mline" "_st" "DUCT_PIPE" "_S" (cadar segLst) "_J" "_Z")
                   (mapcar 'command mlLst)
                   (command "")
;                   (setvar "PLINEWID" 0.0)
;                   (command "_.pline")
;                   (mapcar 'command ptLst)(command "_c")
;                   (setq SegCnt 0)
;                   (while (< SegCnt (1- SegNum))
;                      (command "_.line" (nth SegCnt ptlst)
;                         (nth (- (length ptlst) 3 SegCnt) ptlst)
;                         ""
;                         )
;                      (setq SegCnt (1+ SegCnt))
;                      )
;                   (setvar "PLINEWID" dpipepWd)
                    ); end progn
                 ); end if
	     ); end if
	 ); end if
    (if
      (and (= dpipeelb "Radius")
        (not(equal ang1 ang2 0.000001))
        (< 2(length segLst))
      ); end and
      (progn
       	(setq lPln
	      (vlax-ename->vla-object(entlast))
	      tAng (abs (- ang2 ang1))
	     ); end setq
	(if (> tAng pi)
	   (if(< ang1 ang2)
	     (setq ang1(+ ang1 pi)
		   ang2(- ang2 pi)); end setq
	     (setq ang1(- ang1 pi)
		   ang2(+ ang2 pi)); end setq
	     ); end if
           ); end if
	(setq Bulge(/(sin(/(rem(- ang2 ang1)pi)4.0))(cos(/(rem(- ang2 ang1)pi)4.0))))
	(vla-SetBulge lPln 1 Bulge)
	(vla-SetBulge lPln 3 (- Bulge))
       ); end progn
	 ); end if
  (if
    (=(cadar segLst)(nth 2(car segLst)))
    	 (command "_.mline" "_st" "DUCT_PIPE"
		"_S" (cadar segLst) "_J" "_Z"
		plStart1 plEnd "")
;       (progn
;		(setq ptLst
;			(list (polar plStart1 (+ (angle plStart1 plEnd) (/ pi 2.0)) (/(cadar segLst)2))
;			     (polar plStart1 (- (angle plStart1 plEnd) (/ pi 2.0)) (/(cadar segLst)2))
;			     (polar plEnd (- (angle plStart1 plEnd) (/ pi 2.0)) (/(nth 2(car segLst))2))
;			     (polar plEnd (+ (angle plStart1 plEnd) (/ pi 2.0)) (/(nth 2(car segLst))2))
;			     )
;			)
;	       	(setvar "PLINEWID" 0.0)
;	       	(command "_.pline")
;		(mapcar 'command ptLst)(command "_c")
;	       	(setvar "PLINEWID" dpipepWd)
;	); end progn
    ); end if
    
    (setq segLst(cdr segLst)); end setq
    ); end while
  (command "_.erase" lEnt "")
  (asmi-LayersStateRestore stLst)
  ); end progn
    ); end if
    ); end of Body Function

    (defun *error*(msg)
     (if actDoc
      (vla-EndUndoMark actDoc)
      ); end if
     (setvar "CMDECHO" 0)
     (command "_.undo" "1")
     (if oldVars
      (mapcar 'setvar
	     '("FILLMODE" "PLINEWID" "CMDECHO" "OSMODE")
	     oldVars); end mapcar
     ); end if
     (if (not (member msg '("console break" "Function cancelled" "quit / exit abort" "")))
        (princ (strcat "\nError: " msg))
        (princ)
        )
    ); end of *error*

  (PipeMLineStyle)
  
  (if(not dpipepWd)(setq dpipepWd 1.0))
  (setq oldWd dpipepWd
	oldVars(mapcar 'getvar '("FILLMODE" "PLINEWID" "CMDECHO" "OSMODE"))
        ); end setq
  (if(entlast)(setq lObj(entlast)))
  (vla-StartUndoMark
   (setq actDoc
    (vla-get-ActiveDocument
      (vlax-get-acad-object))))
  (setq dlastelb dpipeelb)
  (initget "Mitered Radius Segmented")
  (setq dpipeelb (getkword (strcat "\nSpecify elbow type "
	(if (= dlastelb "Mitered") "<Mitered>/" "Mitered/")
	(if (= dlastelb "Segmented") "<Segmented>/" "Segmented/")
	(if (/= "Mitered" dlastelb "Segmented")
            (strcat "<" (setq dlastelb "Radius") ">: ")
            "Radius: "))
            )
	)
  (if (or (= dpipeelb "Mitered") (and (not dpipeelb) (= dlastelb "Mitered")))
      (progn
         (initget 6)
         (setq dlasttan dpipetan
               dpipetan (getdist (strcat "\nNew elbow throat length <"
                   (rtos (if (/= (type dlasttan) 'REAL)
                   (setq dlasttan 6.0) dlasttan)) ">: ")
                   )
              )
	(if (not dpipetan) (setq dpipetan dlasttan))
	(if (not dpipeelb) (setq dpipeelb dlastelb))
	(setq dlasttan nil)
	)
      (progn
         (initget 6)
         (setq dlastr/w dpiper/w
               dpiper/w (getdist (strcat "\nNew centerline r/w factor <"
                   (rtos (if (/= (type dlastr/w) 'REAL)
                   (setq dlastr/w 1.5) dlastr/w)) ">: ")
                   )
              )
	(if (not dpiper/w) (setq dpiper/w dlastr/w))
	(if (not dpipeelb) (setq dpipeelb dlastelb))
	(setq dlastr/w nil dpiper/w (max dpiper/w 0.5))
	)
     )
  (initget 128)
  (while(not cFlg)
   (setq fPt
	 (getpoint
	   (strcat
	     "\nSpecify start point or width <"
	     (rtos dpipepWd) ">: " ))); end setq
    (cond
      ((= 'LIST(type fPt))
       (setq cFlg T)
       ); end condition #1
      ((= 'REAL(type(distof fPt)))
       (setq dpipepWd(distof fPt)); end setq
       ); end condition #2
      (T
       (princ "\nInvalid option keyword! ")
       ); end condition #3
      ); end cond
    ); end while
    (mapcar 'setvar
	 '("FILLMODE" "PLINEWID" "CMDECHO")
	 (list 0 dpipepWd 0)); end mapcar
  (command "_.pline" fPt)
  (setvar "CMDECHO" 1)
  (while(= 1(getvar "CMDACTIVE"))
    (command pause)
    ); end while
  (setq dpipepwd (getvar "PLINEWID"))
  (BodyFunction)
  (vla-EndUndoMark actDoc)
(mapcar 'setvar
	     '("FILLMODE" "PLINEWID" "CMDECHO" "OSMODE")
	     oldVars); end apply
  (princ)
  ); end of c:DUCT