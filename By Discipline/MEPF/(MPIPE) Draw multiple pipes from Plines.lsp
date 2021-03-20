(defun c:mpipe(/ actDoc Ang1 Ang2 enDist stDist lEnt
       lObj lPln oldVars oldWd plEnd plLst
       plSet plStart1 plStart2 prDir ptLst
       segLst Start stLst tAng vlaPln)
 
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
   '(70 . 272)'(3 . "")'(62 . 256)'(51 . 1.5708)'(52 . 1.5708)
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


 (defun *error*(msg)
   (if oldVars
     (mapcar 'setvar
     '("CMDECHO" "OSMODE" "PLINEWID")
     oldVars); end mapcar
    ); end if
   (if stLst
     (asmi-LayersStateRestore stLst)
     ); end if
    (if actDoc
     (vla-EndUndoMark actDoc)
     ); end if
   (princ)
   ); end of *error*

 (PipeMLineStyle)
 
(if(not mpipepWd)(setq mpipepWd 1.0))
  (setq oldWd mpipepWd
               mpipepWd(getdist
                         (strcat "\nSpecify duct pipe width <" (rtos mpipepWd) ">: "))
 ); end setq
(if(null mpipepWd)(setq mpipepWd oldWd))

 (vla-StartUndoMark
  (setq actDoc
   (vla-get-ActiveDocument
     (vlax-get-acad-object))))
 (setq oldVars(mapcar 'getvar '("CMDECHO" "OSMODE" "PLINEWID")))
(princ "\n>>> Select polylines to transform to duct pipes <<< ")

 (setq plSet
 (ssget '((0 . "LWPOLYLINE")))); end setq
 (if plSet
   (progn
(setq stLst(asmi-LayersUnlock)
 	      plLst(vl-remove-if 'listp 
                (mapcar 'cadr(ssnamex plSet)))
 ); end setq
 (setvar "OSMODE" 0)
 (setvar "CMDECHO" 0)
 (foreach pl plLst
   (setq segLst
   (asmi-PlineSegmentDataList pl)
  vlaPln(vlax-ename->vla-object pl)
  ); end setq
 (while (/= 1(length segLst))
   (setq stDist
      (vlax-curve-getDistAtPoint vlaPln
	(caar segLst))
  enDist
      (vlax-curve-getDistAtPoint vlaPln
	(caadr segLst))
     ); end setq
   (if(< 2(length segLst))
      (setq ang1
      (+(/ pi 2)(angle(caar segLst)(caadr segLst)))
     ang2
      (+(/ pi 2)(angle(caadr segLst)(car(nth 2 segLst))))
     ); end setq
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
	 (+ stDist mpipepWd))); end setq
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
	    (- enDist mpipepWd))
	prDir nil); end setq
     ); end if
   (if
     (< 2(length segLst))
      (setq plStart2
       (vlax-curve-getPointAtDist vlaPln
	 (+ enDist mpipepWd))
     ptLst
                    (mapcar
	      '(lambda(x)(trans x 0 1)); end lambda
                (list(polar plEnd ang1 (/ mpipepWd 2))
		(polar plEnd (+ pi ang1)(/ mpipepWd 2))
		(polar plStart2 (+ pi ang2)(/ mpipepWd 2))
		(polar plStart2 ang2 (/ mpipepWd 2))
	      	); end list
	    ); end mapcar
	); end setq
 ); end if
     (setq plStart1(trans plStart1 0 1)
      plEnd(trans plEnd 0 1)
  ); end setq
   	(if plStart2
     		(setq plStart2(trans plStart1 0 1))
     	); end if
      (if
   (not(equal ang1 ang2 0.000001))
      		(progn
	       	(setvar "PLINEWID" 0.0)
	       	(command "_.pline")
		(mapcar 'command ptLst)(command "_c")
	       	(setvar "PLINEWID" mpipepWd)
 	); end progn
 ); end if
   (if
     (not(equal ang1 ang2 0.000001))
     (progn
      	(setq lPln
      (vlax-ename->vla-object(entlast))
     tAng(- ang2 ang1)
     ); end setq
       (if(minusp tAng)(setq tAng(- tAng)))
        (if
	  (and
	   (< 0 tAng)
	   (>= pi tAng)
	   ); end and
	 (progn
       		(vla-SetBulge lPln 1 (/(- ang2 ang1)4))
       		(vla-SetBulge lPln 3 (/(- ang1 ang2)4))
         ); end progn
	 (progn
       		(vla-SetBulge lPln 1(/(- ang1 ang2)12))
       		(vla-SetBulge lPln 3(/(- ang2 ang1)12))
	   ); end progn
	 ); end if
      ); end progn
 ); end if

   	 (command "_.mline" "_st" "DUCT_PIPE"
	"_S" mpipepWd "_J" "_Z"
	plStart1 plEnd "")
   
   (setq segLst(cdr segLst)); end setq
   ); end while
   (vla-Delete vlaPln)
   ); end foreach
     (vla-EndUndoMark actDoc)
     (asmi-LayersStateRestore stLst)
      ); end progn
     ); end if
(mapcar 'setvar
     '("CMDECHO" "OSMODE" "PLINEWID")
     oldVars); end apply
 (princ)
 ); end of c:mpipe