(defun c:plp(/ lSet plLst pl pl1 pl2 oldOsm
      actDoc vLst1 vLst2 stLst *error*)
 
 (vl-load-com)

 (defun GetPlineVer(plObj)
   (mapcar 'cdr
    (vl-remove-if-not
     '(lambda(x)(=(car x)10))
     (entget plObj)))
   ); end of GetPLineVer

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

 (if(not duct:pWd)(setq duct:pWd 1.0))
 (setq oldWd duct:pWd
duct:pWd(getdist
     (strcat "\nSpecify pipes diameter <" (rtos duct:pWd) ">: "))
); end setq
 (if(null duct:pWd)(setq duct:pWd oldWd))
(princ "\n>>> Select polylines <<< ")
 (if
   (setq lSet
   (ssget '((0 . "LWPOLYLINE"))))
(progn
  (setq stLst(asmi-LayersUnlock)
	plLst(mapcar 'vlax-ename->vla-object
	       (vl-remove-if 'listp 
                        (mapcar 'cadr(ssnamex lSet))))
	); end setq
  (vla-StartUndoMark
    (setq actDoc
	   (vla-get-ActiveDocument
		  (vlax-get-acad-object))))
  (foreach pl plLst
  (command "_.fillet" "_r" duct:pWd)
  (command "_.fillet" "_p"
	   (vlax-vla-object->ename pl))
  (setq pl1(car(vlax-safearray->list
	     (vlax-variant-value
	       (vla-Offset pl (/ duct:pWd 2)))))
	pl2(car(vlax-safearray->list
	     (vlax-variant-value
	       (vla-Offset pl (-(/ duct:pWd 2))))))
	vLst1(GetPlineVer
	       (vlax-vla-object->ename pl1))
	vLst2(GetPlineVer
	       (vlax-vla-object->ename pl2))
	); end setq
  (vla-put-ConstantWidth pl1 0.0)
  (vla-put-ConstantWidth pl2 0.0)
  (vla-Delete pl)
  (foreach itm vLst1
    (setq oldOsm(getvar "OSMODE"))
    (setvar "OSMODE" 0)
    (command "._line" itm (car vLst2) "")
    (setvar "OSMODE" oldOsm)
    (setq vLst2(cdr vLst2))
    ); end foreach
    (asmi-LayersStateRestore stLst)
   ); end foreach
  (vla-EndUndoMark actDoc)
  ); end progn
   ); end if
 (princ)
 ); end of c:plp