(defun C:SW (/ dat c elst wid ename pend pt)
(vl-load-com)
(setvar "cmdecho" 0)
(setq plw (getvar "plinewid"))
(if
(and (setq dat (entsel "\nSelect source polyline: "))
(wcmatch (cdadr (setq elst (entget (setq ename (car dat)))))
"*POLYLINE*"))
(progn
(setq wid (cdr (assoc 40 elst)))
(prompt (strcat "\nWidth is " (rtos wid)))
(setq pend (osnap (cadr dat) "_end"))
(setq pt
(cond
((equal (vlax-curve-getstartpoint ename) pend 0.0001)
(vlax-curve-getstartpoint ename))
((equal (vlax-curve-getendpoint ename) pend 0.0001)
(vlax-curve-getendpoint ename))
(t nil)))
(if pt
(setq p pt)
(setq p (getpoint "\nSpecify start point: ")))
(command "_.pline" p "_w" wid wid)
(while (eq 1 (logand 1 (getvar "cmdactive")))
(command pause))
(if
(and pt (wcmatch (cdadr (entget (entlast))) "*POLYLINE*"))
(command "_.pedit" ename "_j" (entlast) "" "")))
(prompt "\nNot a polyline"))
(if plw
(setvar "plinewid" plw))
(setvar "cmdecho" 1)
(princ))
(princ)