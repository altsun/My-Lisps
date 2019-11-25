;;Send xref to back (draworder). Does not discern between model space, or paper space. Switch
;;to model space/paper space depending on where the xref resides.
(defun c:xrd (/ xrl x2)
  (setq cmd (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq BI (tblnext "block" 1))
  (while BI
    (if (assoc 1 BI)
      (setq xrl (cons (cdr (assoc 2 BI)) xrl)))
    (setq BI (tblnext "block"))
  )
  (setq c1 1 c2 0)
  (repeat (length xrl)
   (setq x1 (cons c1 (nth c2 xrl)))
   (setq x2 (append x2 (list x1))) 
   (setq c1 (1+ c1)
         c2 (1+ c2)
   )
  ) 
  (textscr)
  (princ "------>External References present in ths drawing<--------\n")
  (princ "----------------------------------")
  (mapcar 'print x2)
  (setq r (getint "\nEnter Xref No. to send to back. (i.e. DRAWORDER): "))
  (setq c 0)
  (while (/= s1 r)
   (setq s1 (car (nth c x2)))
    (if (= s1 r)
     (setq x3 (cdr (nth c x2)))
    )
    (setq c (1+ c))
  ) 
  (setq en (ssname (ssget "X" (list (cons 0 "INSERT") (cons 2 x3))) 0))
  (command "_draworder" en "" "B")
  (graphscr)
  (setvar "cmdecho" 1)
  (princ)
 );End
 (princ "\nSend External Reference to Back. Switch to model space/paper space depending on where the xref resides.")
 (princ)




