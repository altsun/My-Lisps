;;;************TRIM LEADER*************************
(defun c:TRIMLEADER (/ LM:mAssoc p lst ss)
(defun LM:mAssoc ( key lst / pair )
(if (setq pair (assoc key lst))
   	(cons (cdr pair) (LM:mAssoc key (cdr (member pair lst))))
)
)
(and (setq ss (ssget '((0 . "*LEADER"))))
 (setq p (getpoint "\nChon diem cat Leader:"))
(foreach ld (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
(entmod (subst (cons 10 p) (cons 10 (last (LM:mAssoc 10 (setq lst (entget ld))))) lst))
)
))
