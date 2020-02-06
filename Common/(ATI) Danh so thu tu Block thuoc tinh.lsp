; Oringin: https://forums.augi.com/showthread.php?145671-(HELP)-Need-a-LISP-Attribute-increments&p=1210997&viewfull=1#post1210997
; Modified by Phạm Thế Dương

(Defun c:ATI (/ startnumber limit
                nestedobjsectselectionmode
                entitydata
                selectedentitytype
                attributeValuetobereplace
                )
     
     (while (not (progn
     (setq prefix (getstring "\nEnter prefix: "))
     (setq startnumber (getstring "\nEnter Start number: "))
     (if (and (numberp (setq limit (read startnumber)))
                                   (<= 1 limit 100000))
               limit
               (and (princ "\n<Invalid number>") nil)))))
     (while (setq nestedobjsectselectionmode (car (nentsel "\nSelect Attribute/Enter to exit:")))
          (setq entitydata (entget nestedobjsectselectionmode))
          (setq selectedentitytype (cdr (assoc 0 entitydata)))
          (if (eq selectedentitytype "ATTRIB")
               (progn
                    (setq attributeValuetobereplace (assoc 1 entitydata))
                    (if (> limit 9)
                         (entmod (subst (cons 1
                                        (strcat prefix (itoa limit)))
                                   attributeValuetobereplace
                                   entitydata))
                         (entmod (subst (cons 1
                                        (strcat prefix "0" (itoa limit)))
                                   attributeValuetobereplace
                                   entitydata))
                    )
                    
                    (setq limit (+ 1 limit))
                         )
               (princ "\nNot an attribute definition"))
          )
     (princ)
     )