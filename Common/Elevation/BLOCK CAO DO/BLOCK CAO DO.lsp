;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.
(defun LM:setattributevalue ( blk tag val / end enx )
    (while
        (and
            (null end)
            (setq blk (entnext blk))
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk)))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (if (entmod (subst (cons 1 val) (assoc 1 (reverse enx)) enx))
                (progn
                    (entupd blk)
                    (setq end val)
                )
            )
        )
    )
)


(DEFUN C:CAODO(/ BHT BHT1 ent blkName tagName toadogoc t1 t1a phantich ss1 lengss n blk1 t3 t4 cham leng1 sokytu)
  (setq BHT (getvar "LUPREC"))
  (setq BHT1 (setvar "LUPREC" 3))
  (while (not (and
		(setq ent (car (nentsel "\nSelect ATT in Block:")))
		(if ent (eq (cdr (assoc 0 (entget ent))) "ATTRIB") ) ) )
  (princ "\n Ban chon nham roi! "))
  (setq blkName (cdr (assoc 2 (entget (cdr (assoc 330 (entget ent))))))
	tagName (cdr (assoc 2 (entget ent))))
  (setq toadogoc(cadr(cdr(assoc 10 (entget (cdr (assoc 330 (entget ent))))))))
  (setq t1  (cdr(assoc 1 (entget ent))))	;text lay ra tu att
  (setq t1a (substr t1 1 1))
  (setq phantich (substr (cdr(assoc 1 (entget ent))) 1 3))		    		;trich bo dau + -
  (IF (= phantich "%%P")
    (setq giatrigoc (atof(substr t1 4)))       ;lay gia tri att
    	(IF (= t1a "+")
            (setq giatrigoc (atof(substr t1 2)))
	    (setq giatrigoc (* -1 (atof(substr t1 2))))))	

 ;PHAN TICH TOA DO DIEM X CUA BLOCK

;CHON BLOCK
(setq ss1 (ssget  (list (cons 0 "INSERT")(cons 66 1)'(-4 . "<NOT") (assoc 10 (entget (cdr (assoc 330 (entget ent))))) '(-4 . "NOT>")  )))	;CHON BLOCK CAO DO(cons -4 "<NOT") (cons -1 ent) (cons -4 "NOT>")
(setq lengss (sslength ss1))
(setq n 0)
(REPEAT lengss
  (setq blk1 (ssname ss1 n))
  (setq t3 (+ giatrigoc  (* 0.001    (- (cadr(cdr(assoc 10 (entget (ssname ss1 n))))) toadogoc))));kiem tra gia tri so voi cao do goc la am hay duong
  (setq t4 (rtos  t3))

;KIEM TRA TEXT THIEU BAO NHIEU SO
  (setq cham (vl-string-search "." t4)) ;kiem tra dau "."
  (IF (= cham nil)
    (setq t4 (strcat t4 ".000"))
    (progn
      (setq leng1 (strlen (substr t4 (+ cham 2))))
      (setq sokytu (- 3 leng1))				;so ky tu phia sau dau cham
      (REPEAT sokytu
	(setq t4 (strcat t4 "0")))))
    	
  (IF (minusp t3)					;neu gia tri am
  	(setq giatri t4)			;quy doi so thanh chuôi
  	(setq giatri (strcat "+" t4)))		;quy doi so thanh chuôi
  
  (LM:setattributevalue blk1 tagName giatri)
  (setq n (+ 1 n)))
  (setvar "LUPREC" BHT)
  )

  




    

  