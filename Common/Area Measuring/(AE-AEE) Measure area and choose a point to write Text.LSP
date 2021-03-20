(Defun c:ae()
	(princ "\n\t Go lenh AE, chon 1 diem nam trong vung kin de tinh dien tich.")
	(setvar "cmdecho" 0)
	(command "osnap" "none")
	(initget "Heso Do")
	(setq pt (getpoint "\n He so / <Chon diem>:"))
   	(if (= pt "Heso")
	    	(progn	
			(setq am (getreal "Cho he so"))
			(if (and (null am) (/= ac 0))
				(setq am ac)
			)
		(setq pt (getpoint "\n Chon diem: "))	
		)
		(setq ac am))
			
	(if (or (= am 0) (null am)) (setq am 1))
	(setq s 0)
	(progn 
;		(setq pt (getpoint "\n Chon diem: "))	
	      (while pt
			(setq entold (cdr (assoc 5 (entget (entlast)))))
			(command "boundary" pt "")
			(setq entnew (cdr (assoc 5 (entget (entlast)))))
			(if (/= entold entnew)    
				(progn 
                        	(setq entnew (entget (entlast)))
                        	(if (assoc 62 entnew)
                          		(setq entnew (subst (cons 62 (+ 3 (cdr (assoc 62 entnew)))) (assoc 62 entnew) entnew))
                          		(setq entnew (append entnew (list (cons 62 (+ 3 (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 entnew))))))))))
                          	)
				
                          
                        	(entmod entnew)
                        	(Command "area" "o" (entlast))
					(setq s (+ s (getvar "area")))
   					(setq pt (getpoint "\n Chon diem: "))
					(entdel (entlast))
	        		)
				(progn
					(princ "chon diem sai")
					(setq pt (getpoint "\n Chon diem: "))
				)
			)
		  )

            )
	
	(command "osnap" "intersection")
  	(setq ts (* s am))
	(princ(* s am))
  	(setq Tes(rtos  ts 2 5 ))
 	(setq TexS (strcat Tes))
  	(setq Pnt(getpoint "\n Chi diem dien gia tri Tich: "))
        (command "text" Pnt 0 TexS "")
	(princ)
)

;;;;;;;;;;;;;;;
(Defun c:aee() ; Tinh dien tich de lai duong bo
	(princ "\n\t Go lenh AE, chon 1 diem nam trong vung kin de tinh dien tich.")
	(setvar "cmdecho" 0)
	(command "osnap" "none")
	(initget "Heso Do")
	(setq pt (getpoint "\n He so / <Chon diem>:"))
   	(if (= pt "Heso")
	    	(progn	
			(setq am (getreal "Cho he so"))
			(if (and (null am) (/= ac 0))
				(setq am ac)
			)
		(setq pt (getpoint "\n Chon diem: "))	
		)
		(setq ac am))
			
	(if (or (= am 0) (null am)) (setq am 1))
	(setq s 0)
	(progn 
;		(setq pt (getpoint "\n Chon diem: "))	
	      (while pt
			(setq entold (cdr (assoc 5 (entget (entlast)))))
			(command "boundary" pt "")
			(setq entnew (cdr (assoc 5 (entget (entlast)))))
			(if (/= entold entnew)    
				(progn 
                        	(setq entnew (entget (entlast)))
                        	(if (assoc 62 entnew)
                          		(setq entnew (subst (cons 62 (+ 3 (cdr (assoc 62 entnew)))) (assoc 62 entnew) entnew))
                          		(setq entnew (append entnew (list (cons 62 (+ 3 (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 entnew))))))))))
                          	)
				
                          
                        	(entmod entnew)
                        	(Command "area" "o" (entlast))
					(setq s (+ s (getvar "area")))
   					(setq pt (getpoint "\n Chon diem: "))
				        		)
				(progn
					(princ "chon diem sai")
					(setq pt (getpoint "\n Chon diem: "))
				)
			)
		  )

            )
	
	(command "osnap" "intersection")
  	(setq ts (* s am))
	(princ(* s am))
  	(setq Tes(rtos  ts 2 5 ))
 	(setq TexS (strcat Tes))
  	(setq Pnt(getpoint "\n Chi diem dien gia tri Tich: "))
        (command "text" Pnt 0 TexS "")
	(princ)
)