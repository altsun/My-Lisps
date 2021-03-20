; Modded by Phạm Thế Dương
; duongphamhn97@gmail.com

(defun c:ss ( / Olmode Clor loop Pnt e    )
(vl-load-com)
(defun *error* ( msg )
(if Olmode (setvar 'osmode Olmode))
(if Clor (setvar 'CECOLOR Clor))
(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
)
(princ)
)
(setq Olmode (getvar "OSMODE"))
(setvar "hpgaptol" 0.5)
(setvar "OSMODE" 0)
(setvar "CECOLOR" "ByLayer")
(setq Clor (getvar "CECOLOR"))
(setvar "CECOLOR" "2")
(or *chieucao* (setq *chieucao* 0.5))
(setq chieucao (getreal (strcat "\n Chieu cao text <"
			  (rtos *chieucao* 2 2)
			 "> :"
		  )
	 )
)
(if (not chieucao) (setq chieucao *chieucao*) (setq *chieucao* chieucao))


  
(setq loop T)
(while loop
	(setq Pnt (getpoint "\nPick \U+0111i\U+1EC3m trong v\U+00F9ng c\U+1EA7n t\U+00EDnh di\U+1EC7n t\U+00EDch: "))
  	(cond
	  (T
		(if Pnt
		  (progn
			(vl-cmdf  "-boundary" Pnt "")
		    	(setq e (entlast))
			(entmake (list (cons 0 "TEXT") (cons 10 Pnt) (cons 40  chieucao) (cons 1 
				(strcat (rtos (/ (Area e) 1000000) 2 1 ) " M2" )
			)))
			(entdel e)
		    	
		  )
		  (setq loop nil)
		)
	  )
	)
)
(setvar "CECOLOR" "ByLayer")
(setvar "OSMODE" Olmode)
(princ)
)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Area (ent)
(setvar "hpgaptol" 0.1)
(vla-get-area (vlax-ename->vla-object ent))
)
(defun mid (ent / p1 p2)
	(vla-getboundingbox (vlax-ename->vla-object ent) 'p1 'p2)
	(setq p1 (vlax-safearray->list p1)
				p2 (vlax-safearray->list p2)
				pt (mapcar '+ p1 p2)
				pt (mapcar '* pt '(0.5 0.5 0.5))
	)
	pt
)
