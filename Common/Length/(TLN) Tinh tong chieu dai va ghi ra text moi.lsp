; Update: 2021-08-24


;;;--------------------------------------------------------------------
(defun Length1(e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
;;;--------------------------------------------------------------------
(defun C:TLN( / ss L e pt Olmode)
(setq
ss (ssget (list (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE")))
L 0.0
)
(vl-load-com)
(while (setq e (ssname ss 0))
(setq L (+ L (length1 e)))
(ssdel e ss)
)

(or *chieucao* (setq *chieucao* 0.5))
(setq chieucao (getreal (strcat "\n Chieu cao text <"
			  (rtos *chieucao* 2 2)
			 "> :"
		  )
	 )
)
(if (not chieucao) (setq chieucao *chieucao*) (setq *chieucao* chieucao))

; Get last osmode and set osmode to 0
(setq Olmode (getvar "OSMODE"))
(setvar "OSMODE" 0)

(setq pt (getpoint "\n Pick point to insert text: "))
(entmake (list (cons 0 "TEXT") (cons 10 pt) (cons 40  chieucao) (cons 1 
	(strcat (rtos L 2 2) )
)))

(_SetClipBoardText (strcat (rtos L 2 2)))
(princ "Result copied to clipboard!")

; Return last osmode
(setvar "OSMODE" Olmode)
(princ)
)
;;;--------------------------------------------------------------------


(defun _SetClipBoardText (text / htmlfile result)
 ;;  Caller's sole responsibility is to pass a
 ;;  text string. Anything else? Pie in face.
 ;;  Attribution: Reformatted version of
 ;;  post by XShrimp at theswamp.org.
 ;;  See http://tinyurl.com/2ngf4r.

 (setq	result
 (vlax-invoke
   (vlax-get
     (vlax-get
       (setq htmlfile (vlax-create-object "htmlfile"))
       'ParentWindow
     )
     'ClipBoardData
   )
   'SetData
   "Text"
   text
 )
 )
 (vlax-release-object htmlfile)
)