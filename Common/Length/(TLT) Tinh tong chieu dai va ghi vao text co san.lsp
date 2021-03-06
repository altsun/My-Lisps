; Created by duongphamhn97@gmail.com
; Last update: 2021-05-11


;;;--------------------------------------------------------------------
(defun Length1(e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
;;;--------------------------------------------------------------------
(defun C:TLT( / ss L e s1 otext ot nt )
(setq
ss (ssget (list (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE")))
L 0.0
)
(vl-load-com)
(while (setq e (ssname ss 0))
(setq L (+ L (length1 e)))
(ssdel e ss)
)

(setq s1 (entget (car (entsel "Select text to write total length\n"))))
(setq otext (assoc 1 s1))
(setq ot (cdr otext))
(setq ot (read ot))
(setq nt (cons 1 (strcat (rtos L 2 2))))
(entmod (subst nt otext s1))
(_SetClipBoardText (strcat (rtos L 2 2)))
(princ "Result copied to clipboard!")
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