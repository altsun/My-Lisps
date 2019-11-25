(vl-load-com)
;;;(alert (strcat "\nCh\U+1ECDn c\U+00E1c \U+0111\U+1ED1i t\U+01B0\U+1EE3ng trong v\U+00F9ng k\U+00EDn: CVK"
;;;	       "\nCh\U+1ECDn c\U+00E1c \U+0111\U+1ED1i t\U+01B0\U+1EE3ng ngo\U+00E0i v\U+00F9ng \U+0111\U+01B0\U+1EE3c ch\U+1ECDn: CVN"
;;;	       "\nCh\U+1ECDn c\U+00E1c \U+0111\U+1ED1i t\U+01B0\U+1EE3ng n?m gi\U+1EEFa 2 v\U+00F9ng: CVG"
;;;	       "\nMake by Nguy\U+1EC5n Thi\U+00EAn \U+0110\U+01B0\U+1EDDng"
;;;	       "\nMobile: 0972.0168.25"
;;;       )
;;;)
;;;(Prompt (strcat "\nCh\U+1ECDn c\U+00E1c \U+0111\U+1ED1i t\U+01B0\U+1EE3ng trong v\U+00F9ng k\U+00EDn: CVK"
;;;	       "\nCh\U+1ECDn c\U+00E1c \U+0111\U+1ED1i t\U+01B0\U+1EE3ng ngo\U+00E0i v\U+00F9ng \U+0111\U+01B0\U+1EE3c ch\U+1ECDn: CVN"
;;;	       "\nCh\U+1ECDn c\U+00E1c \U+0111\U+1ED1i t\U+01B0\U+1EE3ng n?m gi\U+1EEFa 2 v\U+00F9ng: CVG"
;;;	       "\nMake by Nguy\U+1EC5n Thi\U+00EAn \U+0110\U+01B0\U+1EDDng"
;;;		"\nMobile: 0972.0168.25"
;;;       )
;;;)
(load "Acetutil.fas") (load "Acetutil2.fas")
(defun c:CVN(/ ss ss1 ssnameAll pl minp maxp ssInside LtsEnameInside LtsLoc1 e);;;XOA VUNG NGOAI
(alert "\nQuet Chon vung: ")
(setq ss (ssget))
(setq ssnameAll (acet-ss-to-list ss))
(alert "\nKich chon Pline kin ")
(setq ss1 (ssget "_:S" '((0 . "LWPOLYLINE,CIRCLE,ELLIPSE"))))
(setq EnamePL1 (car (acet-ss-to-list ss1)))
(setq pl (vlax-ename->vla-object EnamePL1))
(vla-getboundingbox pl 'minp 'maxp);;;LAY TOA DO MAX MIN CUA PLINE
(setq minp (safearray-value minp))
(setq maxp (safearray-value maxp))
(command "zoom" minp maxp)
(setq ssInside (GetssInside1 ss1))
(setq LtsEnameInside (acet-ss-to-list ssInside))
(setq LtsLoc1 (LM:ListDifference  ssnameAll LtsEnameInside))
(setq LtsLoc (vl-remove-if '(lambda (x) (car (member x (list EnamePL1)))) LtsLoc1))
(command ".PSELECT" (acet-list-to-ss LtsLoc) "")
;;;(foreach e LtsLoc
;;;	(entdel e)
;;;)
(princ)
)
(defun c:CVG(/  ss ss1 ss2 EnamePL1 EnamePL2 ssnameAll ssnameS1 ssInsideS2 LtsEnameInside1 LtsEnameInside2 pl minp maxp ssInside LtsEnameInside LtsLoc1 e);;;XOA VUNG GIAO
(alert "\n Chon vung bao ngoai to: ")
(setq ss1 (ssget "_:S" '((0 . "LWPOLYLINE,CIRCLE,ELLIPSE"))))
(setq EnamePL1 (car (acet-ss-to-list ss1)))
(setq pl (vlax-ename->vla-object EnamePL1))
(vla-getboundingbox pl 'minp 'maxp);;;LAY TOA DO MAX MIN CUA PLINE
(setq minp (safearray-value minp))
(setq maxp (safearray-value maxp))

  
(setq ssnameS1(acet-ss-to-list ss1))
(alert "\n Chon vung bao ngoai nho: ")
(setq ss2 (ssget "_:S" '((0 . "LWPOLYLINE,CIRCLE,ELLIPSE"))))
(setq ssnamS2 (acet-ss-to-list ss2))
(setq EnamePL2 (car ssnamS2))
(command "zoom" minp maxp)
(setq ssInsideS1 (GetssInside1 ss1))
(setq ssInsideS2 (GetssInside1 ss2))
(setq LtsEnameInside1 (acet-ss-to-list ssInsideS1))
(setq LtsEnameInside2 (acet-ss-to-list ssInsideS2))
(setq LtsLoc1 (LM:ListDifference  LtsEnameInside1 LtsEnameInside2))
(setq LtsLoc (vl-remove-if '(lambda (x) (car (member x (list EnamePL2)))) LtsLoc1));;;;LOAI BO PLINE THU 2 (PLINE NHO) RA KHOI VUNG DANH SACH DUOC CHON
(command ".PSELECT" (acet-list-to-ss LtsLoc) "")
;;;(foreach e LtsLoc
;;;		(entdel e)
;;;)
(princ)
)
(defun LM:ListDifference ( l1 l2 )
  (if l1
    (if (member (car l1) l2)
      (LM:ListDifference (cdr l1) l2)
      (cons (car l1) (LM:ListDifference (cdr l1) l2))
    )
  )
)
(defun C:CVK ( / ss ov vl ss1 ss2 ptLst plSet);;;XOA VUNG KIN
(defun *error* (msg)    
	(if ov (mapcar 'setvar vl ov))									; Reset Sys vars
	(princ (strcat "\nError: " msg))									; Print Error Message
	(princ)
)												; Exit Cleanly

(setq  vl '("CMDECHO" "OSMODE" "ORTHOMODE")								; Sys Var list
	ov (mapcar 'getvar vl)
)										; Get Old values  
(mapcar 'setvar vl '(1 0 0))										; Turn off CMDECHO, OSMODE, ORTHOMODE
(princ "\nChon duong bao...")
(setq ss (ssget "_:S" '((0 . "LWPOLYLINE,CIRCLE,ELLIPSE"))))		; Chon duong bao
(setq EnamePL1 (car (acet-ss-to-list ss)))
(setq pl (vlax-ename->vla-object EnamePL1))
(vla-getboundingbox pl 'minp 'maxp);;;LAY TOA DO MAX MIN CUA PLINE
(setq minp (safearray-value minp))
(setq maxp (safearray-value maxp))
(command "zoom" minp maxp)
(setq ssInside (GetssInside1 ss))						; Lay cac doi tuong trong duong bao K1
(command ".PSELECT" ssInside "")
;;;(foreach e (acet-ss-to-list ssInside)
;;;	(entdel e)
;;;)
(mapcar 'setvar vl ov)
(princ)
)
;---------- Tao tap hop chon gom cac doi tuong TRONG duong bao, khong bao gom cac phan cat cua duong giao
;(defun GetssInside1 (ss1 / ptLst cur ssInside ssAll ssOutside i e)
(defun GetssInside1 (ss1)
  (setq ptLst (GetPtLst (setq cur (ssname ss1 0)))
	ssInside (ssget "_WP" ptLst)))									; ssInside: trong duong bao
;---------- Tao tap hop chon gom cac doi tuong NGOAI duong bao, khong bao gom cac phan cat cua duong giao
;(defun GetssOutside1 (ss1 / ptLst cur ssInside ssAll ssOutside i e)
(defun GetssOutside1 (ss1)
  (setq ptLst (GetPtLst (setq cur (ssname ss1 0)))
	ssInside (ssget "_WP" ptLst)									; ssInside: trong duong bao
        ssAll (ssget "X")
        ssOutside (ssdel (ssname ss1 0) ssAll))
  (setq i 0)
  (while (setq e (ssname ssInside i))
   (setq ssOutside (ssdel e ssOutside))									; ssOutside: ngoai duong bao
   (setq i (1+ i)))
  ssOutside)
;---------- Tao tap hop chon gom cac doi tuong GIUA 2 duong bao, khong bao gom cac phan cat cua duong giao
;(defun GetssGiside2 (ssT ssN / ptLstT ptLstN curT curN ssInsideT ssInsideN ssGiside i e)
(defun GetssGiside2 (ssT ssN)
  (setq ptLstT (GetPtLst (setq curT (ssname ssT 0)))
        ptLstN (GetPtLst (setq curN (ssname ssN 0)))
	ssInsideT (ssget "_WP" ptLstT)
	ssInsideN (ssget "_WP" ptLstN)
        ssGiside (ssdel (ssname ssN 0) ssInsideN)
        ssGiside (ssdel (ssname ssT 0) ssInsideN))
  (setq i 0)
  (while (setq e (ssname ssInsideT i))
   (setq ssGiside (ssdel e ssGiside))									; ssGiside: giua 2 duong bao
   (setq i (1+ i)))
  ssGiside)
;--------------------------------------------------------------------------------------------------------------------------------------------------
;---------- Tao tap hop chon gom cac doi tuong NGOAI duong bao va phai cat duong bao
(defun GetssOutside (ss2 / ptLst cur ssInside lstss1 ss1 ssTouching)
  (if (and (setq lstss1 (gettouching ss2))
	   (setq ss1 (ssadd))
	   (mapcar '(lambda (x) (ssadd x ss1)) lstss1))
    (progn												; co ssTouching 
      (break_with ss1 ss2 nil 0)
      (setq cur (ssname ss2 0)
	    ssTouching (ssadd)
	    ssOutside (ssadd))
      (mapcar '(lambda (x) (ssadd x ssTouching)) (gettouching ss2))
      (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ssTouching)))				; loc ssTouching -> ssOutside
	(if
	  (or
	    (not (insidep (vlax-curve-getStartPoint e) cur))
	    (not (insidep (vlax-curve-getEndPoint e) cur))
	    (not (insidep (vlax-curve-getPointAtParam e (/ (+ (vlax-curve-getStartParam e) (vlax-curve-getEndParam e)) 2)) cur)))
	  (ssadd e ssOutside)))))
  (if (ssmemb cur ssOutside) (ssdel cur ssOutside))
  ssOutside)
;---------- Tao tap hop chon gom cac doi tuong TRONG duong bao, bao gom cac phan cat cua duong giao
(defun GetssInside (ss2 / ptLst cur ssInside lstss1 ss1 ssTouching)
  (setq ptLst (GetPtLst (setq cur (ssname ss2 0)))
	ssInside (ssget "_WP" ptLst))  
  (if (and (setq lstss1 (gettouching ss2))
	   (setq ss1 (ssadd))
	   (mapcar '(lambda (x) (ssadd x ss1)) lstss1))
    (progn												; co ssTouching
      (break_with ss1 ss2 nil 0)
      (setq ssTouching (ssadd))
      (mapcar '(lambda (x) (ssadd x ssTouching)) (gettouching ss2))
      (or ssInside (setq ssInside (ssadd)))								; loc ssTouching -> ssInside
      (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ssTouching)))
	(if
	  (and (insidep (vlax-curve-getStartPoint e) cur)
	       (insidep (vlax-curve-getEndPoint e) cur)
	       (insidep (vlax-curve-getPointAtParam e (/ (+ (vlax-curve-getStartParam e) (vlax-curve-getEndParam e)) 2)) cur))
	  (ssadd e ssInside)))))
  (if (ssmemb cur ssInside) (ssdel cur ssInside))
  ssInside)
;----------
(defun GetPtLst (obj / startparam endparam anginc delta div inc pt ptlst)
  (defun ZClosed (lst)
    (if (and (vlax-curve-isClosed obj)
       (not (equal (car lst) (last lst) 1e-6)))
      (append lst (list (car lst)))
      lst))
  (or (eq (type obj) 'VLA-OBJECT)
    (setq obj (vlax-ename->vla-object obj)))
  (setq typ (vlax-get obj 'ObjectName))
  (if (or (eq typ "AcDbCircle") (eq typ "AcDbEllipse"))
    (progn
      (setq param 0)
      (while (< param (* pi 2))
	(setq pt (vlax-curve-getPointAtParam obj param)
	      ptlst (cons pt ptlst)
	      param (+ (/ (* pi 2) 72) param)))
      (reverse ptlst))
    (progn ;Pline (eq typ "AcDbPolyline")
      (setq param (vlax-curve-getStartParam obj)
	    endparam (vlax-curve-getEndParam obj)
	    anginc (* pi (/ 7.5 180.0)))
      (setq tparam param)
      (while (<= param endparam)
	(setq pt (vlax-curve-getPointAtParam obj param))
	(if (not (equal pt (car ptlst) 1e-12))
	  (setq ptlst (cons pt ptlst)))
	(if  (and (/= param endparam)
		  (setq blg (abs (vlax-invoke obj 'GetBulge param)))
		  (/= 0 blg))
	  (progn
	    (setq delta (* 4 (atan blg)) ;included angle
		  inc (/ 1.0 (1+ (fix (/ delta anginc))))
                  arcparam (+ param inc))
	    (while (< arcparam (1+ param))
	      (setq pt (vlax-curve-getPointAtParam obj arcparam)
                    ptlst (cons pt ptlst)
                    arcparam (+ inc arcparam)))))
	(setq param (1+ param)))
      (if (and (apply 'and ptlst)
	       (> (length ptlst) 1))
	(ZClosed (reverse ptlst))))))
;----------
(defun insidep  (pt Obj / Obj Tol ang doc spc flag int lin xV yV)
  (defun vlax-list->3D-point (lst flag)
  (if lst
    (cons ((if flag car cadr) lst)
          (vlax-list->3D-point (cdddr lst) flag))))
  (or (eq 'VLA-OBJECT (type Obj))
      (setq Obj (vlax-ename->vla-object Obj)))
  (if (not(vlax-curve-getParamAtPoint Obj pt))
    (progn
  (setq Tol  (/ pi 6) ; Uncertainty
        ang  0.0 flag T)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
        spc (if (zerop (vla-get-activespace doc))
              (if (= (vla-get-mspace doc) :vlax-true)
                (vla-get-modelspace doc)
                (vla-get-paperspace doc))
              (vla-get-modelspace doc)))
  (while (and (< ang (* 2 pi)) flag)
    (setq flag (and
                 (setq int
                   (vlax-invoke
                     (setq lin
                       (vla-addLine spc
                         (vlax-3D-point pt)
                           (vlax-3D-point
                             (polar pt ang
                               (if (vlax-property-available-p Obj 'length)
                                 (vla-get-length Obj) 1.0)))))
                                  'IntersectWith Obj
                                    acExtendThisEntity))
                 (<= 6 (length int))
                 (setq xV (vl-sort (vlax-list->3D-point int T) '<)
                       yV (vl-sort (vlax-list->3D-point int nil) '<))
                 (or (<= (car xV) (car pt) (last xV))
                     (<= (car yV) (cadr pt) (last yV))))
          ang  (+ ang Tol))
    (vla-delete lin))
  flag)
    T))
;---------- Lay tat ca cac doi tuong cham vao cac duong bao, tra ve danh sach cac ename
 (defun gettouching (sscros / ss lst lstb lstc objl)
    (and
      (setq lstb (vl-remove-if 'listp (mapcar 'cadr (ssnamex sscros)))
            objl (mapcar 'vlax-ename->vla-object lstb))
      (setq ss (ssget "_A" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
				 (cons 410 (getvar "ctab")))))
      (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
      (setq lst (mapcar 'vlax-ename->vla-object lst))
      (mapcar
        '(lambda (x)
           (mapcar
             '(lambda (y)
                (if (not
                      (vl-catch-all-error-p
                        (vl-catch-all-apply
                          '(lambda ()
                             (vlax-safearray->list
                               (vlax-variant-value
                                 (vla-intersectwith y x acextendnone)
                               ))))))
                  (setq lstc (cons (vlax-vla-object->ename x) lstc)))) objl)) lst)) lstc)
;---------- Cat cac duong giao, tra ve danh sach ten cac doi tuong moi
(defun break_with (ss2brk ss2brkwith self Gap / cmd intpts lst masterlist ss ssobjs onlockedlayer ssget->vla-list list->3pair GetNewEntities oc get_interpts break_obj GetLastEnt LastEntInDatabase ss2brkwithList)
  (vl-load-com)  
  (princ "\nCalculating Break Points, Please Wait.\n")
;----------
  (defun onlockedlayer (ename / entlst)
    (setq entlst (tblsearch "LAYER" (cdr (assoc 8 (entget ename)))))
    (= 4 (logand 4 (cdr (assoc 70 entlst)))))
;|  (defun ssget->vla-list (ss)
    (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss ))))
  )|;
;---------- this is faster, changed in ver 1.7
  (defun ssget->vla-list (ss / i ename allobj)
       (setq i -1)
       (while (setq  ename (ssname ss (setq i (1+ i))))
         (setq allobj (cons (vlax-ename->vla-object ename) allobj)))
       allobj)
;---------- return a list of lists grouped by 3 from a flat list
  (defun list->3pair (old / new)
    (while (setq new (cons (list (car old) (cadr old) (caddr old)) new)
                 old (cdddr old)))
    (reverse new))
;---------- return a list of intersect points 
(defun get_interpts (obj1 obj2 / iplist)
  (if (not (vl-catch-all-error-p
             (setq iplist (vl-catch-all-apply
                            'vlax-safearray->list
                            (list
                              (vlax-variant-value
                                (vla-intersectwith obj1 obj2 acextendnone)
                              ))))))
    iplist))
;---------- Break entity at break points in list
(defun break_obj (ent brkptlst BrkGap / brkobjlst en enttype maxparam closedobj minparam obj obj2break p1param p2param brkpt2 dlst idx brkptS brkptE brkpt result GapFlg result ignore dist tmppt #ofpts 2gap enddist lastent obj2break stdist)
  (or BrkGap (setq BrkGap 0.0)) ; default to 0
  (setq BrkGap (/ BrkGap 2.0)) ; if Gap use 1/2 per side of break point
  (setq obj2break ent
        brkobjlst (list ent)
        enttype   (cdr (assoc 0 (entget ent)))
        GapFlg    (not (zerop BrkGap)) ; gap > 0
        closedobj (vlax-curve-isclosed obj2break))
  (if (zerop Brkgap)								;; when zero gap no need to break at end points
    (setq spt (vlax-curve-getstartpoint ent)
          ept (vlax-curve-getendpoint ent)
          brkptlst (vl-remove-if '(lambda(x) (or (< (distance x spt) 0.0001)
                                                 (< (distance x ept) 0.0001)))
                                 brkptlst)))
  (if brkptlst
    (progn
  ;;  sort break points based on the distance along the break object
  ;;  get distance to break point, catch error if pt is off end
  ;; ver 2.0 fix - added COND to fix break point is at the end of a
  ;; line which is not a valid break but does no harm
  (setq brkptlst (mapcar '(lambda(x) (list x (vlax-curve-getdistatparam obj2break
                                               ;; ver 2.0 fix
                                               (cond ((vlax-curve-getparamatpoint obj2break x))
                                                   ((vlax-curve-getparamatpoint obj2break
                                                     (vlax-curve-getclosestpointto obj2break x))))))
                            ) brkptlst))
  ;; sort primary list on distance
  (setq brkptlst (vl-sort brkptlst '(lambda (a1 a2) (< (cadr a1) (cadr a2)))))
  (if GapFlg ; gap > 0
    ;; Brkptlst starts as the break point and then a list of pairs of points
    ;;  is creates as the break points
    (progn
      ;;  create a list of list of break points
      ;;  ((idx# stpoint distance)(idx# endpoint distance)...)
      (setq idx 0)
      (foreach brkpt brkptlst
        
        ;; ----------------------------------------------------------
        ;;  create start break point, then create end break point    
        ;;  ((idx# startpoint distance)(idx# endpoint distance)...)  
        ;; ----------------------------------------------------------
        (setq dist (cadr brkpt)) ; distance to center of gap
        ;;  subtract gap to get start point of break gap
        (cond
          ((and (minusp (setq stDist (- dist BrkGap))) closedobj )
           (setq stdist (+ (vlax-curve-getdistatparam obj2break
                             (vlax-curve-getendparam obj2break)) stDist))
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
           )
          ((minusp stDist) ; off start of object so get startpoint
           (setq dlst (cons (list idx (vlax-curve-getstartpoint obj2break) 0.0) dlst))
           )
          (t
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
          )
        )
        ;;  add gap to get end point of break gap
        (cond
          ((and (> (setq stDist (+ dist BrkGap))
                   (setq endDist (vlax-curve-getdistatparam obj2break
                                     (vlax-curve-getendparam obj2break)))) closedobj )
           (setq stdist (- stDist endDist))
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
           )
          ((> stDist endDist) ; off end of object so get endpoint
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                        (vlax-curve-getendparam obj2break))
                                  endDist) dlst))
           )
          (t
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
          )
        )
        ;; -------------------------------------------------------
        (setq idx (1+ IDX))
      ) ; foreach brkpt brkptlst
      

      (setq dlst (reverse dlst))
      ;;  remove the points of the gap segments that overlap
      (setq idx -1
            2gap (* BrkGap 2)
            #ofPts (length Brkptlst)
      )
      (while (<= (setq idx (1+ idx)) #ofPts)
        (cond
          ((null result) ; 1st time through
           (setq result (list (car dlst)) ; get first start point
                 result (cons (nth (1+(* idx 2)) dlst) result))
          )
          ((= idx #ofPts) ; last pass, check for wrap
           (if (and closedobj (> #ofPts 1)
                    (<= (+(- (vlax-curve-getdistatparam obj2break
                            (vlax-curve-getendparam obj2break))
                          (cadr (last BrkPtLst))) (cadar BrkPtLst)) 2Gap))
             (progn
               (if (zerop (rem (length result) 2))
                 (setq result (cdr result)) ; remove the last end point
               )
               ;;  ignore previous endpoint and present start point
               (setq result (cons (cadr (reverse result)) result) ; get last end point
                     result (cdr (reverse result))
                     result (reverse (cdr result)))
             )
           )
          )
          ;; Break Gap Overlaps
          ((< (cadr (nth idx Brkptlst)) (+ (cadr (nth (1- idx) Brkptlst)) 2Gap))
           (if (zerop (rem (length result) 2))
             (setq result (cdr result)) ; remove the last end point
           )
           ;;  ignore previous endpoint and present start point
           (setq result (cons (nth (1+(* idx 2)) dlst) result)) ; get present end point
           )
          ;; Break Gap does Not Overlap previous point 
          (t
           (setq result (cons (nth (* idx 2) dlst) result)) ; get this start point
           (setq result (cons (nth (1+(* idx 2)) dlst) result)) ; get this end point
          )
        ) ; end cond stmt
      ) ; while
      
      (setq dlst     (reverse result)
            brkptlst nil)
      (while dlst ; grab the points only
        (setq brkptlst (cons (list (cadar dlst)(cadadr dlst)) brkptlst)
              dlst   (cddr dlst))
      )
    )
  )
  ;;   -----------------------------------------------------

  ;; (if (equal  a ent) (princ)) ; debug CAB  -------------
 
  (foreach brkpt (reverse brkptlst)
    (if GapFlg ; gap > 0
      (setq brkptS (car brkpt)
            brkptE (cadr brkpt))
      (setq brkptS (car brkpt)
            brkptE brkptS)
    )
    ;;  get last entity created via break in case multiple breaks
    (if brkobjlst
      (progn
        (setq tmppt brkptS) ; use only one of the pair of breakpoints
        ;;  if pt not on object x, switch objects
        (if (not (numberp (vl-catch-all-apply
                            'vlax-curve-getdistatpoint (list obj2break tmppt))))
          (progn ; find the one that pt is on
            (setq idx (length brkobjlst))
            (while (and (not (minusp (setq idx (1- idx))))
                        (setq obj (nth idx brkobjlst))
                        (if (numberp (vl-catch-all-apply
                                       'vlax-curve-getdistatpoint (list obj tmppt)))
                          (null (setq obj2break obj)) ; switch objects, null causes exit
                          t
                        )
                   )
            )
          )
        )
      )
    )

    (setq closedobj (vlax-curve-isclosed obj2break))
    (if GapFlg ; gap > 0
      (if closedobj
        (progn ; need to break a closed object
          (setq brkpt2 (vlax-curve-getPointAtDist obj2break
                     (- (vlax-curve-getDistAtPoint obj2break brkptE) 0.00001)))
          (command "._break" obj2break "_non" (trans brkpt2 0 1)
                   "_non" (trans brkptE 0 1))
          (and (= "CIRCLE" enttype) (setq enttype "ARC"))
          (setq BrkptE brkpt2)
        )
      )

      (if (and closedobj 
               (not (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (+ (vlax-curve-getdistatparam obj2break
                            ;;(vlax-curve-getparamatpoint obj2break brkpts)) 0.00001))))
                            ;; ver 2.0 fix
                            (cond ((vlax-curve-getparamatpoint obj2break brkpts))
                                  ((vlax-curve-getparamatpoint obj2break
                                      (vlax-curve-getclosestpointto obj2break brkpts))))) 0.00001)))))
        (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (- (vlax-curve-getdistatparam obj2break
                            ;;(vlax-curve-getparamatpoint obj2break brkpts)) 0.00001)))
                            ;; ver 2.0 fix
                            (cond ((vlax-curve-getparamatpoint obj2break brkpts))
                                  ((vlax-curve-getparamatpoint obj2break
                                      (vlax-curve-getclosestpointto obj2break brkpts))))) 0.00001)))
       )
    ) ; endif
    
    ;; (if (null brkptE) (princ)) ; debug
    
    (setq LastEnt (GetLastEnt))
    (command "._break" obj2break "_non" (trans brkptS 0 1) "_non" (trans brkptE 0 1))
    (and *BrkVerbose* (princ (setq *brkcnt* (1+ *brkcnt*))) (princ "\r"))
    (and (= "CIRCLE" enttype) (setq enttype "ARC"))
    (if (and (not closedobj) ; new object was created
             (not (equal LastEnt (entlast))))
        (setq brkobjlst (cons (entlast) brkobjlst))
    )
  )
  )
  ) ; endif brkptlst
  
) ; defun break_obj

;;====================================
;;  CAB - get last entity in datatbase
(defun GetLastEnt ( / ename result )
  (if (setq result (entlast))
    (while (setq ename (entnext result))
      (setq result ename)
    )
  )
  result
)
;;===================================
;;  CAB - return a list of new enames
(defun GetNewEntities (ename / new)
  (cond
    ((null ename) (alert "Ename nil"))
    ((eq 'ENAME (type ename))
      (while (setq ename (entnext ename))
        (if (entget ename) (setq new (cons ename new)))
      )
    )
    ((alert "Ename wrong type."))
  )
  new
)
  
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;;         S T A R T  S U B R O U T I N E   H E R E              
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
    (setq LastEntInDatabase (GetLastEnt))
    (if (and ss2brk ss2brkwith)
    (progn
      (setq oc 0
            ss2brkwithList (ssget->vla-list ss2brkwith))
      (if (> (* (sslength ss2brk)(length ss2brkwithList)) 5000)
        (setq *BrkVerbose* t)
      )
      (and *BrkVerbose*
           (princ (strcat "Objects to be Checked: "
            (itoa (* (sslength ss2brk)(length ss2brkwithList))) "\n")))
      ;;  CREATE a list of entity & it's break points
      (foreach obj (ssget->vla-list ss2brk) ; check each object in ss2brk
        (if (not (onlockedlayer (vlax-vla-object->ename obj)))
          (progn
            (setq lst nil)
            ;; check for break pts with other objects in ss2brkwith
            (foreach intobj  ss2brkwithList
              (if (and (or self (not (equal obj intobj)))
                       (setq intpts (get_interpts obj intobj))
                  )
                (setq lst (append (list->3pair intpts) lst)) ; entity w/ break points
              )
              (and *BrkVerbose* (princ (strcat "Objects Checked: " (itoa (setq oc (1+ oc))) "\r")))
            )
            (if lst
              (setq masterlist (cons (cons (vlax-vla-object->ename obj) lst) masterlist))
            )
          )
        )
      )    
      (and *BrkVerbose* (princ "\nBreaking Objects.\n"))
      (setq *brkcnt* 0) ; break counter
      ;;  masterlist = ((ent brkpts)(ent brkpts)...)
      (if masterlist
        (foreach obj2brk masterlist
          (break_obj (car obj2brk) (cdr obj2brk) Gap)
        )
      )
      )
  )
;;==============================================================
   (and (zerop *brkcnt*) (princ "\nNone to be broken."))
   (setq *BrkVerbose* nil)
  (GetNewEntities LastEntInDatabase)) ; return list of enames of new objects


