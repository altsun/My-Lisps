;;;=======================[ BreakObjects.lsp ]==============================
;;; Author: Copyright© 2006-2012 Charles Alan Butler 
;;; Contact @  www.TheSwamp.org    
;;;   http://www.theswamp.org/index.php?topic=10370.0
;;; Version:  2.2  July 28, 2012
;;; Purpose: Break All selected objects
;;;    permitted objects are lines, lwplines, plines, splines,
;;;    ellipse, circles & arcs 
;;;                            
;;;  Function  c:MyBreak -       DCL for selecting the routines
;;;  Function  c:BreakAll -      Break all objects selected with each other
;;;  Function  c:BreakwObject  - Break many objects with a single object
;;;  Function  c:BreakObject -   Break a single object with other objects 
;;;  Function  c:BreakWith -     Break selected objects with other selected objects
;;;  Function  c:BreakTouching - Break objects touching selected objects
;;;  Function  c:BreakSelected - Break selected objects with any objects that touch it 
;;;  Function  c:BreakRemove - Break selected object with any objects that touch it & remove 
;;;                         every other new segment, start with selected object
;;;  Revision 1.8 Added Option for Break Gap greater than zero
;;;  NEW r1.9  c:BreakWlayer -   Break objects with objects on a layer
;;;  NEW r1.9  c:BreakWithTouching - Break touching objects with selected objects
;;;  Revision 2.0 Fixed a bug when point to break is at the end of object
;;;  Revision 2.1 Fixed another bug when point to break is at the end of object
;;;  Revision 2.2 Fixed another bug when closed objects are to be broken
;;;
;;;
;;;  Function  break_with  - main break function called by all others and
;;;                          returns a list of new enames, see c:BreakAll
;;;                          for an example of using the return list
;;;
;;; Requirements: objects must have the same z-value
;;; Restrictions: Does not Break objects on locked layers 
;;; Returns:  none
;;;
;;;=====================================================================
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED     ;
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR  ;
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;=====================================================================


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;               M A I N   S U B R O U T I N E                   
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun break_with (ss2brk ss2brkwith self Gap / cmd intpts lst masterlist ss ssobjs
                   onlockedlayer ssget->vla-list list->3pair GetNewEntities oc
                   get_interpts break_obj GetLastEnt LastEntInDatabase ss2brkwithList
                  )
  ;; ss2brk     selection set to break
  ;; ss2brkwith selection set to use as break points
  ;; self       when true will allow an object to break itself
  ;;            note that plined will break at each vertex
  ;;
  ;; return list of enames of new objects
  
  (vl-load-com)
  
  (princ "\nCalculating Break Points, Please Wait.\n")

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;                S U B   F U N C T I O N S                      
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ;;  return T if entity is on a locked layer
  (defun onlockedlayer (ename / entlst)
    (setq entlst (tblsearch "LAYER" (cdr (assoc 8 (entget ename)))))
    (= 4 (logand 4 (cdr (assoc 70 entlst))))
  )

  ;;  return a list of objects from a selection set
;|  (defun ssget->vla-list (ss)
    (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss ))))
  )|;
  (defun ssget->vla-list (ss / i ename allobj) ; this is faster, changed in ver 1.7
       (setq i -1)
       (while (setq  ename (ssname ss (setq i (1+ i))))
         (setq allobj (cons (vlax-ename->vla-object ename) allobj))
       )
       allobj
  )
  
  ;;  return a list of lists grouped by 3 from a flat list
  (defun list->3pair (old / new)
    (while (setq new (cons (list (car old) (cadr old) (caddr old)) new)
                 old (cdddr old)))
    (reverse new)
  )
  
;;=====================================
;;  return a list of intersect points  
;;=====================================
(defun get_interpts (obj1 obj2 / iplist)
  (if (not (vl-catch-all-error-p
             (setq iplist (vl-catch-all-apply
                            'vlax-safearray->list
                            (list
                              (vlax-variant-value
                                (vla-intersectwith obj1 obj2 acextendnone)
                              ))))))
    iplist
  )
)


;;========================================
;;  Break entity at break points in list  
;;========================================
;;   New as per version 1.8 [BrkGap] --- This subroutine has been re-written
;;  Loop through the break points breaking the entity
;;  If the entity is not a closed entity then a new object is created
;;  This object is added to a list. When break points don't fall on the current 
;;  entity the list of new entities are searched to locate the entity that the 
;;  point is on so it can be broken.
;;  "Break with a Gap" has been added to this routine. The problem faced with 
;;  this method is that sections to be removed may lap if the break points are
;;  too close to each other. The solution is to create a list of break point pairs 
;;  representing the gap to be removed and test to see if there i an overlap. If
;;  there is then merge the break point pairs into one large gap. This way the 
;;  points will always fall on an object with one exception. If the gap is too near
;;  the end of an object one break point will be off the end and therefore that 
;;  point will need to be replaced with the end point.
;;    NOTE: in ACAD2000 the (vlax-curve-getdistatpoint function has proven unreliable
;;  so I have used (vlax-curve-getdistatparam in most cases
(defun break_obj (ent brkptlst BrkGap / brkobjlst en enttype maxparam closedobj
                  minparam obj obj2break p1param p2param brkpt2 dlst idx brkptS
                  brkptE brkpt result GapFlg result ignore dist tmppt
                  #ofpts 2gap enddist lastent obj2break stdist
                 )
  (or BrkGap (setq BrkGap 0.0)) ; default to 0
  (setq BrkGap (/ BrkGap 2.0)) ; if Gap use 1/2 per side of break point
  
  (setq obj2break ent
        brkobjlst (list ent)
        enttype   (cdr (assoc 0 (entget ent)))
        GapFlg    (not (zerop BrkGap)) ; gap > 0
        closedobj (vlax-curve-isclosed obj2break)
  )
  ;; when zero gap no need to break at end points, not closed
  (if (and (zerop Brkgap)(not closedobj)) ; Revision 2.2
    (setq spt (vlax-curve-getstartpoint ent)
          ept (vlax-curve-getendpoint ent)
          brkptlst (vl-remove-if '(lambda(x) (or (< (distance x spt) 0.0001)
                                                 (< (distance x ept) 0.0001)))
                                 brkptlst)
    )
  )
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
      
      ;;  setup brkptlst with pair of break pts ((p1 p2)(p3 p4)...)
      ;;  one of the pair of points will be on the object that
      ;;  needs to be broken
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
    ;| ;; ver 2.0 fix - removed this code as there are cases where the break point
       ;; is at the end of a line which is not a valid break but does no harm
    (if (and brkobjlst idx (minusp idx)
             (null (alert (strcat "Error - point not on object"
                                  "\nPlease report this error to"
                                  "\n   CAB at TheSwamp.org"))))
      (exit)
    )
    |;
    ;; (if (equal (if (null a)(setq a (car(entsel"\nTest Ent"))) a) ent) (princ)) ; debug CAB  -------------

    ;;  Handle any objects that can not be used with the Break Command
    ;;  using one point, gap of 0.000001 is used
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
      ;;  single breakpoint ----------------------------------------------------
      ;|(if (and closedobj ; problems with ACAD200 & this code
               (not (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (+ (vlax-curve-getDistAtPoint obj2break brkptS) 0.00001))))
          )
        (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (- (vlax-curve-getDistAtPoint obj2break brkptS) 0.00001)))
        
      )|;
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
  (GetNewEntities LastEntInDatabase) ; return list of enames of new objects
)
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      E N D   O F    M A I N   S U B R O U T I N E             
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;           M A I N   S U B   F U N C T I O N S                 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;======================
;;  Redraw ss with mode 
;;======================
(defun ssredraw (ss mode / i num)
  (setq i -1)
  (while (setq ename (ssname ss (setq i (1+ i))))
    (redraw (ssname ss i) mode)
  )
)

;;===========================================================================
;;  get all objects touching entities in the sscross                         
;;  limited obj types to "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"
;;  returns a list of enames
;;===========================================================================
(defun gettouching (sscros / ss lst lstb lstc objl)
  (and
    (setq lstb (vl-remove-if 'listp (mapcar 'cadr (ssnamex sscros)))
          objl (mapcar 'vlax-ename->vla-object lstb)
    )
    (setq
      ss (ssget "_A" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                           (cons 410 (getvar "ctab"))))
    )
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
                (setq lstc (cons (vlax-vla-object->ename x) lstc))
              )
            ) objl)
       ) lst)
  )
  lstc
)



;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;          E N D   M A I N    F U N C T I O N S                 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



;;===============================================
;;   Break all objects selected with each other  
;;===============================================
(defun c:BreakAll (/ cmd ss NewEnts AllEnts tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nSelect objects to break with each other & press enter: ")
  (if (setq ss (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
     (setq NewEnts (Break_with ss ss nil Bgap) ; ss2break ss2breakwith (flag nil = not to break with self)
           ; AllEnts (append NewEnts (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
           )
  )
  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)


;;===========================================
;;  Break a single object with other objects 
;;===========================================
(defun c:BreakObject (/ cmd ss1 ss2 tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )

  ;;  get objects to break
  (prompt "\nSelect single object to break: ")
  (if (and (setq ss1 (ssget "+.:E:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (redraw (ssname ss1 0) 3))
           (not (prompt "\n***  Select object(s) to break with & press enter:  ***"))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (redraw (ssname ss1 0) 4)))
     (Break_with ss1 ss2 nil Bgap) ; ss2break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)

;;==========================================
;;  Break many objects with a single object 
;;==========================================
(defun c:BreakWobject (/ cmd ss1 ss2 tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nSelect object(s) to break & press enter: ")
  (if (and (setq ss1 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 3))
           (not (prompt "\n***  Select single object to break with:  ***"))
           (setq ss2 (ssget "+.:E:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 4))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)


;;==========================================
;;  Break objects with objects on a layer   
;;==========================================
;;  New 08/01/2008
(defun c:BreakWlayer (/ cmd ss1 ss2 tmp lay)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\n***  Select single object for break layer:  ***")
  
  (if (and (setq ss2 (ssget "+.:E:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (setq lay (assoc 8 (entget (ssname ss2 0))))
           (setq ss2 (ssget "_X" (list
                                   '(0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                                   lay (cons 410 (getvar "ctab")))))
           (not (prompt "\nSelect object(s) to break & press enter: "))
           (setq ss1 (ssget (list
                              '(0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                              (cons 8 (strcat "~" (cdr lay))))))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)


;;======================================================
;;  Break selected objects with other selected objects  
;;======================================================
(defun c:BreakWith (/ cmd ss1 ss2 tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nBreak selected objects with other selected objects.")
  (prompt "\nSelect object(s) to break & press enter: ")
  (if (and (setq ss1 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 3))
           (not (prompt "\n***  Select object(s) to break with & press enter:  ***"))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 4))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)



;;=============================================
;;  Break objects touching selected objects    
;;=============================================

(defun c:BreakTouching (/ cmd ss1 ss2 tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nBreak objects touching selected objects.")
  (if (and (not (prompt "\nSelect object(s) to break & press enter: "))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (gettouching ss2))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)



;;=================================================
;;  Break touching objects with selected objects   
;;=================================================
;;  New 08/01/2008
(defun c:BreakWithTouching (/ cmd ss1 ss2 tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )

  ;;  get objects to break
  (prompt "\nBreak objects touching selected objects.")
  (prompt "\nSelect object(s) to break with & press enter: ")
  (if (and (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (setq tlst (gettouching ss2))
      )
    (progn
      (setq tlst (vl-remove-if '(lambda (x)(ssmemb x ss2)) tlst)) ;  remove if in picked ss
      (mapcar '(lambda (x) (ssadd x ss1)) tlst) ; convert to a selection set
      (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
    )
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)


;;==========================================================
;;  Break selected objects with any objects that touch it   
;;==========================================================


(defun c:BreakSelected (/ cmd ss1 ss2 tmp)
  
  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nBreak selected objects with any objects that touch it.")
  (if (and (not (prompt "\nSelect object(s) to break with touching & press enter: "))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (gettouching ss2))
      )
    (break_with ss2 ss1 nil Bgap) ; ss2break ss1breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)


;;==========================================================
;;  Break selected object with any objects that touch it  and remove every 
;;  other segment of the broken object, start with selected object 
;;==========================================================

(defun c:BreakRemove (/ cmd ss1 ss2 tmp entlst ename ent en e dist)
  
  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (setq Bgap 0
        tmp 1)
  ;;  get object to break
  (prompt "\nBreak selected object with any objects that touch it.")
  (if (and (not (prompt "\nSelect object to break with touching & press enter: "))
           (setq ss2 (ssget "_+.:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (setq touch (gettouching ss2)))
      )
    (setq entlst (break_with ss2 ss1 nil Bgap)) ; ss2break ss1breakwith (flag nil = not to break with self)
  )
  (if (not (member (setq ename (ssname ss2 0))  entlst))(setq entlst (append entlst (list ename))))
  (setq pt (cadr(cadddr(car (ssnamex ss2 0))))) ; pick point for ss2
  (foreach ent entlst ; find the new object that is at the picked spot
    (cond
      ((null dist)
       (if (setq p (vlax-curve-getclosestpointto ent pt))
         (setq dist (distance pt p)
               en ent)))
      ((setq p (vlax-curve-getclosestpointto ent pt))
       (if (< (distance pt p) dist)
         (setq dist (distance pt p)
               en ent)))
    ) 
  )
  (while (not(equal en (setq e (car entlst))))
    (setq entlst (append (cdr entlst) (list e))))
  
  (foreach ent entlst
      (if (zerop (setq tmp (- 1 tmp))) (entdel ent)))
  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
)



;; ***************************************************
;;     Function to create a dcl support file if it    
;;       does not exist                               
;;     Usage : (create_dcl "file name")               
;;     Returns : T if successful else nil             
;; ***************************************************
(defun create_Breakdcl (fname / acadfn dcl-rev-check)
  ;;=======================================
  ;;      check revision date Routine          
  ;;=======================================
  (defun dcl-rev-check (fn / rvdate ln lp)
    ;;  revision flag must match exactly and must
    ;;  begin with //
    (setq rvflag "//  Revision Control 05/12/2012@14:11" )
    (if (setq fn (findfile fn))
      (progn ; check rev date
        (setq lp 5) ; read 4 lines
        (setq fn (open fn "r")) ; open file for reading
        (while (> (setq lp (1- lp)) 0)
          (setq ln (read-line fn)) ; get a line from file
          (if (vl-string-search rvflag ln)
            (setq lp 0)
          )
        )
        (close fn) ; close the open file handle
        (if (= lp -1)
          nil ; no new dcl needed
          t ; flag to create new file
        )
      )
      t ; flag to create new file
    )
  )
  (if (null(wcmatch (strcase fname) "*`.DCL"))
    (setq fname (strcat fname ".DCL"))
  )
  (if (dcl-rev-check fname)
    ;; create dcl file in same directory as ACAD.PAT  
    (progn
      (setq acadfn (findfile "ACAD.PAT")
            fn (strcat (substr acadfn 1 (- (strlen acadfn) 8))fname)
            fn (open fn "w")
      )
      (foreach x (list
                   "// WARNING file will be recreated if you change the next line"
                   rvflag
                   "//BreakAll.DCL"
                   "BreakDCL : dialog { label = \"[ Break All or Some by CAB  v2.2 ]\";"
                   "  : text { label = \"--=<  Select type of Break Function needed  >=--\"; "
                   "           key = \"tm\"; alignment = centered; fixed_width = true;}"
                   "    spacer_1;"
                   "    : button { key = \"b1\"; mnemonic = \"T\";  alignment = centered;"
                   "               label = \"Break all objects selected with each other\";} "
                   "    : button { key = \"b2\"; mnemonic = \"T\"; alignment = centered;"
                   "               label = \"Break selected objects with other selected objects\";}"
                   "    : button { key = \"b3\"; mnemonic = \"T\";  alignment = centered;"
                   "               label = \" Break selected objects with any  objects that touch it\";}"
                   "    spacer_1;"
                   "  : row { spacer_0;"
                   "    : edit_box {key = \"gap\" ; width = 8; mnemonic = \"G\"; label = \"Gap\"; fixed_width = true;}"
                   "    : button { label = \"Help\"; key = \"help\"; mnemonic = \"H\"; fixed_width = true;} "
                   "    cancel_button;"
                   "    spacer_0;"
                   "  }"
                   "}"
                  ) ; endlist
        (princ x fn)
        (write-line "" fn)
      ) ; end foreach
      (close fn)
      (setq acadfn nil)
      (alert (strcat "\nDCL file created, please restart the routine"
               "\n again if an error occures."))
      t ; return True, file created
    )
    t ; return True, file found
  )
) ; end defun


;;==============================
;;     BreakAll Dialog Routine  
;;==============================
(defun c:MyBreak(/ dclfile dcl# RunDCL BreakHelp cmd txt2num)
   ;;  return number or nil
  (defun txt2num (txt / num)
    (if txt
    (or (setq num (distof txt 5))
        (setq num (distof txt 2))
        (setq num (distof txt 1))
        (setq num (distof txt 4))
        (setq num (distof txt 3))
    )
    )
    (if (numberp num)
      num
    )
  )
  (defun mydonedialog (flag)
    (setq DCLgap (txt2num (get_tile "gap")))
    (done_dialog flag)
  )
  (defun RunDCL (/ action)
    (or DCLgap (setq DCLgap 0)) ; error trap value
    (action_tile "b1" "(mydonedialog 1)")
    (action_tile "b2" "(mydonedialog 2)")
    (action_tile "b3" "(mydonedialog 3)")
    (action_tile "gap" "(setq DCLgap (txt2num value$))")
    (set_tile "gap" (rtos DCLgap))
    (action_tile "help" "(BreakHelp)")
    (action_tile "cancel" "(done_dialog 0)")
    (setq action (start_dialog))
    (or DCLgap (setq DCLgap 0)) ; error trap value
    (setq DCLgap (max DCLgap 0)) ; nu negative numbers
    
    (cond
      ((= action 1) ; BreakAll
         (command "_.undo" "_begin")
  ;;  get objects to break
  (prompt "\nSelect objects to break with each other & press enter: ")
  (if (setq ss (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
     (setq NewEnts (Break_with ss ss nil DCLgap) ; ss2break ss2breakwith (flag nil = not to break with self)
           ; AllEnts (append NewEnts (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
           )
  )
  (command "_.undo" "_end")
  (princ)
       )
      
      ((= action 2) ; BreakWith
         ;;  get objects to break
  (prompt "\nBreak selected objects with other selected objects.")
  (prompt "\nSelect object(s) to break & press enter: ")
  (if (and (setq ss1 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 3))
           (not (prompt "\n***  Select object(s) to break with & press enter:  ***"))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 4))
      )
    (break_with ss1 ss2 nil DCLgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

       )
      ((= action 3) ; BreakSelected
  (setq ss1 (ssadd))
  ;;  get objects to break
  (prompt "\nBreak selected objects with any objects that touch it.")
  (if (and (not (prompt "\nSelect object(s) to break with touching & press enter: "))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (gettouching ss2))
      )
    (break_with ss2 ss1 nil DCLgap) ; ss2break ss1breakwith (flag nil = not to break with self)
  )
       )
    )
  )
  (defun BreakHelp ()
    (alert
      (strcat
        "BreakAll.lsp				       (c) 2007-2012 Charles Alan Butler\n\n"
        "This LISP routine will break objects based on the routine you select.\n"
        "It will not break objects on locked layers and objects must have the same z-value.\n"
        "Object types are limited to LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE\n"
        "BreakAll -      Break all objects selected with each other\n"
        "BreakwObject  - Break many objects with a single object\n"
        "BreakObject -   Break a single object with many objects \n"
        "BreakWith -     Break selected objects with other selected objects\n"
        "BreakTouching - Break objects touching selected objects\n"
        "BreakSelected - Break selected objects with any objects that touch it\n"
        " The Gap distance is the total opening created.\n"
        "You may run each routine by entering the function name at the command line.\n"
        "For updates & comments contact Charles Alan Butler AKA CAB at TheSwamp.org.\n")
    )
  )
  
  ;;================================================================
  ;;                    Start of Routine                            
  ;;================================================================
  (vl-load-com)
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq dclfile "BreakAll.dcl")
  (cond
    ((not (create_Breakdcl dclfile))
     (prompt (strcat "\nCannot create " dclfile "."))
    )
    ((< (setq dcl# (load_dialog dclfile)) 0)
     (prompt (strcat "\nCannot load " dclfile "."))
    )
    ((not (new_dialog "BreakDCL" dcl#))
     (prompt (strcat "\nProblem with " dclfile "."))
    )
    ((RunDCL))      ; No DCL problems: fire it up
  )
  (and cmd (setvar "CMDECHO" cmd))
  (princ)
)
(prompt "Break routines loaded, Enter Mybreak to run.")
(princ)
;;/'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\.
;;    E n d   O f   F i l e   I f   y o u   A r e   H e r e       
;;/'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\./'\.

