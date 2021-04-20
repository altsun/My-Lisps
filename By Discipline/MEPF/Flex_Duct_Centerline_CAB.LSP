;;  Command Line Call, User picks pline centerline(s)
(defun c:Flex() (MyFlex nil ; no pre selected centerline
                        nil ; use default settings
                        ))

;;  Add Flex Duct by user picking 2 points on objects
;;  Objects must have curve properties to obtain perpendicular angle
(defun c:Flex2PointSpline ()
  (MyFlex "Spline" '((duct:dia 0.75)(collar 0.75) (FlexLayer "0")(FlexColor 1)))
)


;;  Add Flex Duct by user picking 2 points on objects
;;  Objects must have curve properties to obtain perpendicular angle
(defun c:Flex2PointPline ()
  (MyFlex "Pline" '((duct:dia 12.)(collar 4.) (FlexLayer "0")(FlexColor 1)))
)


;;;=======================[ FlexDuct.lsp ]==============================
;;; Author: Copyright© 2010 Charles Alan Butler (CAB)
;;; Contact or Updates  @  www.TheSwamp.org
;;; Version:  2.1   Mar 31,2010
;;; Purpose: Create Flex Duct from a centerline that the user picks
;;;    Centerline may be anything vla-curve will handle
;;; Sub_Routines:      
;;;    makePline which creates a LW Polyline
;;; Restrictions: UCS is supported
;;;    Duct Layer is hard coded, see var Flexlayer
;;;    Debug only error handler at this time
;;; Known Issues:
;;;    Tight curves cause pline jacket distortion
;;;    Added warning when this is about to occur
;;; Returns:  none
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


;;  Lisp entry point
(defun MyFlex (PLent      ; ename of pline or nil for user pick curve object or flag
                          ;;  flag = string "Pline" or "Spline"
               variables  ; List of pairs '((varName value)(varName value) )
               /
               cl-ent    ribWidth  RibShort  RibLong   collar
               dist      steps     ribFlag   pt        curAng    curDer
               RibPtLst1 RibPtLst2 p1        p2        doc       space
               cflag     cl-len    ribRadius tmp       NewPline  NewPline2 
               pl1       pl2       cnt       errflag   InsulThick   FlexColor
               FlexLayer ss        lyrent    90deg     *error*   FlexCLLayer   
              )
  (defun *error* (msg) (vl-bt))
  (vl-load-com)
  (setq Doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-endundomark doc)
  (vla-startundomark doc)

  ;; \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ;;  Variables set by calling routine must be in pairs, no error checking
  ;;  '((FlexLayer "Duct")(FlexColor 1)(collar 0))
  (if (vl-consp variables) 
    (mapcar (function (lambda (x)(set (car x) (cadr x)))) variables)
  )
  ;;  Default settings, Change these if you want
  
  (or FlexLayer  (setq FlexLayer   "0" )) ; put your Duct layer here
  (or FlexColor  (setq FlexColor   nil )) ; put your color over ride here or nil
  (or FlexCLLayer(setq FlexCLLayer "0" )) ; put your Duct Center Line layer here, "" or nil = no change
  (or InsulThick (setq InsulThick  0   )) ; to be added to duct diameter, use 2 for 1" insulation
  (or collar     (setq collar      0   )) ; collar length at each end
  (or DelCL      (setq DelCL       nil )) ; delete the centerline t=Yes nil=No
  (or GroupFlex  (setq GroupFlex   nil )) ; make flex duct a Group t=Yes nil=No
  (if DuctDiam ; override the first time only
    (or duct:dia (setq duct:dia DuctDiam)) ; Duct Diameter, global variable
    (or duct:dia (setq duct:dia     16.0)) ; Duct Diameter, global variable
  )
  ;; \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

  ;;   --------------------------------------
  ;;   --------   Local Functions   ---------
  ;;   --------------------------------------

  ;;  Expects pts to be a list of 2D or 3D points
  (defun makePline (spc pts)
    (if (= (length (car pts)) 2) ; 2d point list
      (setq pts (apply 'append pts))
      (setq
        pts (apply 'append (mapcar '(lambda (x) (list (car x) (cadr x))) pts))
      )
    )
    (setq
      pts (vlax-make-variant
            (vlax-safearray-fill
              (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length pts))))
              pts
            )
          )
    )
    (vla-addlightweightpolyline spc pts)
  )

  
  ;|
  ;;  by CAB 03/22/2009
  ;;  Expects pts to be a list of 2D or 3D points
  ;;  Returns new pline object
  (defun makePline (spc pts / norm elv pline)
    (setq norm (trans '(0 0 1) 1 0 T)
          elv  (caddr (trans (car pts) 1 norm))
    )
    ;(if (= (length (car pts)) 2) ; 2d point list
    ;  (setq pts (apply 'append pts))
    ;  (setq pts (apply 'append
    ;         (mapcar (function (lambda (x) (list (car x) (cadr x)))) pts))
    ;  )
    ;)

    (setq pline
           (vlax-invoke Spc 'addLightWeightPolyline
             (apply 'append
                    (mapcar (function (lambda (pt)
                               (setq pt (trans pt 1 norm))
                               (list (car pt) (cadr pt))
                             ))
                            pts)))
    )
    (vla-put-Elevation pline elv)
    (vla-put-Normal pline (vlax-3d-point norm))
    pline
  )
  |;

  (defun makeSpline (spc pts / norm elv spline fd)
    (setq norm (trans '(0 0 1) 1 0 T))
    (setq spline (vlax-invoke Spc 'AddSpline
             (apply 'append
                (mapcar (function (lambda (pt)
                     (setq pt (trans pt 1 norm)))) pts))
             '(0.0 0.0 0.0) '(0.0 0.0 0.0)
           )
    )
    (setq fd (vlax-curve-getFirstDeriv spline 0))
    (vlax-put spline 'StartTangent fd)
    (vlax-put spline 'EndTangent fd)
    spline
  )

  
  
  ;;   -------------------------------------
  ;;  Does not work in ACAD 2000
  (defun _CreateAnonymousGroup ( ) ; courtesy of Michael Puckett
      (vla-add 
          (vla-get-groups 
              (vla-get-activedocument (vlax-get-acad-object)))  "*")
  )

  ;;  CAB test to see if vlax-curve can be used on an object
  (defun curveOK (ent)                  ; returns nil if not allowed
    (not (vl-catch-all-error-p
           (vl-catch-all-apply 'vlax-curve-getendparam (list ent))))
  )

  ;;D. C. Broad, Jr. -------------------------------------------------
  ;;(sideof <ray-origin> <another-point-on-ray> <point-to-be-tested>)
  (defun SideOf (p1 p2 p / r)
    (setq r (cond
             ((equal p1 p 1e-10) 0)
             (t (sin (- (angle p1 p) (angle p1 p2))))
            )
    )
    (if (equal r 0 1e-10) 0 r)
  )
  ;;return values
  ;;negative = point is to the right side of the ray
  ;;0 = point is on the ray
  ;;otherwise point is on the left side of the ray.
  ;;P1 should not equal P2 for meaningful results.
  ;; -----------------------------------------------------------------

  (defun makeSpline (spc pts / norm elv spline fd)
    (setq norm (trans '(0 0 1) 1 0 T))
    (setq spline (vlax-invoke Spc 'AddSpline
             (apply 'append
                (mapcar (function (lambda (pt)
                     (setq pt (trans pt 1 norm)))) pts))
             '(0.0 0.0 0.0) '(0.0 0.0 0.0)
           )
    )
    (setq fd (vlax-curve-getFirstDeriv spline 0))
    (vlax-put spline 'StartTangent fd)
    (vlax-put spline 'EndTangent fd)
    spline
  )

  
  (defun GetNextPoint (pt pn ent dia / ang sFlag)
    (setq pt (vlax-curve-getclosestpointto ent pt))
    (setq ang (angle '(0 0)
                (vlax-curve-getFirstDeriv ent (vlax-curve-getParamAtPoint ent pt)))
    )
    (cond                               ; get perpendicular angle
      ((zerop (setq sFlag (Sideof pt (polar pt ang 10) pn)))
       (angle pt pn) ; temp, CAB debug
      )
      ((minusp sFlag)                   ; right side of the ray, CW= -90
       (setq ang (- ang (/ pi 2)))
      )
      ((setq ang (+ ang (/ pi 2))))     ; CCW=  +90
    )                                   ; cond
    (polar pt ang dia)
  )


  
  
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;;       S T A R T     H E R E    
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (while            ; Main Loop
    (progn
      (prompt (strcat "\nFlex diameter is set to " (vl-princ-to-string duct:dia)))
      (prompt (strcat "\nFlex collar is set to " (vl-princ-to-string collar)))
      (setvar "errno" 0) ; must pre set the errno to 0 
      (cond ; input section, get or make curve object for centerline
        ((= (type PLent) 'ENAME)
          (setq cl-ent (list PLent 0))) ; automatic mode
        
        ((null PLent)

          (initget "Diameter Collar")
          (setq cl-ent
             (entsel (strcat "\nSelect center line object for flex.[Diameter]<"
                             (vl-princ-to-string duct:dia)
                             "> Enter to quit."))))
        
        ;;====================================================================
        ((and (= (type PLent) 'STR)  (vl-position (strcase PLent) '("PLINE" "SPLINE")))
         (cond
           ((not(vl-consp (progn (initget "Diameter Collar")
              (setq p1 (getpoint (strcat "\nPick start point of flex.[Diameter]<"
                                         (vl-princ-to-string duct:dia) "> Enter to quit."))))))
            (or (setq cl-ent p1) (setvar "errno" 52))
           )
           
           ((not(vl-consp (progn (initget "Diameter Collar")
              (setq p2 (getpoint p1 "\nPick end point of flex.")))))
            (or (setq cl-ent p2) (setvar "errno" 52))
           )
           (t ; two points picked
           (if (and (setq ent (car (nentselp p1)))  (curveOK ent))
              (setq p1a (GetNextPoint p1 p2 ent duct:dia))
              (setq p1a (polar p1 (angle p1 p2) duct:dia))
           )
           (if (and (setq ent (car (nentselp p2))) (curveOK ent))
              (setq p2a (GetNextPoint p2 p1 ent duct:dia))
              (setq p2a (polar p2 (angle p2 p1) duct:dia))
           )

           (or Space (setq Space
              (if (= 1 (getvar "CVPORT"))
                 (vla-get-PaperSpace Doc)
                 (vla-get-ModelSpace Doc)
              )
           ))

           (if (= (strcase PLent) "SPLINE")
             (setq cl-ent (list (vlax-vla-object->ename 
              (makeSpline Space
                (list p1 p1a (polar p1a (angle p1 p1a) duct:dia)
                (polar p1a (angle p1a p2a) (/ (distance p1a p2a) 2))
                (polar p2a (angle p2 p2a) duct:dia) p2a p2)))0 ))
            
             (if (setq cl-ent
                    (makePline Space (list p1 p1a p2a p2)))
               (progn
                 (setq cl-ent (list (vlax-vla-object->ename cl-ent) 0))
                 (setq flr (getvar "FILLETRAD"))
                 (setq cmde (getvar "CmdEcho"))
                 (setvar "CmdEcho" 0)
                 (setvar "FILLETRAD" duct:dia) ; test CAB debug
                 (command "._fillet" "_P" (car cl-ent))
                 (setvar "FILLETRAD" flr)
                 (setvar "CmdEcho" cmde)
               )
             )            
           )
            )
          )
         )
         ;;====================================================================
        
      ) ; end cond

      ;;   
      (cond
         ((null (setq lyrent (tblobjname "layer" Flexlayer)))
          (prompt (strcat "\nDuct Layer " Flexlayer " does not exist."))
         )
         ((= 4 (logand 4 (cdr (assoc 70 (entget lyrent)))))
          (prompt (strcat "\nDuct Layer " Flexlayer " is LOCKED."))
         )
         ((and FlexCLlayer (/= FlexCLlayer "")
               (null (setq lyrent (tblobjname "layer" FlexCLlayer)))
               (princ (strcat "\n*** Center Line Layer " FlexCLlayer " does not exist. ***"))
               (setq FlexCLlayer nil))
         )
        ((= (getvar "errno") 52) ; exit if user pressed ENTER
         nil        ; exit loop
        )
        ((= cl-ent "Diameter")
         (initget (+ 2 4))
         (setq
           tmp (getdist
                 (strcat "\nSpecify duct diameter <" (rtos duct:dia) ">: ")))
         (and tmp (setq duct:dia tmp))
         (setq cl-ent nil)
         t          ; stay in loop
        )
        
        ((= cl-ent "Collar")
         (initget (+ 2 4))
         (setq
           tmp (getdist
                 (strcat "\nSpecify collar length <" (rtos collar 2 2) ">: ")))
         (and tmp (setq collar tmp))
         (setq cl-ent nil)
         t          ; stay in loop
        )

        ((vl-consp cl-ent)
         ;;  check entity before making the duct
         (if (not (vl-catch-all-error-p
                    (setq tmp (vl-catch-all-apply
                                'vlax-curve-getpointatparam
                                (list (car cl-ent) 0.0) ))))
           (progn   ; OK to make duct
             (setq cl-ent   (car cl-ent) ; Center Line
                   ribWidth (* duct:dia 0.167)
                   RibShort (+ duct:dia InsulThick) ; add insulation
                   RibLong  (+ RibShort (* ribWidth 2))
             )

             ;;  centerline length
             (setq cl-len (vlax-curve-getdistatparam
                            cl-ent
                            (vlax-curve-getendparam cl-ent)
                          )
                   steps  (/ cl-len ribWidth)
             )
             (if (= (logand (fix steps) 1) 1) ; T = odd
               (setq steps (fix steps))
               (setq steps (1+ (fix steps)))
             )

             ;;  This is designed to allow curve to find the start point on the CL
             ;;  and to terminat as near as possiable to the end of the CL
             (setq dist     (/ duct:dia 500.) ; start distance along center line
                   ribWidth (/ (- cl-len dist) (1- steps))
             )

            
             (setq ribFlag 0
                   cflag   t
                   cnt     0
                   pl1     nil
                   pl3     nil
                   errflag nil
                   90deg   (/ pi 2)
             )

             ;;  ----------   Create Rib End Points   -----------
             (repeat steps
               (or (setq pt (vlax-curve-getpointatdist cl-ent dist))
                   (princ)) ; debug
               ; (/ 1 0) debug - force error
               (setq
                 curDer (trans
                          (vlax-curve-getfirstderiv cl-ent
                            (vlax-curve-getparamatpoint cl-ent pt)) 0 1)
               )
               ;; Get angle 90 deg to curve
               (setq curAng (+ 90deg (angle '(0 0) curDer)))
               (setq ribRadius (if (zerop ribFlag) (/ RibShort 2) (/ RibLong 2)))
               (setq pt (trans pt 0 1)) ; WCS > UCS
               (setq p1 (polar pt curAng ribRadius))
               (setq p2 (polar pt (+ pi curAng) ribRadius))
               (if cflag ; create start collar points
                 (setq RibPtLst1 (list (polar p1 (angle curDer '(0 0)) collar))
                       RibPtLst2 (list (polar p2 (angle curDer '(0 0)) collar))
                       cflag     nil
                 )
               )

               ;;  this collection method creates a woven pline
               (cond
                 ((null pl1) ; first time through
                  (setq RibPtLst1 (cons p1 RibPtLst1)
                        RibPtLst2 (cons p2 RibPtLst2))
                 )
                 ((= (logand (setq cnt (1+ cnt)) 1) 1) ; T = odd cnt
                  (setq RibPtLst1 (cons pl2 RibPtLst1)
                        RibPtLst1 (cons p2 RibPtLst1)
                        RibPtLst2 (cons pl1 RibPtLst2)
                        RibPtLst2 (cons p1 RibPtLst2))
                 )
                 ((setq RibPtLst1 (cons pl1 RibPtLst1)
                        RibPtLst1 (cons p1 RibPtLst1)
                        RibPtLst2 (cons pl2 RibPtLst2)
                        RibPtLst2 (cons p2 RibPtLst2))
                 )
               )
               (if (and pl3 (inters p1 p2 pl3 pl4 t))
                 (setq errflag t)
               )
               (setq ribFlag (- 1 ribFlag) ; toggle flag
                     dist    (+ ribWidth dist)
                     pl3     pl1
                     pl4     pl2
                     pl1     p1
                     pl2     p2
               )
             )
             
             ;;  create end collar points
             (setq RibPtLst1 (cons p2 RibPtLst1)
                   RibPtLst1 (cons (polar p2 (angle '(0 0) curDer) collar) RibPtLst1)
                   RibPtLst2 (cons p1 RibPtLst2)
                   RibPtLst2 (cons (polar p1 (angle '(0 0) curDer) collar) RibPtLst2)
             )

             ;;  --------   point list to WCS   ------------
             (setq RibPtLst1 (mapcar (function (lambda (x) (trans x 1 0))) RibPtLst1))
             (setq RibPtLst2 (mapcar (function (lambda (x) (trans x 1 0))) RibPtLst2))

             ;;  --------   create jacket plines   ------------
             (or space
                 (setq space
                        (if (zerop (vla-get-activespace doc))
                          (if (= (vla-get-mspace doc) :vlax-true)
                            (vla-get-modelspace doc) ; active VP
                            (vla-get-paperspace doc)
                          )
                          (vla-get-modelspace doc)
                        )
                 )
             )

             (cond
               ((and errflag
                     (progn
                       (initget "Yes No")
                       (= "No"
                          (cond
                            ((getkword "\nTurns too tight, Proceed? [Yes/No]<Yes>:")) 
                            ("Yes")))
                       )
                     )
                t ; skip the create & stay in loop
               )
               ((setq newpline (makePline space RibPtLst1))
                (vla-put-layer newpline Flexlayer)
                (if FlexColor
                  (vla-put-color newpline FlexColor)
                )
                ;;(vla-put-elevation newpline z)

                (setq newpline2 (makePline space RibPtLst2))
                (vla-put-layer newpline2 Flexlayer)
                (if FlexColor
                  (vla-put-color newpline2 FlexColor)
                )
                ;;(vla-put-elevation newpline z)
                
                (if DelCL
                    (entdel cl-ent) ; remove the centerline object
                    (if (and FlexCLlayer (/= FlexCLlayer "")
                              (setq lyrent (tblobjname "layer" (cdr(assoc 8 (entget cl-ent)))))
                           (or (/= 4 (logand 4 (cdr (assoc 70 (entget lyrent)))))
                                  (prompt "\n*** Center Line layer is LOCKED ***"))
                       )
                      (vla-put-layer (vlax-ename->vla-object cl-ent) FlexCLlayer)
                    )
                  )
                ;| COMMAND method removed due to errors in ACAD2008
                (if GroupFlex
                  (progn
                    (setq ss (ssadd))
                    (ssadd (vlax-vla-object->ename newpline) ss)
                    (ssadd (vlax-vla-object->ename newpline2) ss)
                    (or DelCl (ssadd cl-ent ss))
                    (if (vl-cmdf "_.-group" "_create" "*" "" ss "")
                      (princ "\nGrouping Done")
                      (princ "\nError Grouping")
                    )
                  )
                )
                |;
                (if GroupFlex
                  (progn ; using Michael Puckett's method
                    (setq GroupObjects (list newpline newpline2))
                    (or DelCl (setq GroupObjects 
                               (cons (vlax-ename->vla-object cl-ent) GroupObjects)))
                    (setq myGroup (_CreateAnonymousGroup))
                    (vlax-invoke myGroup 'AppendItems GroupObjects)
                   )
                 )

               )
             ) ; cond
           )        ; progn
           (princ "\n** Error - Can not use that object, Try again.")
         )          ; endif
         (not PLent) ; exit flag, exit if PLent
        )
        (t (princ "\n***  Missed Try again  ***"))
      )             ; cond stmt
        
    )               ; progn - while
  )                 ; while
  (vla-endundomark doc)
  (and space (vlax-release-object space))
  (vlax-release-object doc)
  ;;-----------  E N D   O F   L I S P  ----------------------------
  (princ)
)
(prompt "\nFlex Lisp loaded, Enter FLEX to run.")
(princ)


