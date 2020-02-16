;;---------------------=={ Automatic Block Break }==--------------------;;
;;                                                                      ;;
;;  This program enables the user to insert a block at a specified      ;;
;;  point, with surrounding geometry trimmed to the rectangular outline ;;
;;  of the block. Optionally, the program will automatically rotate     ;;
;;  the block to align with a curve object passing through the block    ;;
;;  insertion point.                                                    ;;
;;                                                                      ;;
;;  The program first prompts the user to select a block to insert. At  ;;
;;  this prompt the user may select a block from the drawing, or type   ;;
;;  'Browse' to browse for a drawing file to insert. The user may also  ;;
;;  alter the program rotation setting by typing 'Rotation' at the      ;;
;;  prompt.                                                             ;;
;;                                                                      ;;
;;  Following a valid response, the program prompts the user to specify ;;
;;  an insertion point for the block.                                   ;;
;;                                                                      ;;
;;  If a curve object (Arc, Elliptical Arc, Ellipse, Circle, Line,      ;;
;;  XLine, Spline, LWPolyline or Polyline) is detected at the block     ;;
;;  insertion point and the program rotation setting is enabled, the    ;;
;;  inserted block is automatically rotated to align with the curve.    ;;
;;                                                                      ;;
;;  All surrounding compatible objects found to intersect with the      ;;
;;  inserted block are then trimmed to the rectangular block outline.   ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-11-22                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2011-02-07                                      ;;
;;                                                                      ;;
;;  - Entire program rewritten to allow subfunction to be called with   ;;
;;    block object argument.                                            ;;
;;  - Multiple intersecting objects are now trimmed.                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2011-02-08                                      ;;
;;                                                                      ;;
;;  - Changed block insertion to Visual LISP InsertBlock method.        ;;
;;  - Added calling functions to trim blocks in-situ (ABBE / ABBS).     ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2011-08-03                                      ;;
;;                                                                      ;;
;;  - Altered method used to create bounding polyline to exclude        ;;
;;    attributes when trimming objects surrounding block.               ;;
;;  - Objects surrounding blocks whose insertion point does not lie on  ;;
;;    a curve are now also trimmed.                                     ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2011-09-30                                      ;;
;;                                                                      ;;
;;  - Added option to enable/disable automatic block rotation.          ;;
;;  - Updated code formatting.                                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2013-03-04                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing program to attempt to break MLines.             ;;
;;  - Added new LM:blockboundingbox function to correctly calculate     ;;
;;    the bounding box of Dynamic Block references.                     ;;
;;  - Entire program rewritten.                                         ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2013-10-01                                      ;;
;;                                                                      ;;
;;  - Fixed a bug causing the program to return an error if an object   ;;
;;    intersects the block bounding box at only one point.              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.7    -    2014-11-22                                      ;;
;;                                                                      ;;
;;  - Changed the method used to detect objects located at the block    ;;
;;    insertion point when determining the angle for automatic          ;;
;;    block rotation.                                                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.8    -    2017-10-09                                      ;;
;;                                                                      ;;
;;  - Revised calculation of block bounding box to ignore objects       ;;
;;    residing on frozen layers.                                        ;;
;;  - Added option to specify block name at command-line.               ;;
;;----------------------------------------------------------------------;;

(defun c:abb ( / *error* blk obj ins sel tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
            (princ "\nCurrent layer locked.")
        )
        (   (progn
                (while
                    (not
                        (progn
                            (setvar 'errno 0)
                            (initget "Browse Name Rotation")
                            (princ (strcat "\nAutomatic Block Rotation: " (getenv "LMac\\ABBRotation")))
                            (setq sel
                                (entsel
                                    (strcat "\nSelect Block [Browse/Name/Rotation]"
                                        (if (= "" (setq blk (getvar 'insname)))
                                            ": "
                                            (strcat " <" blk ">: ")
                                        )
                                    )
                                )
                            )
                            (cond
                                (   (= 7 (getvar 'errno))
                                    (prompt "\nMissed, Try Again.")
                                )
                                (   (null sel)
                                    (not (if (= "" blk) (setq blk nil)))
                                )
                                (   (= "Rotation" sel)
                                    (initget "ON OFF")
                                    (setenv "LMac\\ABBRotation"
                                        (cond
                                            (   (getkword
                                                    (strcat "\nAutomatic Block Rotation [ON/OFF] <"
                                                        (getenv "LMac\\ABBRotation") ">: "
                                                    )
                                                )
                                            )
                                            (   (getenv "LMac\\ABBRotation")   )
                                        )
                                    )
                                    nil
                                )
                                (   (= "Name" sel)
                                    (while
                                        (not
                                            (or
                                                (= ""
                                                    (setq tmp
                                                        (getstring t
                                                            (strcat "\nSpecify block name"
                                                                (if (= "" blk) ": " (strcat " <" blk ">: "))
                                                            )
                                                        )
                                                    )
                                                )
                                                (tblsearch "block" tmp)
                                            )
                                        )
                                        (princ (strcat "\nBlock \"" tmp "\" is not defined in the current drawing."))
                                    )
                                    (cond
                                        (   (/= "" tmp) (setq blk tmp))
                                        (   (/= "" blk))
                                    )
                                )
                                (   (= "Browse" sel)
                                    (setq blk (getfiled "Select Block" "" "dwg" 16))
                                )
                                (   (listp sel)
                                    (if (/= "INSERT" (cdr (assoc 0 (entget (car sel)))))
                                        (prompt "\nObject must be a block.")
                                        (setq obj (vla-copy (vlax-ename->vla-object (car sel)))
                                              blk (LM:blockname obj)
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (not (or blk obj))
            )
        )
        (   (setq ins (getpoint (strcat "\nSpecify insertion point for " (vl-filename-base blk) " block: ")))
            (LM:startundo (LM:acdoc))
            (if (null obj)
                (setq obj
                    (vla-insertblock
                        (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                        (vlax-3D-point (trans ins 1 0))
                        blk
                        1.0
                        1.0
                        1.0
                        (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 (trans '(0.0 0.0 1.0) 1 0 t) t))
                    )
                )
            )
            (if blk (setvar 'insname (vl-filename-base blk)))
            (vla-put-insertionpoint obj (vlax-3D-point (trans ins 1 0)))
            (LM:autoblockbreak (vlax-vla-object->ename obj) (= "ON" (getenv "LMac\\ABBRotation")))
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;;----------------=={ Automatic Block Break Existing }==----------------;;
;;                                                                      ;;
;;  This program enables the user to select an existing block and trim  ;;
;;  all surrounding geometry to the rectangular outline of the block.   ;;
;;  Optionally, the program will automatically rotate the selected      ;;
;;  block to align with a curve object passing through the block        ;;
;;  insertion point.                                                    ;;
;;                                                                      ;;
;;  At the block selection prompt, the user may also alter the program  ;;
;;  rotation setting by typing 'Rotation' when prompted.                ;;
;;                                                                      ;;
;;  If a curve object (Arc, Elliptical Arc, Ellipse, Circle, Line,      ;;
;;  XLine, Spline, LWPolyline or Polyline) is detected at the block     ;;
;;  insertion point of the selected block and the program rotation      ;;
;;  setting is enabled, the block is automatically rotated to align     ;;
;;  with the curve.                                                     ;;
;;                                                                      ;;
;;  All surrounding compatible objects found to intersect with the      ;;
;;  selected block are then trimmed to the rectangular block outline.   ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;

(defun c:abbe ( / *error* enx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn
            (setvar 'errno 0)
            (initget "Rotation")
            (princ (strcat "\nAutomatic Block Rotation: " (getenv "LMac\\ABBRotation")))
            (setq sel (entsel "\nSelect Block to Trim [Rotation]: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, Try Again.")
                )
                (   (= "Rotation" sel)
                    (initget "ON OFF")
                    (setenv "LMac\\ABBRotation"
                        (cond
                            (   (getkword
                                    (strcat "\nAutomatic Block Rotation [ON/OFF] <"
                                        (getenv "LMac\\ABBRotation") ">: "
                                    )
                                )
                            )
                            (   (getenv "LMac\\ABBRotation")   )
                        )
                    )
                )
                (   (= 'ename (type (car sel)))
                    (cond
                        (   (/= "INSERT" (cdr (assoc 0 (setq enx (entget (car sel))))))
                            (princ "\nObject must be a block.")
                        )
                        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (cdr (assoc 8 enx)))))))
                            (princ "\nSelected block is on a locked layer.")
                        )
                        (   t
                            (LM:startundo (LM:acdoc))
                            (LM:AutoBlockBreak (car sel) (= "ON" (getenv "LMac\\ABBRotation")))
                            (LM:endundo   (LM:acdoc))
                        )
                    )
                    t
                )
            )
        )
    )
    (princ)
)

;;---------------=={ Automatic Block Break Selection }==----------------;;
;;                                                                      ;;
;;  This program enables the user to select multiple existing blocks    ;;
;;  and automatically trim all surrounding geometry to the rectangular  ;;
;;  outline of each block. Optionally, the program will automatically   ;;
;;  rotate each block in the selection to align with curve objects      ;;
;;  detected to pass through the block insertion point.                 ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;

(defun c:abbs ( / *error* inc rot sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (setq rot (= "ON" (getenv "LMac\\ABBRotation")))
    (if (setq sel (ssget "_:L" '((0 . "INSERT"))))
        (progn
            (LM:startundo (LM:acdoc))
            (repeat (setq inc (sslength sel))
                (LM:AutoBlockBreak (ssname sel (setq inc (1- inc))) rot)
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;;---------------=={ Automatic Block Break SubFunction }==--------------;;
;;                                                                      ;;
;;  Upon calling the function with a VLA Block Reference Object         ;;
;;  argument, the function will trim all surrounding compatible         ;;
;;  geometry found to intersect with the rectangular outline of the     ;;
;;  supplied block reference.                                           ;;
;;                                                                      ;;
;;  Furthermore, if a curve object is detected to pass through (or in   ;;
;;  the vicinity of) the block insertion point and the rotation flag    ;;
;;  argument holds a non-nil value, the supplied block reference is     ;;
;;  rotated to align with the curve.                                    ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright � 2010 - www.lee-mac.com                 ;;
;;----------------------------------------------------------------------;;
;;  Arguments:                                                          ;;
;;  ent - Block Reference Entity                                        ;;
;;  rot - Rotation flag (if T, block is aligned to curve)               ;;
;;----------------------------------------------------------------------;;

(defun LM:autoblockbreak ( ent rot / *error* _furthestapart ang bbx brk cmd crv di1 di2 enx idx ins int lst ply sel tmp )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type ply)) (vlax-write-enabled-p ply))
            (vla-delete ply)
        )
        (if (= 'int (type cmd))
            (setvar 'cmdecho cmd)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (defun _furthestapart ( lst / dis mxd out pt1 pt2 )
        (setq mxd 0.0)
        (while (setq pt1 (car lst))
            (foreach pt2 (setq lst (cdr lst))
                (if (< mxd (setq dis (distance pt1 pt2)))
                    (setq mxd dis
                          out (list pt1 pt2)
                    )
                )
            )
        )
        out
    )
  
    (if (and (= 'ename (type ent))
             (setq enx (entget ent))
             (= "INSERT" (cdr (assoc 0 enx)))
        )
        (progn
            (if
                (and rot
                    (setq bbx (LM:blockboundingbox (vlax-ename->vla-object ent)))
                    (setq sel
                        (ssget "_C"
                            (trans (car   bbx) 0 1)
                            (trans (caddr bbx) 0 1)
                           '((0 . "ARC,ELLIPSE,CIRCLE,LINE,XLINE,SPLINE,*POLYLINE"))
                        )
                    )
                    (progn
                        (setq ins (trans (cdr (assoc 10 enx)) ent 0)
                              crv (ssname sel (1- (sslength sel)))
                              di1 (distance ins (vlax-curve-getclosestpointto crv ins))
                        )
                        (repeat (setq idx (1- (sslength sel)))
                            (setq tmp (ssname sel (setq idx (1- idx))))
                            (if (< (setq di2 (distance ins (vlax-curve-getclosestpointto tmp ins))) di1)
                                (setq di1 di2
                                      crv tmp
                                )
                            )
                        )
                        (< di1 1e-4)
                    )
                )
                (progn
                    (setq ang
                        (angle '(0.0 0.0 0.0)
                            (trans
                                (vlax-curve-getfirstderiv crv
                                    (vlax-curve-getparamatpoint crv
                                        (vlax-curve-getclosestpointto crv ins)
                                    )
                                )
                                0 (cdr (assoc 210 (entget crv)))
                            )
                        )
                    )
                    (if (and (< (/ pi 2.0) ang) (<= ang (/ (* 3.0 pi) 2.0)))
                        (setq ang (+ ang pi))
                    )
                    (vla-put-rotation (vlax-ename->vla-object ent) ang) ;; VL used to account for attributes
                )
            )
            (if
                (and
                    (setq bbx (LM:blockboundingbox (vlax-ename->vla-object ent)))
                    (setq sel
                        (ssget "_C"
                            (trans (car   bbx) 0 1)
                            (trans (caddr bbx) 0 1)
                           '((0 . "ARC,ELLIPSE,CIRCLE,LINE,XLINE,SPLINE,*POLYLINE"))
                        )
                    )
                )
                (progn
                    (setq ply
                        (vlax-ename->vla-object
                            (entmakex
                                (append
                                    (list
                                       '(000 . "LWPOLYLINE")
                                       '(100 . "AcDbEntity")
                                       '(100 . "AcDbPolyline")
                                       '(090 . 4)
                                       '(070 . 1)
                                        (cons 38 (cadddr (assoc 10 enx)))
                                    )
                                    (mapcar '(lambda ( p ) (mapcar '+ (cons 10 (trans p 0 ent)) '(0 0 0))) bbx)
                                    (list (assoc 210 enx))
                                )
                            )
                        )
                    )
                    (repeat (setq idx (sslength sel))
                        (setq ent (ssname sel (setq idx (1- idx))))
                        (if (setq int (LM:Intersections (vlax-ename->vla-object ent) ply acextendthisentity))
                            (setq lst (cons (cons ent int) lst))
                        )
                    )
                    (vla-delete ply)
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (foreach int lst
                        (if (setq brk (_furthestapart (cdr int)))
                            (command
                                "_.break" (list  (car int) (trans (car brk) 0 1)) "_F"
                                "_non"    (trans (car  brk) 0 1)
                                "_non"    (trans (cadr brk) 0 1)
                            )
                        )
                    )
                    (setvar 'cmdecho cmd)
                )
            )
        )
    )
    (princ)
)

;; Intersections  -  Lee Mac
;; Returns a list of all points of intersection between two objects.
;; obj1,obj2 - [vla] VLA-Objects with intersectwith method applicable
;; mode      - [int] acextendoption enum of intersectwith method
;; Returns: [lst] List of 3D WCS intersection points, else nil

(defun LM:intersections ( obj1 obj2 mode / l r )
    (setq l (vlax-invoke obj1 'intersectwith obj2 mode))
    (repeat (/ (length l) 3)
        (setq r (cons (list (car l) (cadr l) (caddr l)) r)
              l (cdddr l)
        )
    )
    (reverse r)
)

;;-------------------=={ Block BoundingBox }==----------------;;
;;                                                            ;;
;;  Returns a point list describing a rectangular frame       ;;
;;  bounding all geometry of a supplied block reference.      ;;
;;  Excludes Text, MText & Attribute Definitions.             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright � 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  blk - VLA Block Reference Object                          ;;
;;------------------------------------------------------------;;
;;  Returns: WCS Point list describing boundingbox of block   ;;
;;------------------------------------------------------------;;

(defun LM:blockboundingbox ( blk / bnm llp lst urp )
    (setq bnm (strcase (vla-get-name blk)))
    (cond
        (   (setq lst (cdr (assoc bnm LM:blockboundingbox:cache))))
        (   (progn
                (vlax-for obj (vla-item (LM:acblk) bnm)
                    (cond
                        (   (= "AcDbBlockReference" (vla-get-objectname obj))
                            (setq lst (append lst (LM:blockboundingbox obj)))
                        )
                        (   (and
                                (= :vlax-true (vla-get-visible obj))
                                (not (wcmatch (vla-get-objectname obj) "AcDbAttributeDefinition,AcDb*Text"))
                                (vlax-method-applicable-p obj 'getboundingbox)
                                (= :vlax-false (vla-get-freeze (vla-item (LM:aclyr) (vla-get-layer obj))))
                                (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                            )
                            (setq lst (vl-list* (vlax-safearray->list llp) (vlax-safearray->list urp) lst))
                        )
                    )
                )
                lst
            )
            (setq lst (mapcar '(lambda ( fun ) (apply 'mapcar (cons fun lst))) '(min max)))
            (setq lst
                (list
                    (car lst)
                    (list (caadr lst) (cadar lst))
                    (cadr lst)
                    (list (caar lst) (cadadr lst))
                )
            )
            (setq LM:blockboundingbox:cache (cons (cons bnm lst) LM:blockboundingbox:cache))
        )
    )
    (apply
        (function
            (lambda ( m v )
                (mapcar (function (lambda ( p ) (mapcar '+ (mxv m p) v))) lst)
            )
        )
        (refgeom (vlax-vla-object->ename blk))
    )
)

;; RefGeom (gile)
;; Returns a list which first item is a 3x3 transformation matrix (rotation,
;; scales, normal) and second item the object insertion point in its parent
;; (xref, block or space)
;;
;; Argument : an ename

(defun refgeom ( ent / ang ang mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
)

;; Matrix x Vector - Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix Transpose - Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix - Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Block Name  -  Lee Mac
;; Returns the true (effective) name of a supplied block reference
                        
(defun LM:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun LM:blockname ( obj ) (vla-get-effectivename obj))
        (defun LM:blockname ( obj ) (vla-get-name obj))
    )
    (LM:blockname obj)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;; Block Collection  -  Lee Mac
;; Returns the VLA Block Collection Object

(defun LM:acblk nil
    (eval (list 'defun 'LM:acblk 'nil (vla-get-blocks (LM:acdoc))))
    (LM:acblk)
)

;; Layer Collection  -  Lee Mac
;; Returns the VLA Layer Collection Object

(defun LM:aclyr nil
    (eval (list 'defun 'LM:aclyr 'nil (vla-get-layers (LM:acdoc))))
    (LM:aclyr)
)

;;----------------------------------------------------------------------;;

(if (null (getenv "LMac\\ABBRotation"))
    (setenv "LMac\\ABBRotation" "ON")
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: AutoBlockBreak.lsp | Version 1.8 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"ABB\" - Insert & Break | \"ABBE\"/\"ABBS\" - Break Existing ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;