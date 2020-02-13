;|
 REVERSE - Reverses line, arc, circle, ellipse, spline, polyline, text,
           hatch pattern, or gradient fill.

   Reverses all line, arc, circle, ellipse, spline, polyline, and
   lightweight polyline entities.

   Circles, arcs, and ellipses are converted to polylines; other objects
   retain their respective entity type.  Circles and circular arcs are
   rendered as heavyweight polylines if the system variable plinetype = 0,
   or lightweight polylines if plinetype > 0.  Ellipses and elliptical
   arcs are rendered as high-resolution, curve-fit polylines with up to 64
   exact vertices and tangents.

   Reverses all text entities including single-line text, attributes,
   multiline text, dimension text, and remote text.  Fonts with leading or
   trailing white space, and text styles with upside down, backward, or
   vertical effects are fully supported.

   Rotates all hatch patterns 90� counterclockwise.  Rotates all gradient
   fills 180�. Solid fills are ignored.

   Single-line text, multiline text, attributes, hatch patterns, and
   gradient fills may be reversed within blocks.  All other block components
   are ignored.  Nested blocks are not supported.

   Preserves color, layer, linetype, linetype scale, plot style, lineweight,
   thickness, elevation, global width, and text style properties.  Preserves
   circular, quadratic, and cubic fitting.  Preserves vertex bulge, segment
   width, and vertex tangent.

 Copyright(c)2005-2011 Version 3.0(US)
   Tom Davis (tdavis@metzgerwillard.com)

 Revision History:

   08/19/10 - Added support for hatch patterns and gradient fills.
   01/27/10 - Removed references.
              See http://www.metzgerwillard.us/tdavis/lisp/reverse.html
   11/06/08 - Updated reference links.
   09/21/08 - Updated reference links.
   02/15/08 - Updated email address.
   09/09/07 - Added underscores for international language support.
   08/26/07 - Added support for mtext with exact line spacing; single-
              line text fonts with leading or trailing white space; and
              upside down, backward, and vertical text effects.
   08/05/07 - Modified to select objects with nentsel instead of entsel;
              extended support to all text objects in block references
              including attributes and dimension text.
   10/05/05 - Added limited dimension text support.
   09/22/05 - Added support for single-line text, multiline text, and
              remote text.
   09/17/05 - First release with initial support for all line, arc,
              circle, ellipse, spline, and polyline entities.
|;
;------------------------------------------------------------------------------
(defun c:RVS ( / oldecho oldsnap ent e etyp)
  ;reverse text, line, arc, circle, ellipse, spline, or polyline
  (setq oldecho (getvar "cmdecho")
        oldsnap (getvar "osmode")
  )
  (setvar "cmdecho" 0)                                           ;turn off echo
  (if (< oldsnap 16384) (setvar "osmode" (+ oldsnap 16384)))     ;turn off osnap
  (command "_select" "")                                         ;deselect all
  (while (setq ent (nentsel "\nSelect reversible object: "))
    (setq e    (car ent)
          etyp (cdr (assoc 0 (entget e)))
    )
    ;exclude block components that are neither text nor hatching
    (if (or (< (length ent) 4)(= etyp "TEXT")(= etyp "MTEXT")(= etyp "HATCH"))
      (progn
        (while (= (cdr (assoc 0 (entget e))) "VERTEX")           ;skip vertices
          (setq e (entnext e))
        )
        (if (= (cdr (assoc 0 (entget e))) "SEQEND")              ;get hwpolyline
          (setq e (cdr (assoc -2 (entget e))))                   ;    or ellipse
        )
        (setq etyp (cdr (assoc 0 (entget e))))
        (princ etyp)
        (command "_undo" "_begin")
        (cond
          ((= etyp "LWPOLYLINE")(revlwpline e))
          ((= etyp "POLYLINE")  (revhwpline e))
          ((= etyp "LINE")      (revline    e))
          ((= etyp "ARC")       (revarc     e))
          ((= etyp "CIRCLE")    (revcircle  e))
          ((= etyp "HATCH")     (revhatch   e))
          ((= etyp "ELLIPSE")   (revellipse e))
          ((= etyp "MTEXT")     (revmtext   e))
          ((= etyp "TEXT")      (revtext    e etyp))
          ((= etyp "ATTRIB")    (revtext    e etyp) (entupd e))
          ((= etyp "RTEXT")     (revrtext   e))
          ((= etyp "SPLINE")    (command "_splinedit" e "_e" ""))
        )
        (if (> (length ent) 3)(entupd (car (cadddr ent))))       ;block text
                                                                 ;or hatching
        (command "_undo" "_end")
      )
      (princ "INSERT")
    )
  )
  (setvar "cmdecho" oldecho)
  (setvar "osmode"  oldsnap)
  (princ)
)
;------------------------------------------------------------------------------
;LWPOLYLINE

(defun revlwpline (e / footer done vertices header flag)
  ;reverse lightweight polyline
  (foreach item (reverse (entget e))
    (cond
      ((not done)
        (cond
          ((= (car item) 40)
            (setq footer (cons (cons 41 (cdr item)) footer)      ;swap width
                  done t
            )
          )
          ((= (car item) 41)
            (setq footer (cons (cons 40 (cdr item)) footer))     ;swap width
          )
          ((= (car item) 42)
            (setq footer (cons (cons 42 (- (cdr item))) footer)) ;negate bulge
          )
          ((= (car item) 210)
            (setq footer (cons item footer))
          )
        )
      )
      ((= (car item) 10)
        (setq vertices (cons item vertices))
      )
      ((= (car item) 40)
        (setq vertices (cons (cons 41 (cdr item)) vertices))     ;swap width
      )
      ((= (car item) 41)
        (setq vertices (cons (cons 40 (cdr item)) vertices))     ;swap width
      )
      ((= (car item) 42)
        (setq vertices (cons (cons 42 (- (cdr item))) vertices)) ;negate bulge
      )
      (t (setq header (cons item header)))
    )
  )
  (setq flag (assoc 70 header))
  (if (< (cdr flag) 128)                 ;turn on linetype generation
    (setq header (subst (cons 70 (+ (cdr flag) 128)) flag header))
  )
  (entmod (append header (reverse vertices) footer))
)
;------------------------------------------------------------------------------
;POLYLINE

(defun revhwpline (e / oldname old ent1 buldge end start ent tangent radians
                       vertex vertices flag)
  ;reverse heavyweight polyline
  (setq oldname  e
        old   (entget oldname)
        e     (entnext e)
        ent1  (entget e)                 ;get first vertex
        bulge (cdr (assoc 42 ent1))
        end   (cdr (assoc 41 ent1))
        start (cdr (assoc 40 ent1))
        e     (entnext e)
        ent   (entget e)                 ;get second vertex
  )
  (while (= (cdr (assoc 0 ent)) "VERTEX")
    (if (= (logand (cdr (assoc 70 ent)) 2) 2)
      (setq tangent (assoc 50 ent)
            radians (- (cdr tangent) pi) ;reverse tangent
            ent     (subst (cons 50 radians) tangent ent)
      )
    )
    (setq vertex   (subst (cons 42 (- bulge))(assoc 42 ent) ent)    ;negate bulge
          vertex   (subst (cons 41 start)    (assoc 41 ent) vertex) ;swap width
          vertex   (subst (cons 40 end)      (assoc 40 ent) vertex) ;swap width
          bulge    (cdr  (assoc 42 ent))
          end      (cdr  (assoc 41 ent))
          start    (cdr  (assoc 40 ent))
          vertices (cons vertex vertices)
          e        (entnext e)
          ent      (entget e)            ;get next vertex or seqend
    )
  )
  (setq flag (assoc 70 old))
  (if (< (cdr flag) 128)                 ;turn on linetype generation
    (setq old (subst (cons 70 (+ (cdr flag) 128)) flag old))
  )
  (entmake old)                          ;make new polyline
  (foreach ent vertices (entmake ent))   ;make new vertices
  (if (= (logand (cdr (assoc 70 ent1)) 2) 2)
    (setq tangent (assoc 50 ent1)
          radians (- (cdr tangent) pi)   ;reverse tangent
          ent1    (subst (cons 50 radians) tangent ent1)
    )
  )
  (setq ent1 (subst (cons 42 (- bulge))(assoc 42 ent1) ent1) ;negate bulge
        ent1 (subst (cons 41 start)    (assoc 41 ent1) ent1) ;swap width
        ent1 (subst (cons 40 end)      (assoc 40 ent1) ent1) ;swap width
  )
  (entmake ent1)                         ;make last new vertex
  (entmake ent)                          ;make new seqend
  (entdel oldname)                       ;delete old polyline
)
;------------------------------------------------------------------------------
;LINE

(defun revline (e / ent start end)
  ;reverse line
  (setq ent   (entget e)
        start (assoc 10 ent)
        end   (assoc 11 ent)             ;swap line endpoints
        ent   (subst (cons 10 (cdr end)) start ent)
        ent   (subst (cons 11 (cdr start)) end ent)
  )
  (entmod ent)
)
;------------------------------------------------------------------------------
;ARC

(defun revarc (e)
  ;reverse arc
  (command "_pedit" e "_y" "_l" "_on" "");turn arc into polyline
  (setq e (entlast))
  (if (> (getvar "plinetype") 0)
    (revlwpline e)
    (revhwpline e)
  )
)
;------------------------------------------------------------------------------
;CIRCLE

(defun revcircle (e / ent radius center pt1 pt2)
  ;reverse circle
  (setq ent    (entget e)
        radius (cdr (assoc 40 ent))
        center (cdr (assoc 10 ent))
        pt1    (mapcar '+ center (list radius 0 0))
        pt2    (mapcar '- center (list radius 0 0))
  )
  (command "_break" e pt1 pt2)                ;turn circle into semicircle
  (command "_pedit" e "_y" "_l" "_on" "_c" "");turn semicircle into closed polyline
  (setq e (entlast))
  (if (> (getvar "plinetype") 0)
    (revlwpline e)
    (revhwpline e)
  )
)
;------------------------------------------------------------------------------
;HATCH

(defun revhatch (e / ent solid item ang pi2 new y)
  ;reverse hatch
  (setq ent   (entget e)
        solid (cdr (assoc 70 ent))                   ;solid fill flag
        pi2   (* 2 pi)
  )
  (cond
    ((= solid 0)                                     ;pattern fill
      (foreach item (reverse ent)
        (cond
          ((or (= (car item) 52) (= (car item) 53))  ;pattern or line angle
            (setq ang (+ (* pi 0.5) (cdr item)))     ;rotate 90�
            (if (>= ang pi2) (setq ang (- ang pi2))) ;normalize angle
            (setq new (cons (cons (car item) ang) new))
          )
          ((or (= (car item) 43) (= (car item) 45))  ;line origin or offset x
            ;rotate line origin or offset 90�: new y = old x; new x = - old y
            (setq new (cons (cons (1+ (car item)) (cdr item)) new)
                  new (cons (cons (car item) (- y)) new))
          )
          ((or (= (car item) 44) (= (car item) 46))  ;line origin or offset y
            (setq y (cdr item))
          )
          (t (setq new (cons item new)))
        )
      )
      (entmod new)
    )
    ((= solid 1)                                     ;solid fill
      (if (= (cdr (assoc 450 ent)) 1)                ;gradient fill
        (progn
          (setq item (assoc 460 ent)                 ;gradient angle
                ang  (+ pi (cdr item))               ;rotate 180�
          )
          (if (>= ang pi2) (setq ang (- ang pi2)))   ;normalize angle
          (setq ent (subst (cons 460 ang) item ent))
          (entmod ent)
        )
      )
    )
  )
)
;------------------------------------------------------------------------------
;RTEXT

(defun revrtext (e / ent ins w h rot ang hd vd new)
  ;reverse rtext
  (command "_explode" e)                         ;explode rtext into mtext
  (setq ent  (entget (entlast))                  ;get mtext
        w    (cdr (assoc 42 ent))                ;width
        h    (cdr (assoc 43 ent))                ;height
  )
  (command "_undo" 1)
  (setq ent  (entget e)                          ;get rtext
        ins  (assoc 10 ent)                      ;insertion point
        rot  (assoc 50 ent)                      ;rotation
        ang  (cdr rot)
        hd   (polar '(0 0 0)    ang           w) ;horizontal displacement
        vd   (polar '(0 0 0) (- ang (/ pi 2)) h) ;vertical displacement
        new  (mapcar '+ (cdr ins) hd vd)         ;new insertion point
        ang  (rem (+ ang pi) (* 2 pi))           ;normalize angle
        ent  (subst (cons 50 ang) rot ent)       ;reverse direction
        ent  (subst (cons 10 new) ins ent)       ;set new insertion point
  )
  (entmod ent)
)
;------------------------------------------------------------------------------
;TEXT or ATTRIB

(defun revtext (e etyp / vc ent box hj vj rot ang p1 p2 h w
                         dist phi hd vd new gf gfs sn p s done)
  ;reverse text or attribute
  (if (= etyp "TEXT")
    (setq vc 73) ;text
    (setq vc 74) ;attribute
  )
  (setq ent (entget e)
        box (textbox ent)                              ;((x1 y1 z1)(x2 y2 z2))
        gf  (cdr (assoc 71 ent))                       ;generation flag
        sn  (cdr (assoc  7 ent))                       ;style name
        hj  (cdr (assoc 72 ent))                       ;horizontal justification
        vj  (cdr (assoc vc ent))                       ;vertical justification
        rot (assoc 50 ent)                             ;rotation
        ang (cdr rot)                                  ;angle
        p1  (assoc 10 ent)                             ;first  alignment point
        p2  (assoc 11 ent)                             ;second alignment point
        h   (cdr (assoc 40 ent))                       ;displacement height
        p   1                                          ;rewind pointer
  )
  (while (not done)                      ;traverse style table
    (setq s   (tblnext "Style" p)
          p   nil                        ;reset pointer
    )
    (if (= sn (cdr (assoc 2 s)))         ;find style name
      (progn
        (setq done t
              gfs  (cdr (assoc 71 s))    ;style generation flag
        )
        (if (= (logand (cdr (assoc 70 s)) 4) 4)
          (setq gf (1+ gf))              ;vertical
        )
      )
    )
  )
  (if  (= gfs (logand gf gfs)) ;exclude conflicting generation flags
    (progn
      (cond                                            ;displacement width
        ((= hj 0)                        ;left
          (setq w (+ (caadr box) (caar box)))
        )
        (t                               ;otherwise
          (setq dist (distance (cdr p1) (cdr p2))
                phi  (angle    (cdr p1) (cdr p2))
                dist (abs (* dist (cos (- phi ang))))
          )
          (if (= (logand gf 2) 2) (setq dist (- dist)))       ;backward
          (if (or (= hj 5) (= hj 3))
            (setq w (-(+ (caar box) (caadr box))      dist))  ;fit or aligned
            (setq w (-(+ (caar box) (caadr box)) (* 2 dist))) ;right, center, middle
          )
        )
      )
      (if (= vj 1)                       ;bottom
        (setq dist (distance (cdr p1) (cdr p2))
              phi  (angle    (cdr p1) (cdr p2))
              dist (abs(* dist (sin (- phi ang))))     ;descender depth
              h    (+ h (* 2 dist))
        )
      )
      (if (= (logand gf 1) 1)            ;vertical
        (cond
          ((or (> hj 2) (= hj 1))                      ;center,aligned,middle,fit
            (setq h 0)
          )
          (t                                           ;otherwise
            (setq h (- (cadadr box) (cadar box)))
            (if (= (+ hj vj) 0) (setq h (- h)))        ;baseline left
            (cond
              ((and (= hj 0) (> vj 0)) (setq vj 3))    ;bottom,middle,top left
              ((= hj 2) (setq vj 0))                   ;right
            )
          )
        )
      )
      (if (= (logand gf 4) 4) (setq h (- h)))          ;upside down
      (setq hd   (polar '(0 0 0)    ang           w)   ;horizontal displacement
            vd   (polar '(0 0 0) (+ ang (/ pi 2)) h)   ;vertical displacement
      )
      (cond                              ;compute new alignment point
        ((or (and (= vj 0) (= hj 1))     ;center
             (and (= vj 0) (= hj 2))     ;right
             (= vj 1))                   ;bottom
          (setq new (mapcar '+ (cdr p2) hd vd))
        )
        ((or (= vj 2) (= hj 4))          ;middle
          (setq new (mapcar '+ (cdr p2) hd))
        )
        ((= vj 3)                        ;top
          (setq new (mapcar '+ (cdr p2) hd)
                new (mapcar '- new vd)
          )
        )
      )
      (cond
        ((= (+ hj vj) 0)                 ;left
          (setq new (mapcar '+ (cdr p1) hd vd)
                ent (subst (cons 10 new) p1 ent)         ;set new alignment point
                ent (subst (cons 50 (+ ang pi)) rot ent) ;reverse direction
          )
        )
        ((or (= hj 5) (= hj 3))          ;fit or aligned
          (setq new (mapcar '+ (cdr p2) vd hd)
                ent (subst (cons 10 new) p1 ent)         ;swap alignment points
                new (mapcar '+ (cdr p1) vd hd)
                ent (subst (cons 11 new) p2 ent)
          )
        )
        (t
          (setq ent (subst (cons 11 new) p2 ent)         ;set new alignment point
                ent (subst (cons 50 (+ ang pi)) rot ent) ;reverse direction
          )
        )
      )
      (entmod ent)
    )
    (alert (strcat "The selected text object is not compatible with\n"
                   "its text style.  When the text style is upside\n"
                   "down or backwards, the text object should also    \n"
                   "be upside down or backwards."))
  )
)
;------------------------------------------------------------------------------
;MTEXT (including dimension text)

(defun revmtext (e / ent ins w h just lss ls ch rot hd vd new)
  ;reverse mtext or dimension text
  (setq ent  (entget e)
        ins  (assoc 10 ent)              ;insertion point
        w    (cdr (assoc 42 ent))        ;width
        h    (cdr (assoc 43 ent))        ;displacement height
        just (cdr (assoc 71 ent))        ;justification
        rot  (assoc 50 ent)              ;rotation
        lss  (cdr (assoc 73 ent))        ;line spacing style
        ch   (cdr (assoc 40 ent))        ;character height
        ls   (/ ch 3)                    ;interline half-space
       ;ls = (5 ch/3 - ch)/2 = ch/3
  )
  (cond
    ((and (= lss 2) (> just 6))          ;exact bottom
      (setq h (+ h ls))
    )
    ((and (= lss 2) (< just 4))          ;exact top
      (setq h (- h ls))
    )
    ((= lss 2)                           ;exact middle
      (setq h ls)
    )
    ((and (> just 3) (< just 7))         ;at least middle
      (setq h 0)
    )
  )
  (setq hd   (polar '(0 0 0)    (cdr rot)           w) ;horizontal displacement
        vd   (polar '(0 0 0) (- (cdr rot) (/ pi 2)) h) ;vertical displacement
  )
  (cond                                  ;compute new insertion point
    ((= just 1)                          ;top left
      (setq new (mapcar '+ (cdr ins) hd vd))
    )
    ((= just 2)                          ;top center
      (setq new (mapcar '+ (cdr ins) vd))
    )
    ((= just 3)                          ;top right
      (setq new (mapcar '- (cdr ins) hd)
            new (mapcar '+ new vd)
      )
    )
    ((= just 4)                          ;middle left
      (setq new (mapcar '+ (cdr ins) hd)
            new (mapcar '- new vd)
      )
    )
    ((= just 5)                          ;middle center
      (setq new (mapcar '- (cdr ins) vd))
    )
    ((= just 6)                          ;middle right
      (setq new (mapcar '- (cdr ins) hd vd))
    )
    ((= just 7)                          ;bottom left
      (setq new (mapcar '+ (cdr ins) hd)
            new (mapcar '- new vd)
      )
    )
    ((= just 8)                          ;bottom center
      (setq new (mapcar '- (cdr ins) vd))
    )
    ((= just 9)                          ;bottom right
      (setq new (mapcar '- (cdr ins) hd vd))
    )
  )
  (setq ent (subst (cons 10 new) ins ent)              ;set new insertion point
        ent (subst (cons 50 (+ (cdr rot) pi)) rot ent) ;reverse direction
  )
  (entmod ent)
)
;------------------------------------------------------------------------------
;ELLIPSE

(defun revellipse (e / old oldent center p1 ratio start end major a b rot
                       minor inc tol 2pi i j phi closed p tan ent flag)
  ;reverse ellipse
  (setq old    e
        oldent (entget old)
        center (cdr (assoc 10 oldent))
        p1     (cdr (assoc 11 oldent))
        ratio  (cdr (assoc 40 oldent))
        start  (cdr (assoc 41 oldent))
        end    (cdr (assoc 42 oldent))
        major  (mapcar '+ center p1)
        a      (distance center major)
        b      (* ratio a)
        rot    (angle center major)
        minor  (polar center (+ rot (/ pi 2)) b)
  )
  (setq inc 64                           ;number of vertices on full ellipse
        tol 1e-5                         ;closure tolerance
        2pi (* 2 pi)
        i   (1+ (fix (+ (* (/ inc 2pi) start) 0.5))) ;start index
        j       (fix (+ (* (/ inc 2pi)   end) 0.5))  ;end index
        phi (list start)
  )
  (while (< i j)                         ;build parameter list
    (setq phi (cons (* (/ 2pi inc) i) phi)
          i   (1+ i)
    )
  )
  (if (and (< start tol) (< (abs (- end 2pi)) tol))
    (setq closed t)
    (setq closed nil
          phi    (cons end phi)
    )
  )
  ;parametric ellipse in object coordinate system
  ;  x = a cos(q);  y = b sin(q);  r = b/a
  ;  dx/dq = -a sin(q);  dy/dq = b cos(q)
  ;  dy/dx = -b/a cot(q) = -r^2 x/y
  ;  tangent direction = atan(dy/dx)

  (setq p   (mapcar '(lambda (q)         ;compute OCS points on ellipse
                       (list (* a (cos q)) (* b (sin q)))
                     )
                     phi
            )
        tan (mapcar '(lambda (q)         ;compute WCS tangent directions
                       (+ (atan (* (- (expt ratio 2)) (car q)) (cadr q)) rot)
                     )
                     p
            )
  )
  (command "_ucs" "_n" 3 center major minor)         ;create OCS
  (setq p (mapcar '(lambda (q)(trans q 1 0)) p))     ;transform from OCS to WCS
  (command "_ucs" "_p")                              ;restore UCS
  (command "_pline")
  (mapcar 'command p)
  (command "")
  (command "_matchprop" old (entlast) "")
  (if closed
    (command "_pedit" (entlast) "_l" "_on" "_c" "_f" "");force hwpline creation
    (command "_pedit" (entlast) "_l" "_on"      "_f" "")
  )
  (setq e   (entnext (entlast))
        ent (entget e)                   ;get first vertex
        i   0
  )
  (while (= (cdr (assoc 0 ent)) "VERTEX")
    (setq flag (assoc 70 ent))
    (if (/= (logand (cdr flag) 1) 1)     ;skip curve fitting vertices
      (progn                             ;set tangent and flag bit
        (setq ent (subst (cons 50 (nth i tan)) (assoc 50 ent) ent)
              i   (1+ i)
              ent (subst (cons 70 (+ (cdr flag) 2)) flag ent)
        )
        (entmod ent)
      )
    )
    (setq e   (entnext e)
          ent (entget e)                 ;get next vertex or seqend
    )
  )
  (command "_pedit" (entlast) "_f" "")   ;update fit
  (entdel old)                           ;delete ellipse
)
(princ)
