;;  CableTray.lsp [command name: CT]
;;  To draw a Cable Tray, with User-specified tray width, inside radius,
;;  cross-bar spacing, and center-line Polyline path.  Draws single-line
;;  Polyline side rails and Line cross-bars.  Places cross-bars at ends; insets
;;  next cross-bars in with excess length equally divided between ends.
;;  Kent Cooper, November 2010
;
(defun C:CT
  (/ *error* ctreset rtd ctdir osm cmde blips widtemp radtemp
  divtemp cthalfwid ctcl ctcllength ctclvla ctbars ctinset incr ctpt)
;
  (defun *error* ()
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); end if
    (command)
    (command "_.undo" "_end")
    (ctreset)
  ); end defun - *error*
;
  (defun ctreset ()
    (setvar 'osmode osm)
    (setvar 'blipmode blips)
    (setvar 'cmdecho cmde)
  ); end defun - ctreset
;
  (defun rtd (rad); radians to degrees
    (/ (* rad 180) pi)
  ); end defun - rtd
;
  (defun ctdir () ; local DIRection of path at ctpt location
    (rtd ; calculate local direction
      (angle
        '(0 0 0)
        (trans
          (vlax-curve-getFirstDeriv
            ctcl
            (vlax-curve-getParamAtPoint ctcl (trans ctpt 1 0))
          ); end getFirstDeriv
          0 1 T ; world to current CS, as displacement
        ); end trans
      ); end angle
    ); end rtd
  ); end defun - ctdir
;
  (vl-load-com)
  (command "_.undo" "_begin")
;
  (setq
    osm (getvar 'osmode)
    cmde (getvar 'cmdecho)
    blips (getvar 'blipmode)
  ); end setq
;
  (if _ctwid_
    (initget 6); then - no zero, no negative
    (initget 7); else - no zero, no negative, no Enter on first use
  ); end if
  (setq
    widtemp
      (getdist ; [returns nil on Enter]
        (strcat
          "\nEnter [or pick two points for] tray width"
          (if _ctwid_ (strcat " <" (rtos _ctwid_) ">") ""); default if present
          ": "
        ); end strcat
      ); end getdist and divtemp
    _ctwid_ (if widtemp widtemp _ctwid_)
      ; User entry other than Enter; if Enter, use default
    cthalfwid (/ _ctwid_ 2)
  ); end setq
;
  (if _ctrad_
    (initget 6); then - no zero, no negative
    (initget 7); else - no zero, no negative, no Enter on first use
  ); end if
  (setq
    radtemp
      (getdist ; [returns nil on Enter]
        (strcat
          "\nEnter [or pick two points for] inside-edge radius"
          (if _ctrad_ (strcat " <" (rtos _ctrad_) ">") ""); default if present
          ": "
        ); end strcat
      ); end getdist and radtemp
    _ctrad_ (if radtemp radtemp _ctrad_)
      ; User entry other than Enter; if Enter, use default
  ); end setq
;
  (if _ctdiv_
    (initget 6); then - no zero, no negative
    (initget 7); else - no zero, no negative, no Enter on first use
  ); end if
  (setq
    divtemp
      (getdist ; [returns nil on Enter]
        (strcat
          "\nEnter [or pick two points for] crossbar spacing"
          (if _ctdiv_ (strcat " <" (rtos _ctdiv_) ">") ""); default if present
          ": "
        ); end strcat
      ); end getdist and divtemp
    _ctdiv_ (if divtemp divtemp _ctdiv_)
      ; User entry other than Enter; if Enter, use default
  ); end setq
;
  (command "_.pline")
  (prompt "\nDraw Cable-Tray center-line Polyline -- start point: ")
  (while (> (getvar "cmdactive") 0) (command pause))
  (setvar 'osmode 0)
  (setvar 'blipmode 0)
  (setvar 'cmdecho 0)
  (command "_.fillet" "_r" (+ _ctrad_ cthalfwid) "_.fillet" "_p" (entlast))
  (setq
    ctcl (entlast)
    ctcllength (vlax-curve-getDistAtParam ctcl (vlax-curve-getEndParam ctcl))
    ctclvla (vlax-ename->vla-object ctcl)
    ctbars (1+ (fix (/ ctcllength _ctdiv_)))
    ctinset (/ (rem ctcllength _ctdiv_) 2)
  ); end setq
  (if (< 0 ctinset (/ _ctdiv_ 2))
    (setq ctbars (1- ctbars) ctinset (+ ctinset (/ _ctdiv_ 2)))
  ); end if
  (vla-offset ctclvla cthalfwid)
  (vla-offset ctclvla (- cthalfwid))
  (command
    "_.line"
    (polar (getvar 'viewctr) (/ pi 2) cthalfwid)
    (polar (getvar 'lastpoint) (* pi 1.5) _ctwid_)
    ""
    "_.copybase" (getvar 'viewctr) (entlast) ""
  ); end command
  (entdel (entlast))
;
  (setq incr -1)
  (repeat ctbars
    (setq
      incr (1+ incr)
      ctpt
        (trans
          (vlax-curve-getPointAtDist
            ctcl
            (+ (* incr _ctdiv_) ctinset)
          ); end getPointAtDist and ctpt
          0 1
        ); end trans and ctpt
    ); end setq
    (command "_.pasteclip" "_rotate" (ctdir) ctpt)
  ); end repeat
  (setq ctpt (trans (vlax-curve-getStartPoint ctcl) 0 1))
  (command "_.pasteclip" "_rotate" (ctdir) ctpt)
  (setq ctpt (trans (vlax-curve-getEndPoint ctcl) 0 1))
  (command "_.pasteclip" "_rotate" (ctdir) ctpt)
  (entdel ctcl)
;
  (command "_.undo" "_end")
  (ctreset)
  (princ)
); end defun - C:CT
;
(prompt "Type CT to draw a Cable Tray.")
