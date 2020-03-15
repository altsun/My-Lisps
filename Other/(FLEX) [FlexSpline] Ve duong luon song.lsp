;;;  FlexSpline.LSP [Command name: FLEX]

;;;  To draw a Spline approximation of a sine wave Centered on a path, such as is used by many
;;;    to represent flexible duct or conduit.  [Not a precisely accurate sine-wave shape, but close.]
;;;  Draws continuous Spline along fit points zig-zagging across center of path object.
;;;  [Non-tangent changes in direction in Polyline paths, or overly tight curves relative to wave
;;;    width in various entity types, will yield quirky results.  Allows selecting or drawing planar
;;;    3D Polyline, "because it can," however unlikely that one would want to because of this; if 3D
;;;    Polyline is fully collinear, will pick a UCS related to current one.  Forbids 3D Spline.]
;;;  Divides path length into half-wave-length increments to make waves end at path ends; forces
;;;    an even number of half-waves for closed paths so that result is complete S-waves continuous
;;;    across start/end point.
;;;  Draws on current Layer, unless Deleting pre-Existing path; if so, draws on its Layer, assuming
;;;    the intent is to replace it (i.e. to replace something drawn only to establish path), but pre-drawn
;;;    path is not necessary -- User can create path within command.
;;;  Under select-Existing option, asks User to select again if nothing is selected, or if selected object
;;;    is an inappropriate entity type.
;;;  Accounts for different Coordinate Systems of selected existing path objects.
;;;  Remembers option choices and offers them as defaults on subsequent use.
;;;  Options:
;;;    1.  draw new within routine, or select pre-Existing, path of any planar finite type with linearity;
;;;    2.  if selected Existing object is on locked Layer, whether to unlock it and Proceed, or Quit;
;;;    3.  PRevious = redraw along prior path (whether retained or deleted) allowing different choices;
;;;    4.  Retain or Delete base path (whether new or selected Existing);
;;;    5.  sine-wave Width (overall/outer);
;;;    6.  sine-wave full-S-cycle approximate Length as ratio of width (actual length will be adjusted to
;;;         fit overall path length), offering same value as Width (ratio of 1.0) as initial default.
;;;  Kent Cooper, last edited 17 July 2015

(vl-load-com)

(defun flexreset ()
  (mapcar 'setvar svnames svvals)
  (vla-endundomark doc)
); defun -- flexreset

(defun C:FLEX
  (/ *error* doc svnames svvals pathent pathdata pathtype 3DP u3p1 u3p2
    u3p3pt u3p3 ptno ucschanged pathlength hwsegs steplength ptdist)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (if ucschanged (vl-cmdf "_.ucs" "_prev"))
      ; ^ i.e. don't go back unless routine reached UCS change but didn't change it back
    (flexreset)
  ); defun -- *error*

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (setq
    svnames '(cmdecho osmode blipmode ucsfollow clayer)
    svvals (mapcar 'getvar svnames); current System Variable values
  ); setq

  (initget
    (strcat
      "Existing Line Arc Circle Pline ELlipse Spline 3dpoly"
      (if *flex* " PRevious" ""); add PR option only if not first use
    ); strcat
  ); initget
  (setq *flextype*
    (cond
      ( (getkword
          (strcat
            "\nPath type [Existing or draw Line(single)/Arc/Circle/Pline(2D)/ELlipse/Spline/3dpoly"
            (if *flex* "/PRevious" ""); offer PR option if not first use
            "] <"
            (cond (*flextype*) ("Line")); prior choice default if present; otherwise Line
            ">: "
          ); strcat
        ); getkword
      ); User option condition
      (*flextype*); Enter with prior choice
      ("Line"); Enter on first use
    ); cond & *flextype*
  ); setq

  (cond ; re-establish/select/make path
    ((= *flextype* "PRevious")
      (if (= *flexdel* "Delete")
        (progn ; then
          (if *isLocked* (command "_.layer" "_unlock" *pathlay* ""))
            ; unlock layer w/o asking [Proceed option chosen before] so it can:
          (entdel *flexpath*); bring back previous path
        ); progn
      ); if
      (entdel *flex*); delete previous result
        ; under Retain-path option where it draws on current Layer, not that of path object,
        ; will fail IF current Layer is locked [not accounted for as being highly unlikely];
        ; under Delete-path option where it draws on object's Layer, already unlocked above
    ); PRevious-path condition
    ((= *flextype* "Existing")
      (while
        (not
          (and
            (setq
              pathent (car (entsel "\nSelect object to draw sine-wave along an Existing path: "))
              pathdata (if pathent (entget pathent))
              pathtype (if pathent (substr (cdr (assoc 100 (reverse pathdata))) 5))
                ; ^ = entity type from second (assoc 100) without "AcDb" prefix;  using this because
                ; (assoc 0) value is the same for 2D & 3D Polylines; 2D OK, but 3D only if planar (if
                ; not, result would be flattened in current CS
            ); setq
            (or
              (not (wcmatch pathtype "Spline,3dPolyline")); other types than Spline/3DPoly
              (and (wcmatch pathtype "Spline,3dPolyline") (vlax-curve-isPlanar pathent)); only if flat
            ); or
          ); and
        ); not
        (prompt "\nNothing selected, or not a finite planar path type; try again:")
      ); while
    ); Existing-object condition
    ((= *flextype* "Line") (command "_.line" pause pause "")); only single Line
    ((/= *flextype* "PRevious"); all other draw-new entity types
      (command (strcat "_." *flextype*)); start drawing command
      (while (> (getvar 'cmdactive) 0) (command pause)); complete it
    ); draw-new condition
  ); cond

  (mapcar 'setvar svnames '(0 0 0 0)); turn off System Variables after path selected/drawn

  (setq
    *flexpath* ; set object as base path [not localized, so it can be brought back if PR and D options]
      (cond
        ((= *flextype* "Existing") pathent); selected object
        ((= *flextype* "PRevious") *flexpath*); keep the same
        ((entlast)); otherwise, newly drawn path
      ); cond & *flexpath*
    pathdata (entget *flexpath*)
    pathtype (substr (cdr (assoc 100 (reverse pathdata))) 5)
  ); setq
  (if
    (and ; User drew Spline or 3DPolyline that is not planar
      (or
        (= pathtype "Spline")
        (setq 3DP (= pathtype "3dPolyline")); separately from Spline, for UCS alignment
      ); or
      (not (vlax-curve-isPlanar *flexpath*))
    ); and
    (progn ; then
      (prompt "\nCannot use non-planar Spline/3DPolyline.")
      (flexreset) (quit)
    ); progn -- then [no else]
  ); if
  (setq
    *pathlay* (cdr (assoc 8 pathdata))
      ; ^ not localized, so that under PRevious option, knows what layer to unlock if needed
    *isLocked* ; not localized, so that under PRevious option, don't need to ask again
      (if (and (= *flextype* "PRevious") *isLocked*)
        T ; keep with PR if previous object was on locked layer [will be unlocked by now]
        (= (logand (cdr (assoc 70 (tblsearch "layer" *pathlay*))) 4) 4)
          ; other types: 0 = Unlocked [nil]; 4 = Locked [T]
      ); if & *isLocked*
  ); setq

  (initget "Retain Delete")
  (setq *flexdel*
    (cond
      ( (getkword
          (strcat
            "\nRetain or Delete base path [R/D] <"
            (if *flex* (substr *flexdel* 1 1) "D"); prior choice default, Delete on first use
            ">: "
          ); strcat
        ); getkword
      ); User-input condition
      (*flexdel*); Enter with prior choice
      ("Delete"); Enter on first use
    ); cond & *flexdel*
  ); setq

  (if (and *isLocked* (= *flexdel* "Delete"))
    (if (/= *flextype* "PRevious"); then -- check for not redoing on previous object  
      (progn ; then -- ask whether to unlock
        (initget "Proceed Quit")
        (setq *flexunlk*
          (cond
            ( (getkword
                (strcat
                  "\nLayer is locked; temporarily unlock and Proceed, or Quit? [P/Q] <"
                  (if *flexunlk* (substr *flexunlk* 1 1) "P"); prior choice default, Proceed on first use
                  ">: "
                ); strcat
              ); getkword
            ); User-input condition
            (*flexunlk*); Enter with prior choice
            ("Proceed"); Enter on first use
          ); cond & *flexunlk*
        ); setq
        (if (= *flexunlk* "Proceed")
          (command "_.layer" "_unlock" *pathlay* ""); then
          (progn (flexreset) (quit)); else
        ); if
      ); progn & inner then argument
    ); inner if & outer then argument
  ); outer if -- no else argument [no issue if not on locked layer with Delete option]

  (initget (if *flexwid* 6 7)); no zero, no negative, no Enter on first use
  (setq *flexwid*
    (cond
      ( (getdist
          (strcat
            "\nWidth (overall) of sine wave"
            (if *flexwid* (strcat " <" (rtos *flexwid*) ">") ""); default only if not first use
              ; EDIT to add mode & precision above if current Units settings not desired
            ": "
          ); strcat
        ); getdist
      ); User-input condition
      (*flexwid*); Enter with prior value
    ); cond & *flexwid*
  ); setq

  (initget 6); no zero, no negative
  (setq *flexlenratio* ; LENgth of full wave as RATIO of width
    (cond
      ( (getreal
          (strcat
            "\nFull-S cycle length as ratio of width <"
            (if *flexlenratio* (rtos *flexlenratio* 2 3) "1.0"); EDIT precision as desired
              ; prior as default only if present; 1 as default on first use
            ">: "
          ); strcat
        ); getreal
      ); User-input condition
      (*flexlenratio*); Enter with prior value
      (1.0); Enter on first use
    ); cond & *flexlenratio*
  ); setq

  (if 3DP
    (progn ; then -- 3DPolyline doesn't define UCS under object option, so:
      (setq
        u3p1 (trans (vlax-curve-getStartPoint *flexpath*) 0 1); first vertex
        u3p2 (trans (vlax-curve-getPointAtParam *flexpath* 1) 0 1); second
        u3p3pt (vlax-curve-getPointAtParam *flexpath* (setq ptno 2))
          ; third [if present] -- WCS point only, un-translated [(trans) fails if nil]
      ); setq
      (if u3p3pt ; at least 3 vertices
        (progn ; then
          (while
            (and
              u3p3pt ; still a remaining vertex
              (not (inters u3p1 u3p2 u3p1 (setq u3p3 (trans u3p3pt 0 1)))); collinear with first 2 vertices
            ); and
              ; will usually finish at 3rd vertex; if first 3 are collinear, will look further
              ;   until it finds one not collinear with 1st & 2nd
            (setq u3p3pt (vlax-curve-getPointAtParam *flexpath* (setq ptno (1+ ptno))))
              ; next vertex [finally nil if all collinear to end]
          ); while
          (if (not u3p3pt); reached end without finding non-collinear point
            (setq u3p3 ""); accept offered default for 3rd point [UCS will be parallel to current UCS]
          ); if
        ); progn -- then
        (setq u3p3 ""); else [single-segment] -- accept offered default for 3rd point [as above]
      ); if [3 or more vertices vs. 2]
      (command "_.ucs" "_new" "_3point" u3p1 u3p2 u3p3); use vertices to define UCS
    ); progn -- then [3DP]
    (command "_.ucs" "_new" "_object" *flexpath*); else [other types] -- set UCS to match object
  ); if
  (setq
    ucschanged T ; marker for *error* to reset UCS if routine doesn't get to it
    ptno 0 ; starting value for intermediate point multiplier [replace prior use if 3DP]
  ); setq

  (setq
    pathlength (vlax-curve-getDistAtParam *flexpath* (vlax-curve-getEndParam *flexpath*))
    hwsegs ; closed path needs even number of Half-Wave SEGmentS; open can have odd number
      (if (vlax-curve-isClosed *flexpath*)
        (* (fix (+ (/ pathlength *flexwid* *flexlenratio*) 0.5)) 2); then -- round to nearest *even* number
        (fix (+ (/ pathlength *flexwid* *flexlenratio* 0.5) 0.5)); else -- round to nearest *whole* number
      ); if & hwsegs
    steplength (/ pathlength hwsegs 2)
  ); setq

  (if (and (wcmatch *flextype* "Existing,PRevious") (= *flexdel* "Delete")) (setvar 'clayer *pathlay*))
    ; if Deleting Existing/PRevious path, draw on same Layer [already will for new path]

  (command "_.spline" (trans (vlax-curve-getStartPoint *flexpath*) 0 1)); [leave in Spline command]
  (repeat (1- (* hwsegs 2)); for Fit Points not including final at end
    (command ; feed out to Spline command:
      (polar
        (trans
          (vlax-curve-getPointAtDist ; advance point along path
            *flexpath*
            (setq ptdist (* (setq ptno (1+ ptno)) steplength))
          ); getPointAtDist
          0 1 ; from World coordinates to object's coordinates
        ); trans
        (+ ; localized angle of offset
          (angle
            '(0 0 0)
            (trans
              (vlax-curve-getFirstDeriv
                *flexpath*
                (vlax-curve-getParamAtDist *flexpath* ptdist)
              ); getFirstDeriv
              0 1 T ; world to current CS, as displacement
            ); trans
          ); angle
          (/ pi 2); perpendicular to center-line path [sine +/- gives left/right]
        ); +
        (* *flexwid* 0.5 (sin (* (/ pi 2) (rem ptno 4))))
          ; proportion of sine-wave width [alternates between +/- 0.5 & 0]
      ); polar
    ); command
  ); repeat
  (if (vlax-curve-isClosed *flexpath*); finish Spline
    ; (command "_close" ""); then
       ; Close option used because end-at-path's-end 'else' line below results in "kinked"
       ;   start/end on most closed paths.  Using plain Close line above, accepting default
       ;   for tangent direction, though start/end "flows" across, result has noticeable variance
       ;   there from typical wave shape elsewhere.  Following is a compromise tangent-
       ;   direction determination, better than default above but slightly too steep in some
       ;   situations and not steep enough in others.  There is no universal ideal -- it would
       ;   differ depending on degrees of curvature within first & last wavelengths of path.
    (command ; then [closed path]
      "_close"
      (polar ; tangent-direction found as before, but farther out from last 'ptno' location
        (trans
          (vlax-curve-getPointAtDist
            *flexpath*
            (setq ptdist (* ptno steplength)); re-use from last wave-crest fit point
          ); getPointAtDist
          0 1
        ); trans
        (+
          (angle
            '(0 0 0)
            (trans
              (vlax-curve-getFirstDeriv
                *flexpath*
                (vlax-curve-getParamAtDist *flexpath* ptdist)
              ); getFirstDeriv
              0 1 T
            ); trans
          ); angle
          (/ pi 2)
        ); +
        (* *flexwid* (sin (* (/ pi 2) (rem ptno 4)))); tangent-direction defining point:
          ; outboard of last wave crest, twice as far from path, for compromise location
      ); polar
    ); command
    (command (trans (vlax-curve-getEndPoint *flexpath*) 0 1) "" "" ""); else [open-ended]
  ); if

  (command "_.ucs" "_prev")
  (setq
    ucschanged nil ; eliminate UCS reset in *error* since routine did it already
    *flex* (entlast); save result in case of recall of routine with PRevious option
  ); setq
  (if (= *flexdel* "Delete") (entdel *flexpath*)); remove base path under Delete option

  (if *isLocked* (command "_.layer" "_lock" *pathlay* "")); re-lock layer if appropriate
  (flexreset)
  (princ)
); defun -- FLEX

(prompt "Type FLEX to draw sine-wave-approximation Spline centered along path.")