(vl-load-com)
(if (null etrim) (load "extrim.lsp"))
(defun c:TRBL ( /  ent1   ssT1 ssT2  TenBlock LtsDsten ssBl Layer   Dem  loop e en Pmax_min P1a ObjRectang Lts1 Lts2 DsTenloc LtsNameBlock Name1);;;TRIM BLOCK
(setvar "CMDECHO" 0)
(defun *error* ( msg )
		(if Olmode (setvar 'osmode Olmode))
		(if (not (member msg '("*BREAK,*CANCEL*,*EXIT*")))
		    (princ (strcat "\nError: " msg))
		)
		(princ)
)

(setq Olmode (getvar "osmode"));;;LUU CHE DO BAT DIEM OSNAP
(command "undo" "begin")
(setvar "OSMODE" 0)
(setq Lts1 (list))
(setq Lts2 (list))
(Alert "Qu\U+00E9t ch\U+1ECDn c\U+00E1c lo\U+1EA1i  Block c\U+1EA7n l\U+1ECDc")
(setq ssBl (acet-ss-to-list (ssget (list (cons 0 "INSERT")))))
(foreach e1 ssBl
	(setq Name1 (cdr (assoc 2 (entget e1))))
  	(setq Lts2 (append Lts2 (list Name1)))

)

  
(setq LtsNameBlock (FILTERNAME Lts2))
  
(if LtsNameBlock
    (progn
      	(setq DsTenloc (LM:lst->str LtsNameBlock ","))
      	(Alert (strcat "\nT\U+1EADp BLOCK s\U+1EBD \U+0111\U+01B0\U+1EE3c ch\U+1ECDn c\U+00F3 t\U+00EAn l\U+00E0: " DsTenloc
		       "\n\U+0110\U+1EC3 ti\U+1EBFp t\U+1EE5c, m\U+1EDDi b\U+1EA1n qu\U+00E9t ch\U+1ECDn \U+0111\U+1ED1i t\U+01B0\U+1EE3ng"
	       )
	)
	(setq ssT1  (ssget (list (cons 0 "INSERT") (cons 2 DsTenloc))))
        (if (not ssT1) (setq ssT1  (ssget"_X" (list (cons 0 "INSERT") (cons 2 DsTenloc)))))
;;;      	(vla-zoomextents (vlax-get-acad-object))
	(if ssT1
		(Progn
		  	(setq ssT2  (SortXY (acet-ss-to-list ssT1 )))
			(foreach en ssT2
				(setq Pmax_min (lst-getboundingbox
				   (mapcar (function vlax-ename->vla-object)
				           (vl-remove-if (function listp) (mapcar (function cadr) (ssnamex (acet-list-to-ss (list en)))))
				   ) 
				  ) 
				 )
			  	(command "rectang" (car Pmax_min) (cadr Pmax_min))
			  	(setq ObjRectang (entlast))
			  	(setq e (vlax-ename->vla-object en))
				(setq P1a (vlax-get e 'InsertionPoint))
			  	(etrim ObjRectang P1a)
			  	(entdel ObjRectang)
			)
		)
	)
     )
)
(setvar  "highlight" 1)
(command "undo" "end")
(setvar "OSMODE"  Olmode)
(princ)
)


(defun SortXY (lst /)
(setq Lts-Sort (vl-sort (vl-sort lst '(lambda(e1 e2) (< (cadr (vlax-get (vlax-ename->vla-object e1) 'InsertionPoint)) (cadr (vlax-get (vlax-ename->vla-object e2) 'InsertionPoint))))) '(lambda(e1 e2) (< (car (vlax-get (vlax-ename->vla-object e1) 'InsertionPoint)) (car (vlax-get (vlax-ename->vla-object e2) 'InsertionPoint))))))
Lts-Sort
)


;;;;;;;;;;;;;;http://forums.augi.com/showthread.php?89444-Create-tight-boundary-around-multiple-objects

(defun lst-getboundingbox (lst / maxp minp)
 ;; by ElpanovEvgeniy
 ;; 14.08.2004
 (if (and lst (listp lst))
  (apply
   (function
    (lambda (a1 a2 a3 a4 a5 a6)
     (list
      (list (apply (function min) a1) (apply (function min) a2) (apply (function min) a3))
      (list (apply (function max) a4) (apply (function max) a5) (apply (function max) a6))
     ) ;_  list
    ) ;_ lambda
   ) ;_ function
   (apply
    (function mapcar)
    (cons
     'list
     (mapcar (function (lambda (x)
                        (vla-getboundingbox x 'minp 'maxp)
                        (append (vlax-safearray->list minp) (vlax-safearray->list maxp))
                       ) ;_ lambda
             ) ;_ function
             (vl-remove-if
              (function null)
              (mapcar (function (lambda (x)
                                 (cond ((= (type x) 'ENAME) (vlax-ename->vla-object x))
                                       ((= (type x) 'VLA-object) x)
                                       (t nil)
                                 ) ;_  cond
                                ) ;_ lambda
                      ) ;_ function
                      lst
              ) ;_  mapcar
             ) ;_ vl-remove-if
     ) ;_  mapcar
    ) ;_  cons
   ) ;_  apply
  ) ;_  apply
 ) ;_ if
)


(defun FILTERNAME ( l)
    (if l
      (cons (car l)
        (FILTERNAME
          (vl-remove-if '(lambda ( x ) (equal x (car l) )) (cdr l)) 
        )
      )
    )
)

(defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
)


;defun c:extrim
 
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;Entity-TRIM function
;takes: na - entity name
;  a - a point, the side to trim on
;NOTE: This function does not allow for the possible miss of
;      non-continuous linetypes.
;
(defun etrim ( na a / la b d e1 lst lst2 n j k m ss na2 na3 na4
                      x y z flag flag2 flag3 zlst vpna vplocked
             )
 
 
(setq e1 (entget na));setq
(if (or (setq flag (equal (acet-dxf 0 e1) "POLYLINE"))
        (setq flag (equal (acet-dxf 0 e1) "LWPOLYLINE"))
        (equal (acet-dxf 0 e1) "LINE")
        (equal (acet-dxf 0 e1) "CIRCLE")
        (equal (acet-dxf 0 e1) "ARC")
        (equal (acet-dxf 0 e1) "ELLIPSE")
        (equal (acet-dxf 0 e1) "TEXT")
        (equal (acet-dxf 0 e1) "ATTDEF")
        (equal (acet-dxf 0 e1) "MTEXT")
        (equal (acet-dxf 0 e1) "SPLINE")
    );or
    (progn
     (if (and flag
              (equal 8 (logand 8 (acet-dxf 70 e1)))
         );and
         (setq flag nil)
     );if
     (setq     a (trans a 1 0)
            vpna (acet-currentviewport-ename)
     );setq
     (acet-ucs-cmd (list "_View"))
 
     (setq   lst (acet-geom-object-point-list na nil)  ;;;find extents of selected cutting edge object
             lst (acet-geom-list-extents lst)
               x (- (car (cadr lst)) (car (car lst)))
               y (- (cadr (cadr lst)) (cadr (car lst)))
               x (* 0.075 x)
               y (* 0.075 y)
               z (list x y)
               x (list (+ (car (cadr lst)) (car z))
                       (+ (cadr (cadr lst)) (cadr z))
                 );list
               y (list (- (car (car lst)) (car z))
                       (- (cadr (car lst)) (cadr z))
                 );list
            zlst (zoom_2_object (list x y))
     );setq
     (if vpna
         (setq vplocked (acet-viewport-lock-set vpna nil)) ;unlock cur viewport if needed.
     );if
     (command "_.zoom" "_w" (car zlst) (cadr zlst))
 
     (entupd na)                  ;;;update the ent. so it's curves display smoothly
 
     (setq lst (acet-geom-object-point-list na
                       (/ (acet-geom-pixel-unit) 2.0)
               )
     );setq
     (if (or (not flag)
             (not (acet-geom-self-intersect lst nil))
         );or
         (progn             ;then the object is valid and not a self intersecting polyline.
          (if (and flag
                   (equal (car lst) (last lst) 0.0001)
              );and
              (setq flag3 T);then the polyline could potentialy need a second offset
          );if
          (if (setq la (acet-layer-locked (getvar "clayer")))
              (command "_.layer" "_unl" (getvar "clayer") "")
          );if
 
          (command "_.pline")
          (setq b nil)
          (setq n 0);setq
          (repeat (length lst)
           (setq d (nth n lst))
           (if (not (equal d b 0.0001))
              (progn
               (command d)
               (setq lst2 (append lst2 (list d)));setq
               (setq b d);setq
              );progn
           );if
           (setq n (+ n 1))
          );repeat
          (command "")
          (setq  na2 (entlast)
                  ss (ssadd)
                  ss (ssadd na2 ss)
                 lst nil
          );setq
          (acet-ss-visible ss 1)
          (setq lst2 (get_fence_points na2 a lst2 flag3 flag));setq
 
          (if la
              (command "_.layer" "_lock" (getvar "clayer") "")
          );if
          (acet-ucs-cmd (list "_p"))
          ;Move the ents to force a display update of the ents to avoid viewres problems.
          (setvar "highlight" 0)
          (if (setq ss (ssget "_f" (last lst2)))
              (command "_.move" ss "" "0,0,0" "0,0,0")
          );if
          (if flag
              (progn
               (if (setq la (acet-layer-locked (acet-dxf 8 e1)))
                   (command "_.layer" "_unl" (acet-dxf 8 e1) "")
               );if
               (acet-ucs-set-z (acet-dxf 210 e1))
               (command "_.copy" na "" "0,0,0" "0,0,0")
               ;(entdel na)
               (acet-ss-visible (ssadd na (ssadd)) 1);make it invisible for a while.
                                                    ;rk 12:01 PM 3/10/98
               (setq na3 na
                      na (entlast)
               );setq
               (command "_.pedit" na "_w" "0.0" "_x")
               (acet-ucs-cmd (list "_p"))
               (if la (command "_.layer" "_lock" (acet-dxf 8 e1) ""));if
              );progn
          );if
          (command "_.trim" na "")
          (setq m (- (length lst2) 1));setq
          (setq k 0)
          (repeat (length lst2)
           (setq lst (nth k lst2))
           (setq a (trans (car lst) 0 1))
           (setq n 1)
           (repeat (- (length lst) 1) ;repeat each fence list
            (setq b (trans (nth n lst) 0 1))
            (if (equal a b 0.0001)
                (setq flag2 T)
                (setq flag2 nil)
            );if
            (setq na4 nil);setq
            (setq j 0);setq
            (while (not flag2)       ;repeat each segment of the fence until no new ents are created.
             (setq na4 (entlast));setq
             (command "_F" a b "")
             (if (and (equal na4 (entlast))
                      (or (not (equal k m))
                          (> j 0)
                      );or
                 );and
                 (setq flag2 T)
             );if
             (setq j (+ j 1));setq
            );while
            (setq a b);setq
            (setq n (+ n 1));setq
           );repeat
 
           (setq k (+ k 1))
          );repeat
          (command "")
 
          (if flag
              (progn
               (if (setq la (acet-layer-locked (acet-dxf 8 e1)))
                   (command "_.layer" "_unl" (acet-dxf 8 e1) "")
               );if
               (entdel na) ;get rid of the copy
 
               ;(entdel na3);bring back the original
               (acet-ss-visible (ssadd na3 (ssadd)) 0) ;bring back the original
                                                      ;rk 12:01 PM 3/10/98
               (if la (command "_.layer" "_lock" (acet-dxf 8 e1) ""));if
              );progn
          );if
         );progn
         (progn
          (acet-ucs-cmd (list "_p"))
          (princ "\nSelf intersecting edges are not acceptable.")
         );progn else invalid self intersecting polyline
     );if
     (command "_.zoom" "_p")
     (if vplocked
         (acet-viewport-lock-set vpna T) ;then re-lock the viewport
     );if
    );progn then it's a most likely a valid entity.
);if
);defun etrim
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun another_offset ( pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4 / na ss lst da1 da2)
 
(setq da1 (abs (- a2 a1)));setq
(setq da2 (- (* b (max pl2 pl1))
             (/ (* b (abs (- pl2 pl1)))
                 2.0
             )
          )
);setq
(if (> (abs (- da2 da1))
       (* 0.01 (max a1 a2))
    )
    (progn
 
     (acet-pline-make (list lst2))
     (setq  na (entlast)
           na2 (entlast)
            ss (ssadd)
            ss (ssadd na ss)
     );setq
     (acet-ss-visible ss 1)
     (command "_.offset" b na2 a "")
     (if (and (not (equal na (entlast)))
              (setq lst3 (acet-geom-vertex-list (entlast)))
              (setq lst3 (intersect_check lst2 lst3 lst4))
         );and
         (progn
          (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
          (setq lst (list (acet-geom-vertex-list (list (entlast) 0))));setq
          (entdel (entlast));then offset was a success so delete the ent after getting it's info
         );progn then
         (if (not (equal na (entlast))) (entdel (entlast)));if else
     );if
     (entdel na2)
    );progn then let's do that second offset
);if
 
lst
);defun another_offset
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_fence_points ( na2 a lst2 flag plflag / a1 a2 pl1 pl2 b c d n
                                                   lst lst2 lst3 lst4 na
                        )
 
(if flag
    (progn
     (setq lst2 (cdr lst2));setq
     (repeat (fix (/ (length lst2) 2))
      (setq lst2 (append (cdr lst2) (list (car lst2)));append
      );setq
     );repeat
     (setq lst2 (append lst2 (list (car lst2))));setq
     (command "_.area" "_ob" na2)
     (setq pl1 (getvar "perimeter")
            a1 (getvar "area")
     );setq
    );progn
);if
 
(setq    a (trans a 0 1)
         b (* (getvar "viewsize") 0.05);initial offset distance
         n 3.0                         ;number of offsets
         d (/ b (- n 1))               ;delta offset
         c (acet-geom-pixel-unit)
      lst4 (acet-geom-view-points)
);setq
 
(while (> b c)
(setq na (entlast))
(command "_.offset" b na2 a "")
(if (and (not (equal na (entlast)))
         (setq lst3 (acet-geom-vertex-list (entlast)))
         (or (not plflag)
             (setq lst3 (intersect_check lst2 lst3 lst4))
         );or
    );and
    (progn
     (setq lst3 (acet-geom-m-trans lst3 1 0))
     (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
     (if flag
         (progn
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
         );progn
     );if
     (setq lst (append lst (list lst3)));setq
     (entdel (entlast))  ;delete the ent after getting it's vertex info
     (if flag
         (setq lst (append lst
                           (another_offset pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4)
                   );append
         );setq
     );if
    );progn then offset was a success
    (if (not (equal na (entlast))) (entdel (entlast)));if else
);if
(setq b (- b d));setq
);while
(setq na (entlast))
(command "_.offset" c na2 a "")
(if (and (not (equal na (entlast)))
         (setq lst3 (acet-geom-vertex-list (entlast)))
         (or (not plflag)
             (setq lst3 (intersect_check lst2 lst3 lst4))
         );or
    );and
    (progn
     (setq lst3 (acet-geom-m-trans lst3 1 0))
     (acet-ss-visible (ssadd (entlast) (ssadd)) 1)
     (if flag
         (progn
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
         );progn
     );if
     (setq lst (append lst (list lst3)));setq
     (entdel (entlast));then offset was a success so delete the ent after getting it's info
     (if flag
         (setq lst (append lst
                           (another_offset pl1 pl2 a1 a2 c na2 lst2 a lst3 lst4)
                   );append
         );setq
     );if
    );progn then
    (if (not (equal na (entlast))) (entdel (entlast)));if else
);if
(entdel na2)
 
lst
);defun get_fence_points
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns a list of points on screen if the first two lists do not
;contain segments that intersect each other.
;
(defun intersect_check ( lst lst2 lst3 / x x2 y y2 lst4 flag len len2
                                         a aa b bb c d n j)
 
(setq  len (length lst)
      len2 (length lst2)
         x (car (car lst3))
        x2 (car (cadr lst3))
         y (cadr (car lst3))
        y2 (cadr (cadr lst3))
);setq
 
(setq n 0);setq
(while (and (not flag)
            (< (+ n 1) len2)
       );and
(setq   aa (nth n lst2)
        bb (nth (+ n 1) lst2)
         a (bns_truncate_2_view aa bb x y x2 y2)
         b (bns_truncate_2_view bb aa x y x2 y2)
      lst4 (append lst4 (list a))
);setq
(if (or (not (equal a aa))
        (not (equal b bb))
    );or
    (setq lst4 (append lst4 (list b)))
);if
(setq j 0);setq
 (while (and (not flag)
             (< (+ j 1) len)
        );and
 (setq    c (nth j lst)
          d (nth (+ j 1) lst)
       flag (inters a b c d)
 );setq
 
 (setq j (+ j 1));setq
 );while
 
(setq n (+ n 1));setq
);while
(if (not (equal b (last lst4)))
    (setq lst4 (append lst4 (list b)));setq
);if
(if (not flag)
    (setq flag lst4)
    (setq flag nil)
);if
flag
);defun intersect_check
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zoom_2_object ( lst / p1 p2 p3 p4 p5 p6 mp dx dy dx2 dy2
                             r1 r2 na e1 x w h dv1 dv2 x
                     )
 
 (setq  lst (acet-geom-m-trans lst 1 2)
         p1 (acet-geom-m-trans (acet-geom-view-points) 1 2)    ;p1 and p2 are the viewpnts
         p2 (cadr p1)
         p1 (car p1)
         p1 (list (car p1) (cadr p1))
         p2 (list (car p2) (cadr p2))
 );setq
 (if lst
     (progn
      (setq   p5 (acet-geom-list-extents lst)              ;p5 and p6 are the geometry points
              p6 (cadr p5)
              p5 (car p5)
              p5 (list (car p5) (cadr p5))
              p6 (list (car p6) (cadr p6))
              mp (acet-geom-midpoint p5 p6)           ;prepare to resize the geometry rectang to
              dx (- (car p2) (car p1))    ;have the same dy/dx ratio as p1 p2.
              dy (- (cadr p2) (cadr p1))
             dx2 (- (car p6) (car p5))
             dy2 (- (cadr p6) (cadr p5))
      );setq
      (if (equal dx 0.0)  (setq dx 0.000001))  ;just in case div by zero
      (if (equal dx2 0.0) (setq dx2 0.000001))
      (setq   r1 (/ dy dx)
              r2 (/ dy2 dx2)
      );setq
      (if (< r2 r1)
          (setq dy2 (* r1 dx2));then scale dy2 up
          (progn
           (if (equal r1 0.0)  (setq r1 0.000001))  ;just in case div by zero
           (setq dx2 (* dy2 (/ 1.0 r1)));else scale dx2 up
          );progn
      );if
      (setq p5 (list (- (car mp) (/ dx2 1.98))   ;1.98 is used instead of 2.0 to expand
                     (- (cadr mp) (/ dy2 1.98))  ;the rectangle slightly
               );list
            p6 (list (+ (car mp) (/ dx2 1.98))
                     (+ (cadr mp) (/ dy2 1.98))
               );list
      );setq
     );progn then lst
 );if
 (if (and lst
          (equal 0 (getvar "tilemode"))
          (not (equal 1 (getvar "cvport")))
          (setq na (acet-currentviewport-ename))
     );and
     (progn
      (setq  e1 (entget na)
              x (cdr (assoc 10 e1))
              w (cdr (assoc 40 e1))
              h (cdr (assoc 41 e1))
             p3 (list (- (car x) (/ w 2.0))
                      (- (cadr x) (/ h 2.0))
                );list
             p4 (list (+ (car x) (/ w 2.0))
                      (+ (cadr x) (/ h 2.0))
                );list
             p3 (trans p3 3 2)      ;p3 and p4 are the viewport points
             p4 (trans p4 3 2)
            dv1 (acet-geom-delta-vector p1 p3)
            dv2 (acet-geom-delta-vector p2 p4)
              x (distance p1 p2)
      );setq
      (if (equal 0 x) (setq x 0.000001));just in case
      (setq   x (/ (distance p5 p6)
                   x
                )
            dv1 (acet-geom-vector-scale dv1 x)
            dv2 (acet-geom-vector-scale dv2 x)
             p5 (acet-geom-vector-add p5 dv1)
             p6 (acet-geom-vector-add p6 dv2)
       );setq
     );progn then
 );if
 (setq p1 (list (car p1) (cadr p1) 0.0)
       p2 (list (car p2) (cadr p2) 0.0)
       p5 (list (car p5) (cadr p5) 0.0)
       p6 (list (car p6) (cadr p6) 0.0)
 );setq
 (if lst
     (setq lst (list (trans p5 2 1)
                     (trans p6 2 1)
               );list
     );setq
     (setq lst nil)
 );if
 
 lst
);defun zoom_2_object


(princ)



