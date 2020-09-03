;|							;;
	E L L B Y (double line Elbow maker v.1.5	;;
	By. Andrea Andreetti	2000/12/08		;;
	New release	        2009/01/02		;;
							|;

(defun c:ellby  (/       sai     abi     ilist   p1      p2      lines   val     iname   ass10   ass1    assang  i1a
               i1b     e1a     e1b     i2a     i2b     e2a     e2b     li1a    li1b    li2a    li2b    le1a    le1b
               le2a    le2b    inti2   inti2a  inte2a  pick1   pick2   dist_1  dist_2  fdist   vla-rci rci     vla-rce
               rce     jc1i    jc2i    acip    jc1e    jc2e    acep    jex2    jix2    jex1    jix1    linetypename
               colorname       layername       doc     space   vla_line1       vla_line2
               )
  (setq ilist nil)
  (setq sai nil)
  ;;	Degree Conversion	;;
  ;;
  (defun dtr (a) (* pi (/ a 180.0)))
  (defun rtd (a) (/ (* a 180) pi))
  ;;
  ;;	Degree Conversion	;;  
  (initget 1024)
  (setq p1 (getpoint "\nSelect object..."))
  (setq p2 (getcorner p1))
  (setq mp (polar p1 (angle p1 p2) (/ (distance p1 p2) 2)))
  (setq p1 (list (nth 0 p1) (nth 1 p1)))
  (setq p2 (list (nth 0 p2) (nth 1 p2)))
  (setq lines (ssget "_C" p1 p2))
  (if (/= (sslength lines) 4)
    (alert "4 lines need to be selected")
    (progn (setq val -1)
           (repeat 4
             (setq iname (entget (ssname lines (setq val (1+ val)))))
             (setq ass10 (cdr (assoc 10 iname)))
             (setq ass11 (cdr (assoc 11 iname)))
             (setq assang (angle ass10 ass11))
             (if (not ilist)
               (setq ilist (append ilist (list (cons (cdar iname) (rtd assang)))))
               (progn
                 (if (or (eq (rtos (rtd (angle ass10 ass11))) (rtos (cdar ilist)))
                         (eq (rtos (rtd (angle ass11 ass10))) (rtos (cdar ilist)))
                     )
                   (setq sai (list (caar ilist) (cdar iname)))
                 )
               )
             )
           )
           (foreach n sai (ssdel n lines))
           (setq sbi (list (ssname lines 0) (ssname lines 1)))
           (setq i1a (cdr (assoc 10 (entget (car sai)))))
           (setq i1b (cdr (assoc 11 (entget (car sai)))))
           (setq e1a (cdr (assoc 10 (entget (cadr sai)))))
           (setq e1b (cdr (assoc 11 (entget (cadr sai)))))
           (setq inti1 (polar p1 (+ (dtr 90) (angle i1a i1b)) 2))
           (setq inti1a (inters p1 inti1 i1a i1b nil))
           (setq inte1a (inters p1 inti1 e1a e1b nil))
           (if (< (distance p1 inti1a) (distance p1 inte1a))
             (progn (setq li1a (entget (car sai)))
                    (setq le1a (entget (cadr sai)))
             )
             (progn (setq le1a (entget (car sai)))
                    (setq li1a (entget (cadr sai)))
             )
           )
           (setq i2a (cdr (assoc 10 (entget (car sbi)))))
           (setq i2b (cdr (assoc 11 (entget (car sbi)))))
           (setq e2a (cdr (assoc 10 (entget (cadr sbi)))))
           (setq e2b (cdr (assoc 11 (entget (cadr sbi)))))
           (setq inti2 (polar p1 (- (dtr 90) (angle i2a i2b)) 2))
           (setq inti2a (inters p1 inti2 i2a i2b nil))
           (setq inte2a (inters p1 inti2 e2a e2b nil))
           (if (< (distance p1 inti2a) (distance p1 inte2a))
             (progn (setq li2a (entget (car sbi)))
                    (setq le2a (entget (cadr sbi)))
             )
             (progn (setq le2a (entget (car sbi)))
                    (setq li2a (entget (cadr sbi)))
             )
           )
           (setq pick2 (inters i2a
                               (polar i2a (+ (dtr 90) (angle i2a i2b)) 4)
                               e2a
                               e2b
                               nil
                       )
           )
           (setq pick1 (inters i1a
                               (polar i1a (+ (dtr 90) (angle i1a i1b)) 4)
                               e1a
                               e1b
                               nil
                       )
           )
           (setq dist_1 (distance i1a pick1))
           (setq dist_2 (distance i2a pick2))
           (if (< dist_1 dist_2)
             (setq fdist dist_1)
             (setq fdist dist_2)
           )
           (vl-load-com)
           (setvar "FILLETRAD" fdist)
           (vl-cmdf "._fillet" (cdar li1a) (cdar li2a))
           (setq rci (entlast))
           (setq vla-rci (vlax-ename->vla-object rci))
           (setvar "FILLETRAD" (* fdist 2.0))
           (vl-cmdf "._fillet" (cdar le1a) (cdar le2a))
           (setq rce (entlast))
           (setq vla-rce (vlax-ename->vla-object rce))
           (setq jc1i (vlax-safearray->list
                        (vlax-variant-value (vla-get-startpoint vla-rci))
                      )
           )
           (setq jc2i (vlax-safearray->list
                        (vlax-variant-value (vla-get-endpoint vla-rci))
                      )
           )
           (setq acip (vlax-safearray->list
                        (vlax-variant-value (vla-get-center vla-rci))
                      )
           )
           (setq jc1e (vlax-safearray->list
                        (vlax-variant-value (vla-get-startpoint vla-rce))
                      )
           )
           (setq jc2e (vlax-safearray->list
                        (vlax-variant-value (vla-get-endpoint vla-rce))
                      )
           )
           (setq acep (vlax-safearray->list
                        (vlax-variant-value (vla-get-center vla-rce))
                      )
           )
           (setvar "OSMODE" 0)
           (setq jex2 (inters acip
                              (polar acip (+ (dtr 90) (angle e2a e2b)) 4)
                              e2a
                              e2b
                              nil
                      )
           )
           (setq jix2 (inters acip
                              (polar acip (+ (dtr 90) (angle i2a i2b)) 4)
                              i2a
                              i2b
                              nil
                      )
           )
           (setq jex1 (inters acip
                              (polar acip (+ (dtr 90) (angle e1a e1b)) 4)
                              e1a
                              e1b
                              nil
                      )
           )
           (setq jix1 (inters acip
                              (polar acip (+ (dtr 90) (angle i1a i1b)) 4)
                              i1a
                              i1b
                              nil
                      )
           )
           (if (> (distance jex1 jix1) (distance jex2 jix2))
             (progn (setq jex1 (inters acep
                                       (polar acep (+ (dtr 90) (angle e1a e1b)) 4)
                                       e1a
                                       e1b
                                       nil
                               )
                    )
                    (setq jix1 (inters acep
                                       (polar acep (+ (dtr 90) (angle i1a i1b)) 4)
                                       i1a
                                       i1b
                                       nil
                               )
                    )
             )
             (progn (setq jex2 (inters acep
                                       (polar acep (+ (dtr 90) (angle e2a e2b)) 4)
                                       e2a
                                       e2b
                                       nil
                               )
                    )
                    (setq jix2 (inters acep
                                       (polar acep (+ (dtr 90) (angle i2a i2b)) 4)
                                       i2a
                                       i2b
                                       nil
                               )
                    )
             )
           )

           (setq VLA_li1a (vlax-ename->vla-object (cdar li1a)))
      	   (setq thknsA (vla-get-Thickness VLA_li1a))

           (setq VLA_li2a (vlax-ename->vla-object (cdar li2a)))
      	   (setq thknsB (vla-get-Thickness VLA_li2a))
      
           (setq layername (vla-get-layer vla-rce))
           (setq colorname (vla-get-color vla-rce))
           (setq linetypename (vla-get-linetype vla-rce))
           (setq doc   (vla-get-activedocument (vlax-get-acad-object))
                 space (if (= (getvar "cvport") 1)
                         (vla-get-paperspace doc)
                         (vla-get-modelspace doc)
                       )
           )
           (setq vla_line2 (vlax-invoke space 'addline jex2 jix2))
           (setq vla_line1 (vlax-invoke space 'addline jex1 jix1))      

           (vla-put-Thickness vla_line1 thknsA)
           (vla-put-Thickness vla_line2 thknsB)
      
           (vla-put-layer vla_line1 layername)
           (vla-put-color vla_line1 colorname)
           (vla-put-linetype vla_line1 linetypename)
           (vla-put-layer vla_line2 layername)
           (vla-put-color vla_line2 colorname)
           (vla-put-linetype vla_line2 linetypename)
      
           (vlax-release-object vla_line2)
           (vlax-release-object vla_line1)
           (vlax-release-object doc)
           (vlax-release-object vla-rci)
           (vlax-release-object vla-rce)
    )
  )
)


(princ "\n ELLBY v.1.5 - By. Andrea Andreetti       Type \"ELLBY\" to start")
