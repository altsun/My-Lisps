(defun c:LegLengthMod ( / ss dimobjs)
  
  ;; codehimbelonga KerryBrown@theSwamp 2010.05.28
  ;; http://www.theswamp.org/index.php?topic=33493.msg389031#msg389031

  (vl-load-com)
  (if (and (setq ss (ssget '((0 . "DIMENSION"))))
           (setq dimobjs (mapcar 'vlax-ename->vla-object
                                 (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                         )
           )
      )
    (foreach dim dimobjs
      (vla-put-extlinefixedlensuppress dim :vlax-true)
      (vla-put-extlinefixedlen dim (* 2 (vla-get-textheight dim)))
    )
  )
  (princ)
)


(defun c:SD (/ sel newpt ent edata elist)
    
  ;; codehimbelonga KerryBrown@theSwamp 2010.05.28
  

  (if (and (setq sel (entsel "\nSelect Dimension to Split."))
           (setq newpt (getpoint "\Select new Dim Point"))
      )
    (progn (setq ent   (car sel)
                 edata (entget ent)
                 elist (vl-remove-if
                         '(lambda (pair)
                            (member (car pair)
                                    (list -1 2 5 102 310 300 330 331 340 350 360 410)
                            )
                          )
                         edata
                       )
           )
           (entmod (subst (cons 14 newpt) (assoc 14 elist) edata))
           (entmakex (subst (cons 13 newpt) (assoc 13 elist) elist))
           (command "_matchprop" (car sel) (entlast) "")
    )
  )
  (princ)
)