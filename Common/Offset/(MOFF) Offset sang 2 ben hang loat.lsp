(defun c:mOff (/ ss tmp)
  (vl-load-com)
  (or *moff (setq *moff 10.0))

  (if (setq ss (ssget '((0 . "ARC,CIRCLE,ELLIPSE,*LINE"))))
    (progn
      (initget 6)
      (and (setq tmp (getdist (strcat "\nSpecify Offset <" (vl-princ-to-string *moff) "> : ")))
           (setq *moff tmp))

      (mapcar
        (function
          (lambda (x)
            (vla-offset x *moff)
            (vla-offset x (- *moff))))
        (mapcar 'vlax-ename->vla-object
          (vl-remove-if 'listp
            (mapcar 'cadr (ssnamex ss)))))))
  (princ))