(defun c:MAV (/ AT:GetSel atts ss i ass)
  ;; Match Attribute Values
  ;; Alan J. Thompson, 2017.01.31


  (defun AT:GetSel (meth msg fnc / ent)
    ;; meth - selection method (entsel, nentsel, nentselp)
    ;; msg - message to display (nil for default)
    ;; fnc - optional function to apply to selected object
    ;; Ex: (AT:GetSel entsel "\nSelect arc: " (lambda (x) (eq (cdr (assoc 0 (entget (car x)))) "ARC")))
    ;; Alan J. Thompson, 05.25.10
    (while
      (progn (setvar 'ERRNO 0)
             (setq ent (meth (cond (msg)
                                   ("\nSelect object: ")
                             )
                       )
             )
             (cond ((eq (getvar 'ERRNO) 7) (princ "\nMissed, try again."))
                   ((eq (type (car ent)) 'ENAME)
                    (if (and fnc (not (fnc ent)))
                      (princ "\nInvalid object!")
                    )
                   )
             )
      )
    )
    ent
  )


  (if (and (AT:GetSel entsel
                      "\nSelect source attributed block: "
                      (lambda (x / d)
                        (if (and (eq (cdr (assoc 0 (setq d (entget (car x))))) "INSERT")
                                 (eq (cdr (assoc 66 d)) 1)
                            )
                          (setq atts (mapcar (function (lambda (a) (cons (vla-get-tagstring a) (vla-get-textstring a))))
                                             (vlax-invoke (vlax-ename->vla-object (car x)) 'GetAttributes)
                                     )
                          )
                        )
                      )
           )
           (progn
             (princ "\nSelect destination attributed block(s): ")
             (setq ss (ssget "_:L" '((0 . "INSERT") (66 . 1))))
           )
      )
    (repeat (setq i (sslength ss))
      (foreach a (vlax-invoke (vlax-ename->vla-object (ssname ss (setq i (1- i)))) 'GetAttributes)
        (if (setq ass (cdr (assoc (vla-get-tagstring a) atts)))
          (vla-put-textstring a ass)
        )
      )
    )
  )

  (princ)
)
(vl-load-com)
(princ)