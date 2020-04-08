;;************************LỆNH TẮT OFFSET SANG 2 BÊN THEO ĐỘ RỘNG HÀNG LOẠT

(defun c:FFB ( / ss tmp b)
    (setq b 0)
    (if (setq ss (ssget '((0 . "ARC,CIRCLE,ELLIPSE,*LINE"))))
        (progn
            (initget 6)
            (and (setq tmp (getdist (strcat "\nSpecify Offset <" (vl-princ-to-string b) "> : ")))
                (setq b tmp))
            (mapcar
                (function
                    (lambda (x)
                        (vla-offset x (/ b 2))
                        (vla-offset x (/ (- b) 2))
                    )
                )
                (mapcar 'vlax-ename->vla-object
                    (vl-remove-if 'listp
                        (mapcar 'cadr (ssnamex ss))
                    )
                )
            )
        )
    )
)