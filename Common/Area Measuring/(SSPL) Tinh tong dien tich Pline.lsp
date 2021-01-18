(defun c:SSPL ( / pl sum_area result)
    (if (setq pl (ssget '((0 . "*polyline"))))
        (progn
            (setq sum_area 0)
            (repeat (setq i (sslength pl))
                (setq sum_area
                    (+ (vla-get-area
                        (vlax-ename->vla-object (ssname pl (setq i (1- i))))
                        )
                    sum_area
                    )
                )
            )
            (setq result (strcat "\nPolyline Areas: " (rtos (/ sum_area 1000000) 2 1) " m2"))
            (alert result)
            (prompt result)
        )
    )
    (princ)
)