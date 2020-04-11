(defun c:rnb ( / i l n p s x )
    (while
        (not (or (= "" (setq x (getstring t "\nTien to: ")))
                 (snvalid (vl-string-trim " " x))
             )
        )
        (princ "\nTien to chua ky hieu ko hop le.")
    )
    (if (and (/= "" x) (setq s (ssget '((0 . "INSERT")))))
        (progn
            (setq p (strcat "`**,*" (strcase x)))
            (repeat (setq i (sslength s))
                (or (member (setq n (LM:blockname (vlax-ename->vla-object (ssname s (setq i (1- i)))))) l)
                    (wcmatch (strcase n) p)
                    (setq l (cons n l))
                )
            )
            (vlax-for b (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
                (if (member (vla-get-name b) l)
                    (vla-put-name b (strcat x (vla-get-name b)))
                )
            )
        )
    )
    (princ)
)

;; Block Name  -  Lee Mac
;; Returns the true (effective) name of a supplied block reference
                        
(defun LM:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun LM:blockname ( obj ) (vla-get-effectivename obj))
        (defun LM:blockname ( obj ) (vla-get-name obj))
    )
    (LM:blockname obj)
)

(vl-load-com) (princ)