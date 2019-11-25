;; free lisp from cadviet.com

(defun C:CDV(/ ss opt p1 p2 d ag i oldos mode n) ;;;Muti/Divide copy
(setq
    ss (ssget)
    opt (strcase (getstring "\nCopy Multi-n/Divide-n <D2>:"))
    p1 (getpoint "\nFirst base point:")
    p2 (getpoint p1 "\nSecond base point:")
    d (distance p1 p2)
    ag (angle p1 p2)
    i 0
    oldos (getvar "osmode")
)
(if (= opt "") (setq opt "D2"))
(setq
    mode (substr opt 1 1)
    n (atoi (substr opt 2 (- (strlen opt) 1)))
)
(if (= mode "D") (setq d (/ d n)))
(setvar "osmode" 0)
(repeat n
    (setq i (1+ i))
    (command "copy" ss "" p1 (polar p1 ag (* i d)))
)
(setvar "osmode" oldos)
(princ)
)
