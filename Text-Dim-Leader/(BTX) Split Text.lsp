;;; Break a text string to words
;;; "This is a text." changed to: "This" "is" "a" "text."
;;; It works for any rotated texts
;;; written by Abbas Aqdam

;;;========================================Sub-Functions=====================================================
(defun GetWidthSize (lst TS H W)
	(mapcar '(lambda (d)(abs(apply '- (mapcar 'car(textbox(list(cons 1 d)(cons 7 ts)(cons 40 h)(cons 41 w)))))))lst)
);GetWidthSize

(defun Add_Point_X_R ( P X R )
(list (+ (car p) (* X (cos R))) (+ (cadr p) (* X (sin R))) 0.0)
)
;;;========================================MAIN===============================================================
(defun BreakText ( ename / v1 p q lst lstW ts h w R X i)
(setq e1 (entget ename) v1 (cdr (assoc 1 e1)) p 0 lst nil)
(while (setq q (vl-string-position 32 v1 p))
	(setq lst (append lst (list (substr v1 (1+ p) (- q p) ))))
	(setq p (1+ q))
);while
(setq lst (append lst (list(substr v1 (1+ p)))))
(setq lst (vl-remove "" lst))
(setq ts (cdr(assoc 7 e1)) h (cdr(assoc 40 e1)) w (cdr(assoc 41 e1)))
(setq R (cdr(assoc 50 e1)));R in Radian
(setq spc (* 1.35 (car(GetWidthSize '("-") ts h w))))
	;;to estimate one space char width (between words)
(setq lstW (GetWidthSize lst ts h w))
(setq e1 (subst (cons 72 0) (assoc 72 e1) e1 ))
(setq e1 (subst (cons 73 0) (assoc 73 e1) e1 ))
(setq P (cdr (assoc 10 e1)));;Text insertion point
(setq i 0 X 0)
(entdel ename)
(repeat (length lst)
	(setq e1 (subst (cons 1 (nth i lst)) (assoc 1 e1) e1 ))
	(if (> i 0) (setq X (+ X (+ (nth (1- i) lstW) spc))))
	(setq e1 (subst (cons 10 (Add_Point_X_R P X R)) (assoc 10 e1) e1 ))
	(entmake e1)
	(setq i (1+ i))
);repaet
(princ)
);BreakText
;;;======================================== USAGE: =============================================================

(defun C:BTX ( / ss i n ename )
(setq ss (ssget '((0 . "TEXT"))))
  (if ss
    (progn
      (setq i 0 n (sslength ss))
          (while (< i n)
            (setq ename (ssname ss i))
			(BreakText ename)
            (setq i (1+ i))
          )
    );progn
  );if
  (setq ss nil)
  (princ)
)

(princ "***** TYPE BTX *****")