;;;(alert (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh t\U+00E1ch Lo\U+1EA1i \U+0111\U+1EA5t + S\U+1ED1 th\U+1EEDa + Di\U+1EC7n t\U+00EDch tr\U+00EAn c\U+00F9ng m\U+1ED9t d\U+00F2ng"
;;;	       "\nB\U+01B0\U+1EDBc 1: Thay th\U+1EBF d\U+1EA5u / th\U+00E0nh d\U+1EA5u c\U+00E1ch (space) trong b\U+1EA3n v\U+1EBD. VD t\U+1EEB LUC 24/84.2 => LUC 24 84.2"
;;;	       "\nB\U+01B0\U+1EDBc 2: D\U+00F9ng l\U+1EC7nh T2T \U+0111\U+1EC3 t\U+00E1ch c\U+00E1c ph\U+1EA7n t\U+1EED trong d\U+00F2ng"
;;;	       "\nB\U+01B0\U+1EDBc 3: D\U+00F9ng l\U+1EC7nh TTD \U+0111\U+1EC3 l\U+1ECDc v\U+00E0 g\U+00E1n layer cho t\U+1EEBng \U+0111\U+1ED1i t\U+01B0\U+1EE3ng"
;;;	       "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;	       "\nEmail: Heaven2407@gmail.com"
;;;	       "\nNumber: 0972.0168.25"
;;;	       "\nC\U+00E1c b\U+1EA1n c\U+00F3 th\U+1EC3 n\U+1EA1p th\U+1EBB \U+0111i\U+1EC7n tho\U+1EA1i cho m\U+00ECnh n\U+1EBFu ch\U+01B0\U+01A1ng tr\U+00ECnh h\U+1EEFu \U+00EDch v\U+1EDBi c\U+00E1c b\U+1EA1n. ^^"
;;;	       
;;;	)
;;;)
;;;(Prompt (strcat "\nCh\U+01B0\U+01A1ng tr\U+00ECnh t\U+00E1ch Lo\U+1EA1i \U+0111\U+1EA5t + S\U+1ED1 th\U+1EEDa + Di\U+1EC7n t\U+00EDch tr\U+00EAn c\U+00F9ng m\U+1ED9t d\U+00F2ng"
;;;	       "\nB\U+01B0\U+1EDBc 1: Thay th\U+1EBF d\U+1EA5u / th\U+00E0nh d\U+1EA5u c\U+00E1ch (space) trong b\U+1EA3n v\U+1EBD. VD t\U+1EEB LUC 24/84.2 => LUC 24 84.2"
;;;	       "\nB\U+01B0\U+1EDBc 2: D\U+00F9ng l\U+1EC7nh T2T \U+0111\U+1EC3 t\U+00E1ch c\U+00E1c ph\U+1EA7n t\U+1EED trong d\U+00F2ng"
;;;	       "\nB\U+01B0\U+1EDBc 3: D\U+00F9ng l\U+1EC7nh TTD \U+0111\U+1EC3 l\U+1ECDc v\U+00E0 g\U+00E1n layer cho t\U+1EEBng \U+0111\U+1ED1i t\U+01B0\U+1EE3ng"
;;;	       "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n"
;;;	       "\nEmail: Heaven2407@gmail.com"
;;;	       "\nNumber: 0972.0168.25"
;;;	       "\nC\U+00E1c b\U+1EA1n c\U+00F3 th\U+1EC3 n\U+1EA1p th\U+1EBB \U+0111i\U+1EC7n tho\U+1EA1i cho m\U+00ECnh n\U+1EBFu ch\U+01B0\U+01A1ng tr\U+00ECnh h\U+1EEFu \U+00EDch v\U+1EDBi c\U+00E1c b\U+1EA1n. ^^"
;;;	)
;;;)

	       
(defun c:T2T ( / _splitwords _textwidth ang dxf ent enx fun inc lst pnt sel spc tot wid );;;;TEXT TO TEXT
(prompt "Make by Nguyen Thien Duong")
    (defun _splitwords ( str / pos )
        (if (setq pos (vl-string-position 32 str))
            (cons (cons 1 (substr str 1 pos)) (_splitwords (substr str (+ pos 2))))
            (list (cons 1 str))
        )
    )

    (defun _textwidth ( enx )
        ((lambda ( lst ) (- (caadr lst) (caar lst))) (textbox enx))
    )

;;;    (if (setq sel (ssget '((0 . "TEXT") (-4 . "<NOT") (-4 . "<OR") (72 . 3) (72 . 4) (72 . 5) (-4 . "OR>") (-4 . "NOT>"))))
      (if (setq sel (ssget '((0 . "TEXT") )))
        (repeat (setq inc (sslength sel))
            (setq ent (ssname sel (setq inc (1- inc)))
                  enx (entget ent)
                  tot 0.0
                  lst nil
            )
            (foreach item (_splitwords (cdr (assoc 1 enx)))
                (if (= "" (cdr item))
                    (setq lst (cons '(nil . 0.0) lst))
                    (setq dxf (entget (entmakex (subst item (assoc 1 enx) enx)))
                          wid (_textwidth dxf)
                          tot (+ tot wid)
                          lst (cons (cons dxf wid) lst)
                    )
                )
            )
            (if (< 1 (length lst))
                (progn
                    (setq wid (_textwidth enx)
                          spc (/ (- wid tot) (float (1- (length lst))))
                          lst (reverse lst)
                          ang (cdr (assoc 50 enx))
                    )
                    (if
                        (and
                            (= 0 (cdr (assoc 72 enx)))
                            (= 0 (cdr (assoc 73 enx)))
                        )
                        (setq pnt (cdr (assoc 10 enx)))
                        (setq pnt (cdr (assoc 11 enx)))
                    )
                    (cond
                        (   (= (cdr (assoc 72 enx)) 0)
                            (setq fun (lambda ( a b ) (+ spc (cdr a))))
                        )
                        (   (= (cdr (assoc 72 enx)) 1)
                            (setq fun (lambda ( a b ) (+ spc (/ (+ (cdr a) (cdr b)) 2.0)))
                                  pnt (polar pnt (+ ang pi) (/ (- wid (cdar lst)) 2.0))
                            )
                        )
                        (   (= (cdr (assoc 72 enx)) 2)
                            (setq fun (lambda ( a b ) (+ spc (cdr b)))
                                  pnt (polar pnt (+ ang pi) (- wid (cdar lst)))
                            )
                        )
                    )
                    (mapcar
                        (function
                            (lambda ( a b / dxf )
                                (if (setq dxf (car a))
                                    (entmod
                                        (subst (cons 10 pnt) (assoc 10 dxf)
                                            (subst (cons 11 pnt) (assoc 11 dxf) dxf)
                                        )
                                    )
                                )
                                (setq pnt (polar pnt ang (fun a b)))
                            )
                        )
                        lst (append (cdr lst) '((nil . 0.0)))
                    )
                    (entdel ent)
                )
            )
        )
    )
    (princ)
)
(princ)

;;;(defun C:TTD (/ ss ss1 ss2 e1 e2 txt);;;;;TACH THUA DAT
;;;(MakeLayer_ "LOAIDAT" 1)
;;;(MakeLayer_ "SOTHUA" 2)
;;;(MakeLayer_ "DIENTICH" 3)
;;;(MakeLayer_ "GHICHU" 4)
;;;(setq ss (ssget (list (cons 0 "TEXT"))))
;;;(foreach e2 (acet-ss-to-list ss)
;;;	(setq txt (cdr (assoc 1 (entget e2))))
;;;	(cond
;;;	  (
;;;	   (and (not (distof txt)) (= (strlen txt) 3))
;;;	   (vla-put-Layer (vlax-ename->vla-object e2) "LOAIDAT")
;;;	  )
;;;	  	  (
;;;	   (vl-string-search "." txt)
;;;	   (vla-put-Layer (vlax-ename->vla-object e2) "DIENTICH")
;;;	  )
;;;	  (
;;;	   T
;;;	   (vla-put-Layer (vlax-ename->vla-object e2) "SOTHUA")
;;;	  )
;;;	)
;;;)
;;;(princ)  
;;;)

(defun C:TTD (/ ss ss1 ss2 e1 e2 txt);;;;;TACH THUA DAT
(MakeLayer_ "LOAIDAT" 1)
(MakeLayer_ "SOTHUA" 2)
(MakeLayer_ "DIENTICH" 3)
(MakeLayer_ "GHICHU" 4)
(setq ss (ssget (list (cons 0 "TEXT"))))
(foreach e2 (acet-ss-to-list ss)
	(setq txt (cdr (assoc 1 (entget e2))))
	(if (and (not (distof txt)) (= (strlen txt) 3))
	    (vla-put-Layer (vlax-ename->vla-object e2) "LOAIDAT")
	    (progn
		(if (and (not (distof txt)) (<(strlen txt) 3) (> (strlen txt) 3))
	    	    (vla-put-Layer (vlax-ename->vla-object e2) "GHICHU")
		    (Progn
			(if (vl-string-search "." txt)
		    	    (vla-put-Layer (vlax-ename->vla-object e2) "DIENTICH")
			    (Progn
				(if (distof txt 2)
			    	    (vla-put-Layer (vlax-ename->vla-object e2) "SOTHUA")
				)
			    )
			 )
		    )
		  )
	    )
	)
)
(princ)  
)






(defun MakeLayer_ ( name colour /)
    (if (null (tblsearch "LAYER" name))
        (entmake
            (list
               '(0 . "LAYER")
               '(100 . "AcDbSymbolTableRecord")
               '(100 . "AcDbLayerTableRecord")
               '(70 . 0)
                (cons 2 name)
                (cons 62 colour)
            )
        )
    )
)

