(vl-load-com)
;;;(alert (strcat "\nCH\U+01AF\U+01A0NG TR\U+00CCNH X\U+00D3A TEXT THEO \U+0110\U+1ED8 CAO NH\U+1EACP V\U+00C0O"
;;;	       "\n"
;;;	       "\nT\U+00EAn l\U+1EC7nh: XDTDC"
;;;	       "\n"
;;;	       "\n"
;;;	       "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n - \U+0110\U+1ECBa Ch\U+00EDnh K48"
;;;	       "\nMobile: 0972.0168.25"
;;;	       "\nN\U+1EBFu LISP n\U+00E0y mang l\U+1EA1i l\U+1EE3i \U+00EDch cho b\U+1EA1n, b\U+1EA1n c\U+00F3 th\U+1EC3 n\U+1EA1p th\U+1EBB \U+0111i\U+1EC7n tho\U+1EA1i \U+1EE7ng h\U+1ED9 m\U+00ECnh nha. ^^"
;;;        )
;;;)
;;;(Prompt (strcat"\nCH\U+01AF\U+01A0NG TR\U+00CCNH X\U+00D3A TEXT THEO \U+0110\U+1ED8 CAO NH\U+1EACP V\U+00C0O"
;;;	       "\n"
;;;	       "\nT\U+00EAn l\U+1EC7nh: XDTDC"
;;;	       "\n"
;;;	       "\n"
;;;	       "\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n - \U+0110\U+1ECBa Ch\U+00EDnh K48"
;;;	       "\nMobile: 0972.0168.25"
;;;	       "\nN\U+1EBFu LISP n\U+00E0y mang l\U+1EA1i l\U+1EE3i \U+00EDch cho b\U+1EA1n, b\U+1EA1n c\U+00F3 th\U+1EC3 n\U+1EA1p th\U+1EBB \U+0111i\U+1EC7n tho\U+1EA1i \U+1EE7ng h\U+1ED9 m\U+00ECnh nha. ^^"
;;;        )
;;;)
(defun c:XGT ( / DOCAO_XOA dem ss3 ss item temp Code ans);;;;Xoa do theo do cao
(setvar "CMDECHO" 0)
(setq dem 0)
(Alert "\nQu\U+00E9t ch\U+1ECDn Text")
(setq ss3 (ssget (list (cons 0  "TEXT"))))
(setq ss (ChonTextSo ss3))
(setq ss (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex ss))))
(initget "L N B")
(setq ans (strcase (getkword "\nB\U+1EA1n mu\U+1ED1n x\U+00F3a Text c\U+00F3 \U+0111\U+1ED9 cao l\U+1EDBn h\U+01A1n, nh\U+1ECF h\U+01A1n hay bang ( L / N / B ):")))
(cond ((= ans "L")
	      (progn
			(setq DOCAO_XOA (getreal "\nNh\U+1EADp \U+0111\U+1ED9 cao l\U+1EDBn h\U+01A1n s\U+1EBD b\U+1ECB x\U+00F3a: "))
			(foreach item ss
				(setq temp  (entget item))
				(setq  Code (cdr (assoc 1 temp)))
				(if (> (atof Code) DOCAO_XOA)
					(progn
						(entdel item)
						(setq dem (+ dem 1))
					)
				)
			 )
	      )
     )
     ((= ans "N")
	      (progn
			(setq DOCAO_XOA (getreal "\nNh\U+1EADp \U+0111\U+1ED9 cao l\U+1EDBn nh\U+1ECF s\U+1EBD b\U+1ECB x\U+00F3a: "))
			(foreach item ss
				(setq temp  (entget item))
				(setq  Code (cdr (assoc 1 temp)))
				(if (< (atof Code) DOCAO_XOA)
					(progn
						(entdel item)
						(setq dem (+ dem 1))
					)
				)
			 )
	      )
     )
     ((= ans "B")
	      (progn
			(setq DOCAO_XOA (getreal "\nNh\U+1EADp \U+0111\U+1ED9 cao \U+0111\U+00FAng gi\U+00E1 tr\U+1ECB s\U+1EBD b\U+1ECB x\U+00F3a: "))
			(foreach item ss
				(setq temp  (entget item))
				(setq  Code (cdr (assoc 1 temp)))
				(if (equal (atof Code) DOCAO_XOA 0.00000001)
					(progn
						(entdel item)
						(setq dem (+ dem 1))
					)
				)
			 )
	      )
     )
)


(alert (strcat "\nC\U+00F3 t\U+1EA5t c\U+1EA3: " (rtos dem 2 0) " \U+0111\U+1ED1i t\U+01B0\U+1EE3ng b\U+1ECB x\U+00F3a"
	       "\n"
	       "\n"
		"\nMake by: Nguy\U+1EC5n Th\U+00E0nh Du\U+00E2n - \U+0110\U+1ECBa Ch\U+00EDnh K48"
		"\nMobile: 0972.0168.25"
       )
)
(princ)
)



(defun ChonTextSo (ss / ss i ent str ss1) 
    (progn
      (setq i	0
	    ss1	(ssadd)
      )
      (repeat (sslength ss)
	(setq ent (ssname ss i)
	      str (cdr(assoc 1 (entget ent)))
	      i	  (+ 1 i)
	)
	(if (distof str 2)
	  (ssadd ent ss1)
	)
      )
      (if (> (sslength ss1) 0)
	ss1
      )
    )
)