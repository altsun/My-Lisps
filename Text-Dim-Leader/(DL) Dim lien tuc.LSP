(defun c:dl (/	      tapdiem  temp	diem	 diemchan diemduong
	     xc	      yc       xd	yx	 dx	  dy
	     kieuduong	       tapdtdave	 tapchan  pttruoc
	     ptht     x	       y
	    )

  (setq tapdiem nil)
  (while (setq
	   temp	(getpoint "\nVao diem danh dim (Go Enter de ket thuc): ")
	 )
    (setq tapdiem (append tapdiem (list temp))
	  diem	  temp
    )
    (grdraw diem (polar diem (/ pi 2) 100.0) 1)
    (grdraw diem (polar diem 0.0 100.0) 1)
    (grdraw diem (polar diem pi 100.0) 1)
    (grdraw diem (polar diem (* pi 1.5) 100.0) 1)
  )
  (setq	diemchan  (getpoint "\nVao diem chan dim: ")
	diemduong (getpoint diemchan "\nVao diem duong dim: ")
	xc	  (car diemchan)
	yc	  (cadr diemchan)
	xd	  (car diemduong)
	yd	  (cadr diemduong)
	dx	  (abs (- xc xd))
	dy	  (abs (- yc yd))
	kieuduong (cond
		    ((>= dx dy) 2)
		    ((< dx dy) 1)
		  )
	tapdtdave nil
  )

  (setvar "osmode" 0)
  (if (= kieuduong 1)
    (progn
      (setq tapchan (mapcar '(lambda (x)
			       (list
				 (list (car x) (cadr diemchan))
				 (list (car x) (cadr diemduong))
			       )
			     )
			    tapdiem
		    )
	    pttruoc (car tapchan)
	    tapchan (cdr tapchan)
      )

      (foreach ptht tapchan
	(command ".dimlinear" (car pttruoc) (car ptht) (cadr ptht))
	(if tapdtdave
	  (setq tapdtdave (ssadd (entlast) tapdtdave))
	  (setq tapdtdave (ssadd (entlast)))
	)
	(setq pttruoc ptht)
      )
    )
    (progn
      (setq tapchan (mapcar '(lambda (x)
			       (list
				 (list (car diemchan) (cadr x))
				 (list (car diemduong) (cadr x))
			       )
			     )
			    tapdiem
		    )
	    pttruoc (car tapchan)
	    tapchan (cdr tapchan)
      )
      (foreach ptht tapchan
	(command ".dimlinear" (car pttruoc) (car ptht) (cadr ptht))
	(if tapdtdave
	  (setq tapdtdave (ssadd (entlast) tapdtdave))
	  (setq tapdtdave (ssadd (entlast)))
	)
	(setq pttruoc ptht)
      )
    )
  )

  (sssetfirst nil tapdtdave)
  (redraw)
  (done)
)