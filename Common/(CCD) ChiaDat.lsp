
;;;=================================
;;;DIALOG FUNCTIONS
;;;=================================
(defun begin_dialog(DiaFile DiaName)
(setq i (load_dialog DiaFile))
(if (not (new_dialog DiaName i)) (exit))
(action_tile "cancel" "(done_dialog) (command \"regen\") (exit)")
)
;;;-------------------------------------------------------------
(defun end_dialog()
(start_dialog) (unload_dialog i)
)
;;;-------------------------------------------------------------
(defun set_list(MyTile MyList MyVal / j x)
(start_list MyTile)
    (setq j 0)
    (while (setq x (nth j MyList))
	(add_list x)
	(setq j (1+ j))
    )
(end_list)
(set_tile MyTile MyVal)
)
;;;-------------------------------------------------------------


;;;=================================
;;;PUBLIC FUNCTIONS
;;;=================================
(defun GetMid (p1 p2)
;;;Midpoint: p1, p2
    (polar p1 (angle p1 p2) (/ (distance p1 p2) 2))
)
;;;-------------------------------------------------------------
(defun ints (e1 e2 / ob1 ob2 V L1 L2)
;;;Intersections of e1, e2. Return LIST of points
;;;Thank Mr. Hoanh for this function!
(setq
    ob1 (vlax-ename->vla-object e1)
    ob2 (vlax-ename->vla-object e2)
)
(setq V (vlax-variant-value (vla-IntersectWith ob1 ob2 acExtendOtherEntity)))
(if (/= (vlax-safearray-get-u-bound V 1) -1)
    (progn
          (setq L1 (vlax-safearray->list V) L2 nil)
          (while L1
	(setq L2 (append L2 (list (list (car L1) (cadr L1) (caddr L1)))))
	(repeat 3 (setq L1 (cdr L1)))
          )
    )
    (setq L2 nil)
)
  L2
)
;;;-------------------------------------------------------------
(defun getVert (e / i L)
;;;Return list of all vertex from pline e
(setq i 0 L nil)
(vl-load-com)
(repeat (fix (+ (vlax-curve-getEndParam e) 1))
    (setq L (append L (list (vlax-curve-getPointAtParam e i))))
    (setq i (1+ i))
)
L
)
;;;-------------------------------------------------------------
(defun sideP (p1 p2 e / p1n p2n)
;;;Check same side of 2 points by line e, return T or nil
(command "ucs" "n" "ob" e)
(setq
    p1n (trans p1 0 1)
    p2n (trans p2 0 1)
)
(command "ucs" "p")
(>= (* (cadr p1n) (cadr p2n)) 0)
)
;;;-------------------------------------------------------------
(defun wtxt (txt p / sty d h) ;;;Write txt on graphic screen, defaul setting
(setq
    sty (getvar "textstyle")
    d (tblsearch "style" sty)
    h (cdr (assoc 40 d))
)
(if (= h 0) (setq h (cdr (assoc 42 d))))
(entmake
    (list (cons 0 "TEXT") (cons 7 sty) (cons 1 txt) (cons 10 p) (cons 40 h) (assoc 41 d))
)
)
;;;-------------------------------------------------------------
(defun LastLoad( / K)
(setq K (strcat
    "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\"
    "CurrentVersion\\Explorer\\ComDlg32\\OpenSaveMRU\\*"
))
(vl-registry-read K (substr (vl-registry-read K "MRUList") 1 1))
)
;;;-------------------------------------------------------------
(defun addspath(s) ;;;Add support file search path
(setenv "ACAD" (strcat (getenv "ACAD") ";" s))
)
;;;-------------------------------------------------------------



;;;=================================
;;;PRIVATE FUNCTIONS
;;;=================================
(defun area_DL (p)
;;;Get area. Specify by e0, e1, p
;;;Filtered vertex, same side with p
(setq Lf (ints e0 e1))
(foreach x L0
    (if (sideP x p e1) (setq Lf (append Lf (list x))))
)
;;;Convert to curve-param and sort
(setq Lpara nil)
(foreach x Lf
    (setq para (vlax-curve-getParamAtPoint e0 x))
    (setq Lpara (append Lpara (list para)))
)
(setq Lpara (vl-sort Lpara '<))
;;;Get area
(command ".area")
(foreach x Lpara (command (vlax-curve-getPointAtParam e0 x)))
(command "")
(setq S (getvar "area"))
)
;;;-------------------------------------------------------------
(defun move_slow (e ag dr) ;;;Move e by angle ag, step dr
(if (= song 1)
    (command "move" e "" (list 0 0) (polar (list 0 0) ag dr))
    (if (> dr 0)
        (command "rotate" e "" pc "r" pC pM pN)
        (command "rotate" e "" pc "r" pC pN pM)
    )
)
)
;;;-------------------------------------------------------------
(defun RunDL ()
(setvar "cmdecho" 0)
(setvar "osmode" 0)
(setq OK nil)
(while (not OK)
    (setq
         Li (ints e0 e1)
         i (- (length Li) 1)
         pM (getMid (car Li) (nth i Li))
         pN (polar pM ag tol)
         St (area_DL pN)
    )
    (if (<= (* St flag) (* S1 flag))
         (progn (setq flag (* flag -1)) (setq OK T))
         (move_slow e1 ag (* flag tol))
    )
)
)
;;;-------------------------------------------------------------
(defun ActTyle()
(setq S1 (* S0 (atof $value)))
(set_tile "dientich" (rtos S1))
)
;;;-------------------------------------------------------------
(defun ActDientich()
(setq k (/ (atof $value) S0))
(set_tile "tyle" (rtos k))
)
;;;-------------------------------------------------------------
(defun theoTL()
(mode_tile "tyle" 0)
(mode_tile "dientich" 1)
)
;;;-------------------------------------------------------------
(defun theoDT()
(mode_tile "tyle" 1)
(mode_tile "dientich" 0)
)
;;;-------------------------------------------------------------
(defun SL_chiadat()
(setq
    byDT (atoi (get_tile "theodt"))
    byTL (atoi (get_tile "theotl"))
    S1 (atof (get_tile "dientich"))
    k (atof (get_tile "tyle"))
    Acc (atoi (get_tile "chinhxac"))
    Song (atoi (get_tile "song"))
    Ghi (atoi (get_tile "ghi"))
)
)
;;;-------------------------------------------------------------
(defun Dialog_chiadat()
(begin_dialog "chiadat.dcl" "chiadat")

(set_tile "tong" (strcat "Dien tich tong cong = " (rtos S0)))
(set_tile "theodt" (itoa byDT))
(set_tile "theotl" (itoa byTL))
(mode_tile "dientich" byTL)
(mode_tile "tyle" byDT)
(set_tile "dientich" (rtos S1))
(set_tile "tyle" (rtos k))
(set_list "chinhxac" AccList  (itoa Acc))
(set_tile "song" (itoa song))
(set_tile "quay" (itoa quay))
(set_tile "ghi" (itoa ghi))

(action_tile "theodt" "(theoDT)")
(action_tile "theotl" "(theoTL)")
(action_tile "tyle" "(ActTyle)")
(action_tile "dientich" "(ActDientich)")
(action_tile "accept" "(SL_chiadat) (done_dialog)")

(end_dialog)
)
;;;-------------------------------------------------------------
(defun GhiDT()
(wtxt (rtos S1) (getpoint "\nDiem chuan ghi dien tich chia:"))
(wtxt (rtos (- S0 S1)) (getpoint "\nDiem chuan ghi dien tich con lai:"))
)
;;;-------------------------------------------------------------


;;;=================================
;;;MAIN PROGRAM
;;;=================================
(defun C:ccd (/ e0 e1 Li i di p0 k tol S0 p1 ag L0 OK Lf x
	     p Lpara para S oldos S00 flag pM pN St prec)
(vl-load-com)
;;;CHON PLINE VA DUONG CHIA
(setq e0 (car (entsel "\nChon 1 pline kin:")))
(redraw e0 3)
(setq
    e1 (car (entsel "\nChon duong chia, cat pline it nhat tai 2 diem:"))
    Li (ints e0 e1)
)
(redraw e1 3)
(if (< (length Li) 2) (progn (alert "\nKhong tim thay 2 giao diem!") (progn (command "regen") (exit))))
(setq
    i (- (length Li) 1)
    di (distance (car Li) (nth i Li))
    p0 (getpoint "\nPick 1 diem, ve phia can chia so voi duong chuan:")
)

;;;GOI DIALOG
(setq S0 (vlax-curve-getArea e0))
(if (not S1) (setq S1 (/ S0 2)))

(if (not byDT) (setq byDT 1))
(if (= byDT 1) (setq byTL 0) (setq byTL 1))
(if (not Acc) (setq Acc 4))
(if (not song) (setq song 1))
(if (= song 1) (setq quay 0) (setq quay 1))
(if (not ghi) (setq ghi 0))
(setq
    k (/ S1 S0)
    AccList (list "0" "0.0" "0.00" "0.000" "0.0000" "0.00000" "0.000000" "0.0000000" "0.00000000")
)
(Dialog_chiadat)
(command "regen")

;;;TINH TOAN
(if (= song 0) (setq pc (getpoint "\nChon diem co dinh:")))
(setq
    L0 (getVert e0) ;;;List of all vertex
    S00 (area_DL p0)
    St S00
    p1 (vlax-curve-getClosestPointTo e1 p0)
    ag (angle p1 p0)
    prec (expt 10.0 (- acc))
    oldos (getvar "osmode")
)
(cond
    ((<= (abs (- S00 S1)) prec) (progn (alert "Duong chia da dung vi tri!") (command "regen") (exit)))
    ((> S00 S1) (setq flag 1))
    ((< S00 S1) (setq flag -1))
)
(setq tol (* di 0.01))

;;;RUN DIVIDE LAND
(while (> (abs (- St S1)) prec) (runDL) (setq tol (* 0.1 tol)))
(alert "FINISH!")

;;;GHI DIEN TICH
(if (= ghi 1) (GhiDT))

;;;KET THUC
(setvar "cmdecho" 1)
(setvar "osmode" oldos)
(command "regen")
(princ)
)
;;;=================================
;;;Add support file search path
(if (not (findfile "Chiadat.lsp")) (addspath (vl-filename-directory (LastLoad))))
;;;=================================
