;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=38369&st=0&p=139366&hl=esport113&fromsearch=1&#entry139366
(defun c:stt (/ ans ins lst blkName tagName ent);Block Order
  ;;  By : Gia_Bach, www.CadViet.com      ;;
  (vl-load-com)
  (while (not (and
		(setq ent (car (nentsel "\n Chon thuoc tinh can danh so: ")))
		(if ent (eq (cdr (assoc 0 (entget ent))) "ATTRIB") ) ) )
    (princ "\n Ban chon nham roi! ")    )
  (setq blkName (cdr (assoc 2 (entget (cdr (assoc 330 (entget ent))))))
	tagName (cdr (assoc 2 (entget ent)))	)
  
  (initget 1 "Yes No")
  (setq x (getkword "\nBan co muon nhap Tien to ? (Yes or No) "))
  (if (= x "Yes")
    (progn
      (or prefix (setq  prefix "KC-"))
      (setq ans (getstring t (strcat "\n Nhap tien to <<"prefix ">> :")))
      (if (/= ans "")(setq prefix ans)) )
    (setq prefix ""))

  (or stt (setq stt 1))
  (initget 6)
  (setq ans (getint (strcat "\n Nhap so bat dau <<"(itoa stt) ">> :")))
  (if ans (setq stt ans))
  (if (> stt 9)
    (setq str (strcat prefix (itoa stt)))
    (setq str (strcat prefix  "0" (itoa stt))) )    
	
  (princ "\nChon Khung ten can danh so thu tu :")
  (if (ssget(list (cons 0 "INSERT")(cons 66 1)(cons 2 blkName)))
    (progn
      (vlax-for e (vla-get-ActiveSelectionSet (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(setq ins (vlax-safearray->list (variant-value (vla-get-InsertionPoint e)))
	      lst (cons (list e ins)lst))	)
      (setq lst (vl-sort lst '(lambda (x y) (or	(> (cadr (cadr x)) (cadr (cadr y)))
						(and (< (car (cadr x)) (car (cadr y)))
						     (= (cadr (cadr x)) (cadr (cadr y))) ) ) ) ))
      (foreach e (append (mapcar 'car lst) )
	(foreach Att (vlax-invoke e 'GetAttributes)
	  (if (= (vla-get-TagString att) tagName)
	    (vla-put-TextString att str) ))
	(setq stt (+ 1 stt))
	(if (> stt 9)
	  (setq str (strcat prefix (itoa stt)))
	  (setq str (strcat prefix  "0" (itoa stt))) )	)  ) )
  (princ))
