;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/162604-y%C3%AAu-c%E1%BA%A7u-lisp-nh%C3%A2n-nhi%E1%BB%81u-s%E1%BB%91-v%E1%BB%9Bi-m%E1%BB%99t-s%E1%BB%91-l%E1%BB%B1a-ch%E1%BB%8Dn/
(prompt "Lenh NT")(defun C:NT() ;;;;;;;;;; NHAN VOI MOT SO ;;;;;;;;;;;
(command "undo" "BE")(setq ttt (getreal "Nhap gia tri muon nhan : "))(setq tp (getint "Nhap so thap phan : "))(princ "Chon cac Text can nhan:")(setq ss (ssget '((0 . "TEXT"))))(setq j -1)(repeat (sslength ss)(setq j (+ j 1))(setq dt1 (ssname ss j))(setq el (entget dt1) )(setq gt (cdr (assoc 1 el) ))(setq gt1 (atof gt))(setq gt2 (* gt1 ttt))(setq gt2 (rtos gt2 2 tp))(setq elt (subst (cons 1 gt2) (assoc 1 el) el))(entmod elt))(command "undo" "END"))
