(defun c:olt(/ st dt str lst-num)
  (setvar "cmdecho" 0)
  (vl-load-com)
  (defun Str_Split(str sym / lst i)
  ;write by Tue_NV
  (while (setq i (vl-string-search sym str 0))
    (setq lst (append lst (list (substr str 1 i)))
      i (+ i 1 (strlen sym))  str (substr str i (strlen str) ) )
  )
 (append lst (list (substr str 1 (strlen str))))
)
  (setq str (getstring "Chuoi Offset \"#,#@#,#\"  : "))
  (setq dt (car(entsel "\ndoi tuong can offset :")) pt (getpoint "\nHuong Offset :")) 
(Repeat (length (setq lst-num (reverse 
       (apply 'append
        (mapcar '(lambda(x / a lst) 
     (if (wcmatch x "*#`@#*")
          (progn (setq a (atof (last (STR_SPLIT x "@")))) (Repeat (atoi x) (setq lst (append lst (list a)))) ) (list (atof x)) ))
(STR_SPLIT str ",")) ) )  ) )  
     (command "offset" (apply '+ lst-num) dt "_non" pt "e")
     (setq lst-num (cdr lst-num))
)
(princ)
)