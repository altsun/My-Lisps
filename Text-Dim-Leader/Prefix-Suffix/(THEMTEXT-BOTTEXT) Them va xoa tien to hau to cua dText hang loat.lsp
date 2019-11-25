;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:THEMTEXT (/ c e ss txt cmde ttdangs ttdangt)
  (command "undo" "be")
  (setq cmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ttdangt (getstring 5"\nChuoi muon them phia truoc:")) 
  (setq ttdangs (getstring 5"\nChuoi muon them phia sau:")) 
  (if (null ttdangt)(setq ttdangt ""))
  (if (null ttdangs)(setq ttdangs ""))
 (prompt "\nChon chu muon chinh.")
  (setq ss (ssget))
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  (while e
    (setq e (entget e))
    ; Ensure entity is text
    (if (= (cdr (assoc 0 e)) "TEXT")
        (progn
                 (setq txt (strcat ttdangt (cdr (assoc 1 e)) ttdangs))
           (setq e (subst (cons 1 txt) (assoc 1 e) e))
           (entmod e)
        )
    )
    (setq c (1+ c)) ; Increment counter.
    (setq e (ssname ss c))  ; Obtain next entity.
   )
   (setvar "CMDECHO" cmde)
   (command "undo" "end")
      (Prin I)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:BOTTEXT (/ c e ss txt cmde tbdangs tbdangt)
  (command "undo" "be")
  (setq cmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq tbdangt (getreal "\nSo ky tu muon bot phia truoc:")) 
  (setq tbdangs (getreal "\nSo ky tu muon bot phia sau:")) 
  (if (null tbdangt)(setq tbdangt 0))
  (if (null tbdangs)(setq tbdangs 0))
  (setq sotru (+ tbdangt tbdangs))
 (prompt "\nChon chu muon chinh.")
  (setq ss (ssget))
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  (while e
    (setq e (entget e))
    ; Ensure entity is text
    (if (= (cdr (assoc 0 e)) "TEXT")
        (progn
(setq sochu (strlen (cdr (assoc 1 e))))
(if (> sochu sotru)
(progn
(setq txt (substr (cdr (assoc 1 e)) (fix (+ 1 tbdangt)) (fix (- sochu tbdangt tbdangs))))
           (setq e (subst (cons 1 txt) (assoc 1 e) e))
           (entmod e)
)
)

        )
    )
    (setq c (1+ c)) ; Increment counter.
    (setq e (ssname ss c))  ; Obtain next entity.
   )
   (setvar "CMDECHO" cmde)
   (command "undo" "end")
      (Prin I)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;