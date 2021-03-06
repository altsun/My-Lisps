;;;************NI NHIU I TNG VI NHAU THANH 1 I TNG*************************
(defun c:NN (/ tdt ssdt)
 (defun ObjName (ssdt /) (cdr (assoc '0 (entget ssdt))))
 (defun MoPL (ssdt /) (= (cdr (assoc '70 (entget ssdt))) 0))

 (defun NoiPL (ssdt tdt /)
  (if (MoPL ssdt)
   (command ".PEDIT" ssdt "J" tdt "" "")
  )
 )
 (defun NoiLC (ssdt tdt /) (command ".PEDIT" ssdt "Y" "J" tdt "" ""))

 (setq tdt (ssget)
ssdt (ssname tdt 0))
 (if (or (= (Objname ssdt) "LWPOLYLINE")
(= (Objname ssdt) "POLYLINE"))
  (NoiPL ssdt tdt)
  (if (or (= (Objname ssdt) "LINE") (= (Objname ssdt) "ARC"))
   (NoiLC ssdt tdt)
  )
 )
 (princ)
)