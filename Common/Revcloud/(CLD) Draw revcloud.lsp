;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=41998
(defun C:CLD (/ aleng p1 p2 )
(setq p1 (getpoint "\nCh\U+1ECDn \U+0111i\U+1EC3m \U+0111\U+1EA7u HCN :")
      p2 (getcorner p1 "\n\U+0110i\U+1EC3m cu\U+1ED1i :")
      aleng (getdist "\nB\U+00E1n k\U+00EDnh cong :"))
(command "_.rectangle" p1 p2)
(command "_.revcloud" "_A" aleng "" "_O" (entlast) "")
(princ)
)
