(defun c:TxtSR (/ ss en ed blk n str1 str2)
  (prompt "Select entities to search through: ")
  (setq    ss (ssget)
    n  0
  ) ;_ end of setq
  (setq str1 (getstring t "Enter string to search for: "))
  (setq str2 (getstring t "Enter string to replace with: "))

  (defun ReplaceString (ed / str m)
    (setq str (cdr (assoc 1 ed))
      m   0
    ) ;_ end of setq
    (while (setq m (vl-string-search str1 str m))
      (setq str (vl-string-subst str2 str1 str m))
      (setq m (1+ m))
    ) ;_ end of while
    (setq ed (subst (vl-list* 1 str) (assoc 1 ed) ed))
    (entmod ed)
    (entupd (cdr (assoc -1 ed)))
    ed
  ) ;_ end of defun

  (while (< n (sslength ss))
    (setq en (ssname ss n)
      ed (entget en)
    ) ;_ end of setq
    (if    (assoc 1 ed)
      (setq ed (ReplaceString ed))
    ) ;_ end of if
    (if    (= "INSERT" (cdr (assoc 0 ed)))
      (progn
    ;; Search through block's entities
    (setq blk en
          en  (tblsearch "BLOCK" (cdr (assoc 2 ed)))
    ) ;_ end of setq
    (if en
      (setq en (cdr (assoc -2 en)))
    ) ;_ end of if
    (while (and en (setq ed (entget en)))
      (if (assoc 1 ed)
        (setq ed (ReplaceString ed))
      ) ;_ end of if
      (setq en (entnext en))
    ) ;_ end of while

    ;; Search through attributes following block
    (setq en (entnext blk))
    (while (and en (setq ed (entget en)))
      (if (= "ATTRIB" (cdr (assoc 0 ed)))
        (setq ed (ReplaceString ed))
      ) ;_ end of if
      (setq en (entnext en))
    ) ;_ end of while

    (entupd blk)
      ) ;_ end of progn
    ) ;_ end of if
    (setq n (1+ n))
  ) ;_ end of while
  (setq ss nil)
  (gc)
) ;_ end of defun