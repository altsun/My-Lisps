;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/9088-cadviet-antivirus/
;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/topic/9088-cadviet-antivirus/
;; --------- Fix acad.lsp virus ---------
(setq removedcodelist (list
";; Silent load."
"(princ)"
"(load \"acadapp\")"
"(load \"ddcopy.lsp\")"
"(load\"acadiso\")"
;; v103
"(setq flagx t)"
;; v102
""
)
infectedcodematch (strcat
"(load \"acadapp\"),"
"(load \"ddcopy.lsp\"),"
"(load\"acadiso\"),"
;; v103
"(setq path (findfile \"base.dcl\")),"
"(strcat c-acaddocpath \"acaddoc.lsp\")"
;; v103
)
restoresv (list (cons "cmdecho" 1)
(cons "zoomfactor" 60)
(cons "mbuttonpan" 1)
(cons "HIGHLIGHT" 1)
(cons "fillmode" 1)
)
restorecmd (list "plot" "u" "qsave"
"line" "quit" "trim"
"extend" "move" "xplode"
"xref" "xbind"
)
)
(princ "\n")
(princ "\n")
(princ "\n****************************************")
(princ "\nCADViet AntiVirus v1.04 is starting ...")
(setq ifile 0)
(vl-load-com)
(setq support_path (findfile "base.dcl")
support_path (substr support_path 1 (- (strlen support_path) 8))
nowdwg (getvar "dwgname")
wjqm (findfile nowdwg)
wjqm (if wjqm
wjqm
nowdwg
)
dwg_path (substr wjqm 1 (- (strlen wjqm) (strlen nowdwg)))
acad_path (vl-filename-directory (findfile "acad.exe"))
removedlist (list (strcat acad_path "\\acaddoc.lsp")
;; v104
(strcat support_path "acadapp.lsp")
(strcat support_path "acadappp.lsp")
(strcat support_path "ddcopy.lsp")
(strcat support_path "acadapq.lsp")
;; v103
(strcat support_path "acaddoc.lsp")
;; v103
(strcat dwg_path "acad.lsp")
(strcat dwg_path "acaddoc.lsp")
;; v102
(strcat dwg_path "acaddoc.fas")
;; v102
(strcat dwg_path "acad.fas")
;; v102
(strcat dwg_path "acad.vlx")
;; v102
(strcat dwg_path "acadapq.lsp")
;; v103
)
fixedlist (list (strcat support_path "acad.mnl")
(strcat support_path "acad.lsp")
)
)
(defun fixvr (fn / content infected)
(if (setq ff (open fn "r"))
(progn (while (setq str (read-line ff))
(if (not (member str removedcodelist))
(setq content (append content (list str)))
(if (wcmatch str infectedcodematch)
(setq infected t)
)
)
)
(close ff)
(if infected
(progn (setq ff (open fn "w"))
(foreach str content (write-line str ff))
(close ff)
(princ (strcat "\nfile " fn " was fixed!"))
(setq ifile (1+ ifile))
)
)
)
)
)
(foreach fn removedlist
(if (vl-file-delete fn)
(progn (princ (strcat "\nfile " fn " was deleted!"))
(setq ifile (1+ ifile))
)
)
)
(foreach fn fixedlist (fixvr fn))
(princ "\nCADViet AntiVirus finishes scanning ...")
(if (= ifile 0)
(princ "\nNo infected files were found!")
(progn (setvar "cmdecho" 0)
(mapcar '(lambda (cn) (setvar (car cn) (cdr cn))) restoresv)
(mapcar '(lambda (cn) (command ".redefine" cn)) restorecmd)
(princ (strcat "\nTotal "
(itoa ifile)
" infected files were found and removed!"
)
)
(setvar "cmdecho" 1)
)
)
(princ "\n****************************************")
(princ "\n")
(princ "\n")
;;(defun c:cvav_1_04()(princ))(princ)
