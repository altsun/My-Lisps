;; free lisp from cadviet.com
;;; this lisp was downloaded from https://www.cadviet.com/forum/topic/57149-y%C3%AAu-c%E1%BA%A7u-lisp-%C4%91%E1%BB%95i-t%C3%AAn-blog-%C4%91%C6%B0%E1%BB%A3c-ch%E1%BB%8Dn/
;| Change Anonymous Block to normal with new Name
@ Ketxu 27 - 9 - 2011
|;
(defun c:nb( / blkObj blkName blkNew_Name fn pt)
(vl-load-com)
(defun ST:SS->List-Vla (ss / n e l)
 (setq n (sslength ss))
 (while (setq e (ssname ss (setq n (1- n))))
(setq l (cons (vlax-ename->vla-object e) l))
 )
)
(defun change_block(old new)
(foreach blkObj (setq ss (ST:SS->List-Vla (ssget (list (cons 0 "INSERT")(cons 2 old)))))
(vla-put-name blkObj new);;change the name
(vla-update blkObj)
)
)
(grtext -1 "Free Lisp From Cadviet @Ketxu")
(setvar "cmdecho" 0)
(setq
blkObj (vlax-ename->vla-object (car(entsel "\nBlock Source :")))
blkName (vlax-get-property blkObj
(if (vlax-property-available-p blkObj 'EffectiveName) 'EffectiveName 'Name)
 ) 
blkNew_Name (getstring "\n New Name :")
 fn (strcat (getenv "TEMP") "\\" blkNew_Name ".dwg")
)
(command ".-wblock" fn "_Y" blkName "") 
(command "._insert" (strcat blkNew_Name "=" fn) nil )
(if (wcmatch "`*" (substr blkName 1 1))(setq blkName (strcat "`*" (substr blkName 2))))
(change_block blkName blkNew_Name)
(vl-file-delete  fn)
)
