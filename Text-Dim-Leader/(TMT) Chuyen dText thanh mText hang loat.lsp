;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/topic/47264-da-xong-lisp-chuyen-dtext-thanh-mtext-nhieu-doi-tuong/
(defun c:tmt() (mapcar '(lambda(x) (command "txt2mtxt" x "")) (acet-ss-to-list (ssget '((0 . "TEXT"))))))