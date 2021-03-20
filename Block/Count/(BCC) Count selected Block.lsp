(vl-load-com)

(defun c:BCC ( / ent data block_name ss)
	(if (setq ent (car (entsel "Chon block: \n")))
		(progn
			(setq data (entget ent))
			(if (= (cdr (assoc 0 data)) "INSERT")
				(progn
					; Get block name
					(setq block_name (cdr (assoc 2 data)))
					; Select blocks to count
					(if (setq ss (ssget (list (cons 0 "INSERT") (cons 2 block_name)) ) )  ; To us variable in ssget filter, use list and cons
						; Alert number of selected block
						(alert (strcat "Number of block " "\"" block_name "\"" ": " (itoa (sslength ss)) ) )
					)
				)
			)
		)
	)
)
