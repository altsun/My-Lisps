; Created by duongphamhn97@gmail.com
; Last update: 2021-05-18


(defun c:SSBO( / ss hatch_list pl_list hatch_ent pl_ent sum_area area loop ent s1 otext ot nt result)
    ;; *error* local redefinition
    (defun *error* (msg)
        (if (member msg '("Function cancelled" "quit / exit abort"))
            (progn
                (princ (strcat "\nError: " msg))

                ; Delete all hatches created
                (foreach element hatch_list
                    (entdel element)
                )
                ; Delete all polylines created
                (foreach element pl_list
                    (entdel element)
                )

                ; Alert total area
                (setq result (strcat "\nTotal area: " (rtos sum_area 2 2) " (copied to clipboard!)"))
                (alert result)
                (prompt result)

                ; Copy total area to clipboard
                (_SetClipBoardText (rtos sum_area 2 2))
            )
        )
    )

    (setq hatch_list '())
    (setq pl_list '())
    (setq sum_area 0.0)

    (setq loop T)
    (while loop
        (setq ss (ssget))
        (if ss
            (progn
                (command "_.-boundary" "a" "b" "n" "P" "" "" pause "")
                (if (/= (entlast) (car pl_list))
                    (progn
                        (setq area 0.0)
                        (setq pl_ent (entlast))
                        (setq pl_list (cons pl_ent pl_list))  ; add last created polyline (boundary) to list

                        (command "_.-bhatch" "S" pl_ent "" "P" "solid" "")  ; create hatch which boundary is last created polyline
                        (if (and
                                (/= (entlast) (car hatch_list))
                                (eq (cdr(assoc 0 (entget (entlast)))) "HATCH")
                            )
                            (progn
                                (setq hatch_ent (entlast))
                                (setq hatch_list (cons hatch_ent hatch_list))  ; add last created hatch to list

                                ; Accumulate area
                                (setq area (vla-get-area (vlax-ename->vla-object pl_ent)))
                                ;; (setq area (vla-get-area hatch_ent))
                                (setq sum_area
                                    (+ area sum_area)
                                )

                                ; Select text and write area
                                (setq s1 (entget (car (entsel "Select text to write area"))))
                                (setq otext (assoc 1 s1))
                                (setq ot (cdr otext))
                                (setq ot (read ot))
                                (setq nt (cons 1 (strcat (rtos area 2 2))))
                                (entmod (subst nt otext s1))
                            )
                        )
                    )
                )
            )
        )
    )
)


(defun _SetClipBoardText (text / htmlfile result)
 ;;  Caller's sole responsibility is to pass a
 ;;  text string. Anything else? Pie in face.
 ;;  Attribution: Reformatted version of
 ;;  post by XShrimp at theswamp.org.
 ;;  See http://tinyurl.com/2ngf4r.

 (setq	result
 (vlax-invoke
   (vlax-get
     (vlax-get
       (setq htmlfile (vlax-create-object "htmlfile"))
       'ParentWindow
     )
     'ClipBoardData
   )
   'SetData
   "Text"
   text
 )
 )
 (vlax-release-object htmlfile)
)