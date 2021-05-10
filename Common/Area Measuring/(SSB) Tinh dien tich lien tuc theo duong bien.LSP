; Created by duongphamhn97@gmail.com
; Last update: 2021-05-10

(defun c:SSB ( / pl sum_area area loop ent_name element s1 otext ot nt result)
    (setq sum_area 0)

    ;; *error* local redefinition
    (defun *error* (msg)
        (if (/= msg "Function cancelled")
            (progn
                (princ (strcat "\nError: " msg))

                ; Alert total area
                (setq result (strcat "\nTotal area: " (rtos sum_area 2 2) " (copied to clipboard!)"))
                (alert result)
                (prompt result)

                ; Copy total area to clipboard
                (_SetClipBoardText (rtos sum_area 2 2))
            )
        )
    )

    (setq loop T)
    (while loop
        ; Select a polyline
        (setq ent_name (car(entsel "\n Select a polyline:")))
        (setq element (entget ent_name))
        (if
            (wcmatch (setq etype (cdr(assoc 0 element))) "*POLYLINE")  ; check if selection is a polyline, then
            (progn
                (setq area (vla-get-area (vlax-ename->vla-object ent_name)))
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