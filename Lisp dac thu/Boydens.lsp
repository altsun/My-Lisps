; Last update: 2021/01/07

(vl-load-com)

;;************************CLEAN CAD DRAWING
(defun c:BOCLEAN ( / *error* origin does_scale sset)
    ; *error* local redefinition
    (defun *error* (msg)
        (if (/= msg "Function cancelled")
            (princ (strcat "\nError: " msg))
        )
    )

    ; Pick origin point
    (setq origin (getpoint "\nPick origin point: "))

    ; Ask if user want to scale the drawing by 10
    (setq does_scale (strcase (getstring "\nDo you want to scale the drawing by 10: Y/<N>")))

    ; Turn off osnap
	(setq c_osmode (getvar "osmode"))
	(setvar "osmode" 0)

    ; Move entire drawing from origin point to point 0,0,0
    (command "_.move" "all" "" origin '(0.0 0.0 0.0))
    
    ; Setbylayer entire drawing
    (command "_.setbylayer" "all" "" "" "")

    ; Burst all blocks (not including nested blocks due to longer time running)
    (setq sset (ssget "X" '((0 . "INSERT"))))
    (sssetfirst nil sset)
    (C:Burst)

    ; Change all layers color to 252
    (setq lay_col (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
    (vlax-for lay lay_col (vla-put-color lay 252))

    ; (Optional) Scale entire drawing by factor 10 at point 0,0,0
    (if (= does_scale "Y")
        (progn
            (command "_.scale" "all" "" '(0.0 0.0 0.0) 10)
        )
    )

    ; Restore osnap
	(setvar "osmode" c_osmode)

    ; End function
    (princ "BOCLEAN Done!")
)