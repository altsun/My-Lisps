; Last update: 2021/02/19

(vl-load-com)

;;************************HELPER FUNCTIONS
(defun ceil (x / n)
	(if (or (= (setq n (fix x)) x) (< x 0))
		n
		(1+ n)
	)
)


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

    ; Ask user
    (setq does_burst (strcase (getstring "\nDo you want to BURST: Y/<N>")))
    (setq does_scale (strcase (getstring "\nDo you want to SCALE by 10: Y/<N>")))

    ; Turn off osnap
	(setq c_osmode (getvar "osmode"))
	(setvar "osmode" 0)

    ; Audit the drawing
    (command "_.audit" "Y" "")

    ; Move entire drawing from origin point to point 0,0,0
    (command "_.move" "all" "" origin '(0.0 0.0 0.0))
    
    ; Setbylayer entire drawing
    (command "_.setbylayer" "all" "" "" "")

    ; (Optional) Burst all blocks (not including nested blocks due to longer time running)
    (if (= does_burst "Y")
        (progn
            (setq sset (ssget "X" '((0 . "INSERT"))))
            (sssetfirst nil sset)
            (C:Burst)
        )
    )

    ; Change all layers color to 252
    (setq lay_col (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
    (vlax-for lay lay_col (vla-put-color lay 252))

    ; Overkill entire drawing
    ; https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/lisp-to-explode-close-corners-overkill-purge-and-flatten-all/m-p/8388474#M376710
    (command "-overkill" "all" "" "Done")  ; "-overkill", not "_.overkill"

    ; (Optional) Scale entire drawing by factor 10 at point 0,0,0
    (if (= does_scale "Y")
        (progn
            (command "_.scale" "all" "" '(0.0 0.0 0.0) 10)
        )
    )

    ; Audit the drawing
    (command "_.audit" "Y" "")

    ; Restore osnap
	(setvar "osmode" c_osmode)

    ; End function
    (princ "BOCLEAN Done!")
)


;;************************CALCULATE CONNECTIONS OF CONNECTOR FOR FLOOR HEATING AND CLIMATE CEILING (BY SELECTING POLYLINES)
(defun c:SCNT ( / pl sum_area sum_area_m2 num_connection result)
    (if (setq pl (ssget '((0 . "*polyline"))))
        (progn
            (setq sum_area 0)
            (repeat (setq i (sslength pl))
                (setq sum_area
                    (+ (vla-get-area
                        (vlax-ename->vla-object (ssname pl (setq i (1- i))))
                        )
                    sum_area
                    )
                )
            )
            (setq sum_area_m2 (/ sum_area 1000000))
            (setq num_connection (ceil (/ sum_area_m2 12)))  ; Divide area (m2) by 12 then round up to nearest integer
            (setq result (strcat "\nZone Areas: " (rtos sum_area_m2 2 1) " m2" "\nNumber of Connections: " (rtos num_connection 2 0)))
            (alert result)
            (prompt result)
        )
    )
    (princ)
)