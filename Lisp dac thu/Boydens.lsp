; Last update: 2021/01/18

(vl-load-com)

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