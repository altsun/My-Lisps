
(defun c:bf (/ dt diem)
(setq dt (car (entsel "\nVao doi tuong can chat")))
(if dt
(progn
(redraw dt 3)
(setq diem (getpoint "\nVao diem chat: "))
(redraw dt 4)
)
)
(if (and dt diem)
(command ".break" dt diem diem)
)
)