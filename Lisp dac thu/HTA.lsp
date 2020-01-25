;;************************LENH TAT CHON LAYER

(defun c:q1 () (command "_layer" "Set" "HTA - Li\U+1EC1n \U+0111\U+1EADm 1" ""))  ; HTA - Liền đậm 1
(defun c:q2 () (command "_layer" "Set" "HTA - Li\U+1EC1n \U+0111\U+1EADm 2" ""))  ; HTA - Liền đậm 2
(defun c:q3 () (command "_layer" "Set" "HTA - Li\U+1EC1n m\U+1EA2nh" ""))  ; HTA - Liền mảnh
(defun c:q4 () (command "_layer" "Set" "HTA- Tr\U+1EE5c" ""))  ; HTA- Trục
(defun c:q5 () (command "_layer" "Set" "HTA - Ch\U+00DA gi\U+1EA2i" ""))  ; HTA - Chú giải
(defun c:q0 () (command "_layer" "Set" "Do dien tich" ""))


;;************************LENH TAT CHEN BLOCK

(defun c:ID1 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")  ; HTA - Cửa
    (command "insert" "CUA DI" pause 1 1 0)  ; Cửa 1, 2, 4 cánh, cửa 30
)
(defun c:ID2 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D1s_plan" pause 1 1 0)  ; Cửa trượt dọc tường 1 cánh
)
(defun c:ID3 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D2s_plan" pause 1 1 0)  ; Cửa trượt dọc tường 2 cánh
)
(defun c:ID4 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D2b_acd" pause 1 1 0)  ; Cửa trượt 1 cánh
)
(defun c:ID5 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D4b_slide" pause 1 1 0)  ; Cửa trượt 2 cánh
)
(defun c:ID6 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D4_plan" pause 1 1 0)  ; Cửa 4 cánh gấp
)


;;************************LENH TAT HATCH NHANH

(defun c:H1 () ;; Solid
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "solid" "1" "0" pause)
)
(defun c:H2 ()  ; Tường
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ansi31" "350" "0" pause)
)
(defun c:H3 ()  ; Bê tông
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ar-conc" "15" "0" pause)
)
(defun c:H4 ()  ; Gỗ
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "gost_wood" "10" "45" pause)
)
(defun c:H5 ()  ; Kính
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ar-rroof" "200" "45" pause)
)
(defun c:H6 ()  ; Gạch vuông
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "net" "2400" "0" pause)
)
(defun c:H7 ()  ; Chấm
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ar-sand" "70" "0" pause)
)