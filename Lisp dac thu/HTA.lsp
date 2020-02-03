;;************************LENH TAT CHON LAYER

(defun c:Q1 () (command "_layer" "Set" "HTA - Li\U+1EC1n \U+0111\U+1EADm 1" ""))  ; HTA - Liền đậm 1
(defun c:Q2 () (command "_layer" "Set" "HTA - Li\U+1EC1n \U+0111\U+1EADm 2" ""))  ; HTA - Liền đậm 2
(defun c:Q3 () (command "_layer" "Set" "HTA - Li\U+1EC1n m\U+1EA2nh" ""))  ; HTA - Liền mảnh
(defun c:Q4 () (command "_layer" "Set" "HTA- Tr\U+1EE5c" ""))  ; HTA- Trục
(defun c:Q5 () (command "_layer" "Set" "HTA - Ch\U+00DA gi\U+1EA2i" ""))  ; HTA - Chú giải
(defun c:Q6 () (command "_layer" "Set" "HTA - C\U+1EEDa" ""))  ; HTA - Cửa
(defun c:Q7 () (command "_layer" "Set" "HTA - N\U+1ED9i th\U+1EA5t" ""))  ; HTA - Nội thất
(defun c:Q8 () (command "_layer" "Set" "HTA - N.m\U+1EDD" ""))  ; HTA - N.mờ
(defun c:Q9 () (command "_layer" "Set" "HTA - N.\U+0111\U+1EE9t" ""))  ; HTA - N.đứt
(defun c:Q0 () (command "_layer" "Set" "Do dien tich" ""))


;;************************LENH TAT CHUYEN LAYER CUA DOI TUONG
(defun tolayer ( ss lay / i e )
    ;;; Custom function
    ;;; Credit: https://www.cadtutor.net/forum/topic/37328-lisp-to-move-selected-objects-to-a-specified-layer/?tab=comments#comment-304488
    ;;; ss - pickset
    ;;; lay -layer name
    (repeat (setq i (sslength ss))
        (entmod
            (subst
                (cons 8 lay)
                (assoc 8 (entget (setq e (ssname ss (setq i (1- i))))))
                (entget e)
            )
        )
    )
)
(defun c:QQ1 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - Li\U+1EC1n \U+0111\U+1EADm 1"  ; HTA - Liền đậm 1
    )
)
(defun c:QQ2 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - Li\U+1EC1n \U+0111\U+1EADm 2"  ; HTA - Liền đậm 2
    )
)
(defun c:QQ3 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - Li\U+1EC1n m\U+1EA2nh"  ; HTA - Liền mảnh
    )
)
(defun c:QQ4 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA- Tr\U+1EE5c"  ; HTA- Trục
    )
)
(defun c:QQ5 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - Ch\U+00DA gi\U+1EA2i"  ; HTA - Chú giải
    )
)
(defun c:QQ6 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - C\U+1EEDa"  ; HTA - Cửa
    )
)
(defun c:QQ7 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - N\U+1ED9i th\U+1EA5t"  ; HTA - Nội thất
    )
)
(defun c:QQ8 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - N.m\U+1EDD"  ; HTA - N.mờ
    )
)
(defun c:QQ9 ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - N.\U+0111\U+1EE9t"  ; HTA - N.đứt
    )
)
(defun c:QQ0 ()
    (tolayer
        (ssget "_:L")  ; selection
        "Do dien tich"  ; Do dien tich
    )
)

;;************************LENH TAT CHEN BLOCK

(defun c:BD1 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")  ; HTA - Cửa
    (command "insert" "CUA DI" pause 1 1 0)  ; Cửa 1, 2, 4 cánh, cửa 30
)
(defun c:BD2 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D1s_plan" pause 1 1 0)  ; Cửa trượt dọc tường 1 cánh
)
(defun c:BD3 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D2s_plan" pause 1 1 0)  ; Cửa trượt dọc tường 2 cánh
)
(defun c:BD4 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D2b_acd" pause 1 1 0)  ; Cửa trượt 1 cánh
)
(defun c:BD5 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D4b_slide" pause 1 1 0)  ; Cửa trượt 2 cánh
)
(defun c:BD6 ()
    (command "_layer" "Set" "HTA - C\U+1EEDa" "")
    (command "insert" "D4_plan" pause 1 1 0)  ; Cửa 4 cánh gấp
)


;;************************LENH TAT HATCH NHANH

(defun c:H1 () ;; Solid
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "solid" pause)
)
(defun c:H2 ()  ; Tường
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ansi31" "350" "0" pause)
)
(defun c:H3 ()  ; Bê tông
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ar-conc" "20" "0" pause)
)
(defun c:H33 ()  ; Bê tông cốt thép (dùng custom hatch)
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "1-btct" "20" "0" pause)
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
(defun c:H8 ()  ; Ngói
    (command "_layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "-bhatch" "P" "ar-rshke" "20" "0" pause)
)