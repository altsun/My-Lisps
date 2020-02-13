;;************************LỆNH TẮT CHỌN LAYER

(defun c:Q1 () (command "_.layer" "Set" "HTA - Li\U+1EC1n \U+0111\U+1EADm 1" ""))  ; HTA - Liền đậm 1
(defun c:Q2 () (command "_.layer" "Set" "HTA - Li\U+1EC1n \U+0111\U+1EADm 2" ""))  ; HTA - Liền đậm 2
(defun c:Q3 () (command "_.layer" "Set" "HTA - Li\U+1EC1n m\U+1EA2nh" ""))  ; HTA - Liền mảnh
(defun c:Q4 () (command "_.layer" "Set" "HTA- Tr\U+1EE5c" ""))  ; HTA- Trục
(defun c:Q5 () (command "_.layer" "Set" "HTA - Ch\U+00DA gi\U+1EA2i" ""))  ; HTA - Chú giải
(defun c:Q6 () (command "_.layer" "Set" "HTA - C\U+1EEDa" ""))  ; HTA - Cửa
(defun c:Q7 () (command "_.layer" "Set" "HTA - N\U+1ED9i th\U+1EA5t" ""))  ; HTA - Nội thất
(defun c:Q8 () (command "_.layer" "Set" "HTA - N.m\U+1EDD" ""))  ; HTA - N.mờ
(defun c:Q9 () (command "_.layer" "Set" "HTA - N.\U+0111\U+1EE9t" ""))  ; HTA - N.đứt
(defun c:Q` () (command "_.layer" "Set" "HTA - Ket cau" ""))  ; HTA - Ket cau
(defun c:Q0 () (command "_.layer" "Set" "Do dien tich" ""))


;;************************LỆNH TẮT CHUYỂN LAYER CỦA ĐỐI TƯỢNG

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
(defun c:QQ` ()
    (tolayer
        (ssget "_:L")  ; selection
        "HTA - Ket cau"  ; HTA - Ket cau
    )
)
(defun c:QQ0 ()
    (tolayer
        (ssget "_:L")  ; selection
        "Do dien tich"  ; Do dien tich
    )
)


;;************************LỆNH TẮT KHI DIM TỰ CHUYỂN LAYER

(defun my_dim (dim_command / *error* current_lay)
    ;; *error* local redefinition
    (defun *error* (msg)
        (if (/= msg "Function cancelled")
            (princ (strcat "\nError: " msg))
        )
        (if current_lay
            (setvar "CLAYER" current_lay)
        )
    )
    (setq current_lay (getvar "CLAYER"))
    ; Check if layer HTA - Chú giải exists and is on
    (if (and
            ; Check if layer HTA - Chú giải exists
            (tblsearch "layer" "HTA - Ch\U+00DA gi\U+1EA2i")  ; HTA - Chú giải
            ; Check if layer HTA - Chú giải is on
            (not (<
                (cdr (assoc 62 (entget (tblobjname "layer" "HTA - Ch\U+00DA gi\U+1EA2i"))))  ; HTA - Chú giải
                0
            ))
        )
        (setvar "CLAYER" "HTA - Ch\U+00DA gi\U+1EA2i")  ; HTA - Chú giải
    )
    (command dim_command)
    (while (= 1 (getvar "cmdactive")) (command pause))
    (setvar "CLAYER" current_lay)
)
(defun c:DF ()
    (my_dim "_.dimaligned")
)
(defun c:DG ()
    (my_dim "_.dimlinear")
)
(defun c:DD ()
    (my_dim "_.dimcontinue")
)


;;************************LỆNH TẮT CHÈN BLOCK

(defun c:BD1 ()
    (command "_.layer" "Set" "HTA - C\U+1EEDa" "")  ; HTA - Cửa
    (command "_.insert" "CUA DI" pause 1 1 0)  ; Cửa 1, 2, 4 cánh, cửa 30
)
(defun c:BD2 ()
    (command "_.layer" "Set" "HTA - C\U+1EEDa" "")
    (command "_.insert" "D1s_plan" pause 1 1 0)  ; Cửa trượt dọc tường 1 cánh
)
(defun c:BD3 ()
    (command "_.layer" "Set" "HTA - C\U+1EEDa" "")
    (command "_.insert" "D2s_plan" pause 1 1 0)  ; Cửa trượt dọc tường 2 cánh
)
(defun c:BD4 ()
    (command "_.layer" "Set" "HTA - C\U+1EEDa" "")
    (command "_.insert" "D2b_acd" pause 1 1 0)  ; Cửa trượt 1 cánh
)
(defun c:BD5 ()
    (command "_.layer" "Set" "HTA - C\U+1EEDa" "")
    (command "_.insert" "D4b_slide" pause 1 1 0)  ; Cửa trượt 2 cánh
)
(defun c:BD6 ()
    (command "_.layer" "Set" "HTA - C\U+1EEDa" "")
    (command "_.insert" "D4_plan" pause 1 1 0)  ; Cửa 4 cánh gấp
)


;;************************LỆNH TẮT HATCH NHANH

(defun c:H1 () ;; Solid
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "solid" pause)
)
(defun c:H2 ()  ; Tường
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "ansi31" "350" "0" pause)
)
(defun c:H3 ()  ; Bê tông
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "ar-conc" "20" "0" pause)
)
(defun c:H33 ()  ; Bê tông cốt thép (dùng custom hatch)
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "1-btct" "20" "0" pause)
)
(defun c:H4 ()  ; Gỗ
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "gost_wood" "10" "45" pause)
)
(defun c:H5 ()  ; Kính
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "ar-rroof" "200" "45" pause)
)
(defun c:H6 ()  ; Gạch vuông
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "net" "2400" "0" pause)
)
(defun c:H7 ()  ; Chấm
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "ar-sand" "70" "0" pause)
)
(defun c:H8 ()  ; Ngói
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "ar-rshke" "20" "0" pause)
)