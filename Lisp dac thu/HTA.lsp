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

(defun c:E1 () (command "_.layer" "Set" "Arcoma - ME1" ""))
(defun c:E2 () (command "_.layer" "Set" "Arcoma - ME2" ""))


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

(defun c:EE1 ()
    (tolayer
        (ssget "_:L")  ; selection
        "Arcoma - ME1"  ; Arcoma - ME1
    )
)
(defun c:EE2 ()
    (tolayer
        (ssget "_:L")  ; selection
        "Arcoma - ME2"  ; Arcoma - ME2
    )
)


;;************************LỆNH TẮT VẼ CẤP THOÁT NƯỚC

(defun my_water_pline (target_lay target_celt / *error* current_lay current_celt)
    ;; *error* local redefinition
    (defun *error* (msg)
        (if (/= msg "Function cancelled")
            (princ (strcat "\nError: " msg))
        )
        (if current_lay
            (setvar "CLAYER" current_lay)
        )
        (if current_celt
            (setvar "celtscale" current_celt)
        )
    )
    (setq current_lay (getvar "CLAYER"))
    (setq current_celt (getvar "celtscale"))
    ; Check if target layer exists and is on
    (if (and
            ; Check if target layer exists
            (tblsearch "layer" target_lay)
            ; Check if target layer is on
            (not (<
                (cdr (assoc 62 (entget (tblobjname "layer" target_lay))))
                0
            ))
        )
        (setvar "CLAYER" target_lay)
    )
    (setvar "celtscale" target_celt)  ; Change celtscale
    ; Draw pline
    (command "_.pline")
    (while (= 1 (getvar "cmdactive")) (command pause))
    ; Return back layer and celtscale
    (setvar "CLAYER" current_lay)
    (setvar "celtscale" current_celt)
)

; Credit: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/changing-linetype-scale-in-lisp/m-p/887494#M113152
(defun c:QQW1 ()
    (my_water_pline "Arcoma - CN l\U+1EA1nh" 300)  ; Arcoma - CN lạnh
)
(defun c:QQW2 ()
    (my_water_pline "Arcoma - CN n\U+00F3ng" 300)  ; Arcoma - CN nóng
)
(defun c:QQW3 ()
    (my_water_pline "Nuoc-Thoat" 50)  ; Nuoc-Thoat
)
(defun c:QQW4 ()
    (my_water_pline "Nuoc-Thoat-Rua" 500)  ; Nuoc-Thoat-Rua
)
(defun c:QQW5 ()
    (my_water_pline "Nuoc-Thoat-Mua" 800)  ; Nuoc-Thoat-Mua
)
(defun c:QQW6 ()
    (my_water_pline "Arcoma - thoat nuoc d60" 800)  ; Arcoma - thoat nuoc d60
)

(defun to_my_water_pline (target_lay target_celt / )
    ; Credit: https://www.afralisp.net/archive/Tips/code35.htm
    ;load the visual lisp extensions
    (vl-load-com)
    ;retrieve a reference to the documents object
    (setq acadDocument (vla-get-activedocument
    (vlax-get-acad-object)))
    ;retrieve a reference to the selection sets object
    (setq ssets (vla-get-selectionsets acadDocument))
    ;add a new selection set
    (setq newsset (vla-add ssets "SS1"))
    ;select your new selection set objects
    (vla-selectOnScreen newsset)
    ;set the counter to zero
    (setq ctr 0)
    ;count the number of objects and loop
    (repeat (vla-get-count newsset)
    ;retrieve each object
    (setq item (vla-item newsset ctr))
    ;check if the entity has a property
    ;and it can be updated
    (setq check (vlax-property-available-p item "LinetypeScale" T))
    ;if it can
    (if check
        (progn
            ; Change it's layer
            (vlax-put-property item 'layer target_lay)
            ;change it's property
            (vlax-put-property item 'LinetypeScale target_celt)
        )
    );if
    ;increment the counter
    (setq ctr (1+ ctr))
    );repeat
    ;delete the selection set
    (vla-delete (vla-item ssets "SS1"))
    (princ)
)

(defun c:QQWW1 ()
    (to_my_water_pline "Arcoma - CN l\U+1EA1nh" 300)  ; Arcoma - CN lạnh
)
(defun c:QQWW2 ()
    (to_my_water_pline "Arcoma - CN n\U+00F3ng" 300)  ; Arcoma - CN nóng
)
(defun c:QQWW3 ()
    (to_my_water_pline "Nuoc-Thoat" 50)  ; Nuoc-Thoat
)
(defun c:QQWW4 ()
    (to_my_water_pline "Nuoc-Thoat-Rua" 500)  ; Nuoc-Thoat-Rua
)
(defun c:QQWW5 ()
    (to_my_water_pline "Nuoc-Thoat-Mua" 800)  ; Nuoc-Thoat-Mua
)
(defun c:QQWW6 ()
    (to_my_water_pline "Arcoma - thoat nuoc d60" 800)  ; Arcoma - thoat nuoc d60
)


;;************************LỆNH TẮT KHI DIM TỰ CHUYỂN LAYER

(defun my_dim (dim_command / *error* current_lay)
    ; Credit: https://stackoverflow.com/a/46776285
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


;;************************LỆNH TẮT QLEADER

(defun c:QE ( / *error* current_lay)
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
    ; Check if layer HTA - chữ exists and is on
    (if (and
            ; Check if layer HTA - chữ exists
            (tblsearch "layer" "HTA - ch\U+1EEF")  ; HTA - chữ
            ; Check if layer HTA - chữ is on
            (not (<
                (cdr (assoc 62 (entget (tblobjname "layer" "HTA - ch\U+1EEF"))))  ; HTA - chữ
                0
            ))
        )
        (setvar "CLAYER" "HTA - ch\U+1EEF")  ; HTA - chữ
    )
    (command "_.qleader" pause pause pause \e)  ; \e simulate escape key
    (setvar "CLAYER" current_lay)
)