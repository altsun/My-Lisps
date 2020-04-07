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

(defun c:00 ()
    (tolayer
        (ssget "_:L")  ; selection
        "0"  ; 0
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

(defun hatch_nhanh (htype hscale hangle / )
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" htype hscale hangle pause)
)
(defun c:H0 ()  ; Đất
    (hatch_nhanh "hound" "1500" "0")
)
(defun c:H1 () ; Solid, kiểu hatch đặc biệt không dùng được hàm hatch_nhanh (do không có hscale)
    (command "_.layer" "Set" "HTA - V\U+1EADt li\U+1EC7u 1" "")  ; HTA- Vật liệu 1
    (command "_.bhatch" "P" "solid" pause)
)
(defun c:H2 ()  ; Tường
    (hatch_nhanh "ansi31" "350" "0")
)
(defun c:H22 ()  ; Mái tôn, lan can
    (hatch_nhanh "ansi32" "1000" "135")
)
(defun c:H3 ()  ; Bê tông, đá
    (hatch_nhanh "ar-conc" "20" "0")
)
(defun c:H33 ()  ; Bê tông cốt thép (dùng custom hatch)
    (hatch_nhanh "1-btct" "20" "0")
)
(defun c:H333 ()  ; Đá ốp
    (hatch_nhanh "gravel" "300" "0")
)
(defun c:H4 ()  ; Gỗ
    (hatch_nhanh "ansi36" "200" "0")  ; Dùng ansi36 vì AutoCAD 2007 không có gost_wood
)
(defun c:H44 ()  ; Sàn gỗ
    (hatch_nhanh "dolmit" "350" "0")
)
(defun c:H5 ()  ; Kính
    (hatch_nhanh "ar-rroof" "200" "45")
)
(defun c:H6 ()  ; Gạch vuông (giá trị khởi tạo 300x300)
    (hatch_nhanh "net" "2400" "0")
)
(defun c:H66 ()  ; Gạch vuông sơ bộ
    (hatch_nhanh "angle" "1200" "0")
)
(defun c:H7 ()  ; Chấm
    (hatch_nhanh "ar-sand" "150" "0")
)
(defun c:H8 ()  ; Ngói
    (hatch_nhanh "ar-rshke" "20" "0")
)
(defun c:H9 ()  ; Gạch ốp (giá trị khởi tạo 220x110)
    (hatch_nhanh "brick" "440" "0")
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


;;************************LỆNH TẮT CHUYỂN TỶ LỆ DIM

(defun c:DS2 () (command "_.dim" "dimstyle" "restore" "HTA 1-20" "exit") )
(defun c:DS3 () (command "_.dim" "dimstyle" "restore" "HTA 1-30" "exit") )
(defun c:DS4 () (command "_.dim" "dimstyle" "restore" "HTA 1-40" "exit") )
(defun c:DS5 () (command "_.dim" "dimstyle" "restore" "HTA 1-50" "exit") )
(defun c:DS6 () (command "_.dim" "dimstyle" "restore" "HTA 1-60" "exit") )
(defun c:DS7 () (command "_.dim" "dimstyle" "restore" "HTA 1-70" "exit") )
(defun c:DS8 () (command "_.dim" "dimstyle" "restore" "HTA 1-80" "exit") )
(defun c:DS9 () (command "_.dim" "dimstyle" "restore" "HTA 1-90" "exit") )
(defun c:DS10 () (command "_.dim" "dimstyle" "restore" "HTA 1-100" "exit") )


;;************************LỆNH TẮT CHUYỂN TỶ LỆ DIM THEO KHUNG TÊN (TODO)
(defun c:CDS( / ss ent entdata scale)
    (setq ss (ssget))
    (setq ent (ssname ss 0))  ; Get first entity of selection set
    (setq entdata (entget ent))  ; Get entity's data
    (if (and (= (cdr(assoc 0 entdata)) "INSERT")  ; If entity is block
        (= (cdr(assoc 2 entdata)) "khung ten")  ; and it's name is "khung ten"
    )
        (progn
            (setq scale (cdr(assoc 41 entdata)))  ; Get block scale
            (cond
                ; To compare float, use equal function
                ((equal scale 0.2 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-20" "exit")
                )
                ((equal scale 0.3 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-30" "exit")
                )
                ((equal scale 0.4 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-40" "exit")
                )
                ((equal scale 0.5 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-50" "exit")
                )
                ((equal scale 0.6 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-60" "exit")
                )
                ((equal scale 0.7 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-70" "exit")
                )
                ((equal scale 0.8 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-80" "exit")
                )
                ((equal scale 0.9 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-90" "exit")
                )
                ((equal scale 1.0 1e-6)
                    (command "_.dim" "dimstyle" "restore" "HTA 1-100" "exit")
                )
            )
        )
    )
)


;;************************LỆNH TẮT XỬ LÝ GIAO GIỮA DÂY ĐIỆN VÀ THIẾT BỊ ĐIỆN (TODO)

;; Intersections  -  Lee Mac
;; Returns a list of all points of intersection between two objects
;; for the given intersection mode.
;; ob1,ob2 - [vla] VLA-Objects
;;     mod - [int] acextendoption enum of intersectwith method

(defun LM:intersections ( ob1 ob2 mod / lst rtn )
    (if (and (vlax-method-applicable-p ob1 'intersectwith)
             (vlax-method-applicable-p ob2 'intersectwith)
             (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
        )
        (repeat (/ (length lst) 3)
            (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
                  lst (cdddr lst)
            )
        )
    )
    (reverse rtn)
)

;;; =====================================================
;;;    c:pl_perp2segment.lsp  By CAB 07/21/04            
;;;
;;;    Draw a line perpendicular to a polyline segment
;;;    picked by the user from a point picked
;;;    Will draw to extension of segment if point is not
;;;    perpendicular to segment
;;;
;;;    Uses current layer
;;;
;;;    Also works with Arc, Circle & Line but the segment
;;;    created is to the chord in Arcs & Circles
;;; =====================================================
(defun c:pl_perp2segment ()
  (if (and (setq ent (entsel "Pick a polyline: "))
           (member (cdr (assoc 0 (entget (car ent))))
                   '("LWPOLYLINE" "ARC" "CIRCLE" "LINE"))
           )
    (if (setq pt (getpoint "\nSelect point."))
      (progn
         (setq plst (getsegment
                      (vlax-ename->vla-object (car ent))
                      (cadr ent)
                    )
         )
         (setq ppl (ge_perppt pt (cadr plst) (caddr plst)))
         (command "._pline" "non" ppl "non" pt "")
         (command "._circle" ppl 100)  ; Create a circle with radius 100
         (command "._pline" "non" p1 "non" pt "non" p2 "")
        )
      (prompt "\n *-* User quit *-*")
      );endif
      (prompt "\n *-* Invalid object selected *-*")
  ); endif
  (princ)
); defun

(defun getsegment (obj pt / cpt eparam stparam)
  (cond
    ((setq cpt (vlax-curve-getclosestpointto obj pt t))
     (setq eparam (fix (vlax-curve-getendparam obj)))
     (if (= eparam (setq stparam (fix (vlax-curve-getparamatpoint obj cpt))))
       (setq stparam (1- stparam))
       (setq eparam (1+ stparam))
     )
     (list eparam
           (vlax-curve-getpointatparam obj stparam)
           (vlax-curve-getpointatparam obj eparam)
     )
    )
  )
)

;; ! ****************************************************************************
;; ! GE_PerpPt
;; ! ****************************************************************************
;; ! Function : Locate the perpendicular drop point from a point pp to any line
;; !            segment defined by two other points pt1 and pt2
;; ! Argument : [pp] - Point to drop from
;; !            [p1] - First Point of line segment
;; !            [p2] - Second Point of line segment
;; ! Return   : The drop point on the line segment
;; ! (C) 1999-2004, Four Dimension Technologies, Bangalore
;; ! e-mail   : rakesh.rao@4d-technologies.com
;; ! Web      : www.4d-technologies.com
;; ! ****************************************************************************
(defun ge_perppt (pp p1 p2 / x y x1 x2 x3 x4 y1 y2 y3 y4 m p4)
  (setq
    x  (car pp)
    y  (cadr pp)
    x1 (car p1)
    y1 (cadr p1)
    x2 (car p2)
    y2 (cadr p2)
  )

  (cond
    ((equal x1 x2 0.0000001) ; Vertical line
     (setq
       x4 x1
       y4 y
       p4 (list x4 y4 0.0)
     )
    )
    ((equal y1 y2 0.0000001) ; Horizontal line
     (setq
       x4 x
       y4 y1
       p4 (list x4 y4 0.0)
     )
    )
    (t
     (setq
       m  (/ (- y2 y1) (- x2 x1)) ; gradient p1- p2
       x4 (/ (+ (/ x m) (+ y (- (* m x1) y1))) (+ (/ 1 m) m))
       y4 (+ y1 (* m (- x4 x1)))
       p4 (list x4 y4 0.0)
     )
    )
  )
  p4
)

(defun c:1E()
  (c:pl_perp2segment)
)