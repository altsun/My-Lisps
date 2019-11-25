;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=22849
(defun C:TC (/ Txt PTxt PTX SS i prmt DK TEMP_JTF DCL_ID DCL_JTF)
  (setq SS (ssget "I" '((0 . "TEXT"))) i 0)
	(command "undo" "begin")
	(setvar "cmdecho" 0)
	(if (not JTF-T) (setq JTF-T (list 0 1 0)))
	(if (= (cadr JTF-T) 1)
		(setq prmt "Text to Center")
		(if (= (caddr JTF-T) 1) (setq prmt "Text to Right") (setq prmt "Text to Left"))
	);if
	(vl-load-com)
  (if (not SS)
    (progn
      (prompt (strcat "\nSelect text object [Setting - " prmt " ]"))
		(setq DK (grread nil 4 2))
		(if (= (car DK) 3)
		(setq SS (ssget "C" (cadr DK) (getcorner (cadr DK)) '((0 . "TEXT"))))
		(if (= (cadr DK) 115)
		(progn
			(setq	DCL_JTF (list "JTFtext : dialog {label = \"Justify in Region\"; : boxed_radio_row {"
	                              "        : radio_button { label = \"Left\"; key = \"Lft\";}"
			  "        : spacer { width = 1.2; }"
	                              "        : radio_button { label = \"Center\"; key = \"Ctr\";}"
	                              "        : radio_button { label = \"Right\"; key = \"Rgt\";}}"
	                              "        ok_cancel;}"))
			(setq	TEMP_JTF (vl-filename-mktemp "CTK.DCL")
				FILE_DCL (open TEMP_JTF "W"))
			 (foreach LL DCL_JTF (write-line LL FILE_DCL))
			(close FILE_DCL)
			(setq DCL_ID (load_dialog TEMP_JTF))
			(new_dialog "JTFtext" DCL_ID)
			(set_tile "Lft" (rtos (nth 0 JTF-T) 2 0))
			(set_tile "Ctr" (rtos (nth 1 JTF-T) 2 0))
			(set_tile "Rgt" (rtos (nth 2 JTF-T) 2 0))
(action_tile "accept" "(setq JTF-T (list(atof(get_tile \"Lft\"))(atof (get_tile \"Ctr\"))(atof (get_tile \"Rgt\"))))(done_dialog)")
(start_dialog)
(unload_dialog DCL_ID)
(vl-file-delete TEMP_JTF)
(setq SS (ssget '((0 . "TEXT"))))
);progn
(if (= (cadr DK) 32) (exit)	(progn (prompt "\nWrong Key (!) Select text oject or press [S]etting") (C:TC)))
);if
);if
);progn
);if
(if SS
(progn
   (command "UCS" "W")
   (setq    OSMLAST    (getvar "osmode"))
   (setvar "OSMODE" 0)
   (cond ((= (nth 1 JTF-T) 1)
   (repeat (sslength SS)
   (setq txt (ssname SS i) PTxt (GET_MIDTEXT txt) PTX (GET_CENTER_REGION PTxt) i (1+ i))
   (if PTX (vl-cmdf "move" txt "" PTxt PTX))
);repeat
);list_Ctr
((= (nth 0 JTF-T) 0)
(repeat (sslength SS)
(setq txt (ssname SS i) PTxt (GET_RIGHTTEXT txt) PTX (cadr (GET_LR_REGION PTxt)) i (1+ i))
(if PTX (vl-cmdf "move" txt "" PTxt PTX))
);repeat
);list_rgt
((= (nth 2 JTF-T) 0)
(repeat (sslength SS)
(setq txt (ssname SS i) PTxt (GET_LEFTTEXT txt) PTX (car (GET_LR_REGION PTxt)) i (1+ i))
(if PTX (vl-cmdf "move" txt "" PTxt PTX))
);repeat
);list_lft
);cond
(setvar "osmode" OSMLAST)
(command "UCS" "P")
);progn
);if
(prompt "Thaistreetz@gmail.com")
(command "undo" "end")
(princ)
);end TC
(defun GET_CENTER_REGION (PT / SSL PTC )
    (setq SSL (entlast))
    (if (= (DXF 0 SSL) "POLYLINE")
        (while    (/= "SEQEND" (DXF 0 (entnext SSL)))
            (setq SSL (entnext SSL))
        );while
    );if
    (vl-cmdf "-boundary" PT "")
    (if (entnext SSL)
        (progn
            (command "region" "L" "")
            (setq PTC (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object (entlast)) 'Centroid))))
            (command "erase" (entlast) "")
            PTC
        );progn
        nil
    );if    
);END
(defun GET_LR_REGION (PT / SSL PTC )
    (setq SSL (entlast))
    (if (= (DXF 0 SSL) "POLYLINE")
        (while    (/= "SEQEND" (DXF 0 (entnext SSL)))
            (setq SSL (entnext SSL))
        );while
    );if
    (vl-cmdf "-boundary" PT "")
    (if (entnext SSL)
        (progn
          (command "region" "L" "")
	        (setq PTC (ACET-GEOM-SS-EXTENTS-FAST (ssget "L")))
          (command "erase" (entlast) "")
          (list (list (car (car PTC)) (+ (cadr (car PTC)) (* 0.5 (abs (- (cadr (car PTC)) (cadr (cadr PTC)))))))
	(list (car (cadr PTC)) (- (cadr (cadr PTC)) (* 0.5 (abs (- (cadr (car PTC)) (cadr (cadr PTC))))))))
        );progn
        nil
    );if    
);END
(defun GET_MIDTEXT (EN / TB PTxt PT0 PTA)
  (setq TB (textbox (entget EN))
        PTxt (GET_M2P (car TB) (cadr TB))
        PT0 (DXF 10 EN)
        PTA (list (+ (car PT0) (car PTxt)) (+ (cadr PT0) (cadr PTxt))))
  (polar PT0 (+ (DXF 50 EN) (angle PT0 PTA)) (distance PT0 PTA))
);end
(defun GET_RIGHTTEXT (EN / TB PTxt PT0 PTA)
  (setq TB (textbox (entget EN))
        PTxt (GET_M2P (car TB) (cadr TB))
        PT0 (DXF 10 EN)
        PTA (list (+ (car PT0) (car PTxt)) (+ (cadr PT0) (cadr PTxt))))
(list(+(car PT0)(car (cadr TB))(abs(-(cadr(car TB))(cadr (cadr TB))))) (cadr(polar PT0 (+(DXF 50 EN)(angle PT0 PTA))(distance PT0 PTA))))
)
(defun GET_LEFTTEXT (EN / TB PTxt PT0 PTA)
  (setq TB (textbox (entget EN))
        PTxt (GET_M2P (car TB) (cadr TB))
        PT0 (DXF 10 EN)
        PTA (list (+ (car PT0) (car PTxt)) (+ (cadr PT0) (cadr PTxt))))
  (list (- (car PT0) (abs (- (cadr (car TB)) (cadr (cadr TB))))) (cadr (polar PT0 (+ (DXF 50 EN) (angle PT0 PTA)) (distance PT0 PTA))))
)
(defun DXF (Id Obj)
    (cdr (assoc Id (entget Obj)))
) 
(defun GET_M2P (PT1 PT2) (polar PT1 (angle PT1 PT2) (* 0.5 (distance PT1 PT2))));end

