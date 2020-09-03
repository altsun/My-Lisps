;;By Andrea Andreetti	2008-11-24			;;
;;							;;
;;		D U C T C O N N E C T 			;;
;;			  				;;
  							;;

(defun c:DuctConnect (/ dr_sel1 dr_sel1data p1 SDrain_10 SDrain_11 DuctStyle1)

  
;;	Degre Conversion	;;
  				;;
(defun dtr (a)
  (* pi (/ a 180.0))
)

(defun rtd (a)
(/ (* a 180) pi)
)  
  				;;
;;	Degre Conversion	;;


  

;;	Language Detection  				;;
  							;;
(if (vl-string-search "(FR)" (strcase (ver)))
  (progn
    (setq qstion0 "\nCommande: DuctConnect	-Activé")
    (setq qstion1 "Type de Conduit (C)arré/(R)ond <")
    (setq qstion2 "Largeur du conduit: ")
    (setq qstion3 "Sélection de la ligne du conduit: ")
   )
  (progn
    (setq qstion0 "\nCommand: DuctConnect	-Activated")
    (setq qstion1 "Duct Type (S)quare/(R)ound <")
    (setq qstion2 "Duct Width: ")
    (setq qstion3 "Select Duct Line: ")
   )
)
  							;;
;;	Language Detection  				;;






  
;;	DuctStyle Detection  				;;
  							;;
(if (not DuctStyle) (Setq DuctStyle "C"))
(initget "S R C")
(setq DuctStyle1 (getkword (strcat qstion1 DuctStyle ">: ")))
(if DuctStyle1 (Setq DuctStyle DuctStyle1))
  							;;
;;	DuctStyle Detection  				;;





  

;;	Selection and Data operation			;;
  							;;
(setq LarDC (getreal qstion2))
(setq LarDC2 (/ LarDC 2))  
(setq dr_sel1 nil)
(while (or (= dr_sel1 nil)
           (/= (cdr (assoc 0 (setq dr_sel1data (entget (car dr_sel1))))) "LINE"))
	(progn
		(princ qstion3)
		(setq dr_sel1 (entsel))
	)
)
(setq p1 (osnap (cadr dr_sel1) "_near"))
(setq SDrain_10  (cdr (assoc 10 dr_sel1data)))	;Start Point
(setq SDrain_11  (cdr (assoc 11 dr_sel1data)))	;End Point
(setq SDrain_8   (cdr (assoc 8 dr_sel1data)))	;Layer
(setq SDrain_62  (cdr (assoc 62 dr_sel1data)))	;Color
(setq SDrain_6  (cdr (assoc 6 dr_sel1data)))	;Linetype
  
  							;;
;;	Selection and Data operation			;;





  

;;	Units detection for ZY and ZY2 variables	;;
  							;;
(if (< (getvar "LUNITS") 3)
  (setq zy 100)
  (setq zy 4)
)
(setq zy2 (/ zy 2))
  							;;
;;	Units detection for ZY and ZY2 variables	;;



  
;;	PREVIEW MODE					;;
  							;;
(simulductattach)
  							;;
;;	PREVIEW MODE					;;

)
  							;;
;;							;;
;;		D U C T C O N N E C T 			;;
;;			  				;;









;;							;;
;;	   S I M U L D U C T A T T A C H		;;
;;			  				;;
							;;
(defun simulductattach (/ #elp agp1 pstart1 pstart2 pointtomod ndrainent)

  (while (= (car (setq grr (grread t 2))) 5)
    (redraw)
  (grdraw p1 (polar p1 (dtr 0) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 45) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 90) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 135) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 180) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 225) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 270) zy2) 52 1)
  (grdraw p1 (polar p1 (dtr 315) zy2) 52 1)
    (setq #elp (car (cdr grr)))
    (setq agp1 (inters #elp
                       (polar #elp (+ (angle sdrain_11 sdrain_10) (dtr 90)) 2.0)
                       sdrain_10
                       sdrain_11
                       nil
               )
    )
    (if (not agp1)
      (setq agp1 (inters #elp
                         (polar #elp (+ (angle sdrain_10 sdrain_11) (dtr 90)) 2.0)
                         sdrain_10
                         sdrain_11
                         nil
                 )
      )
    )
(setq Djp1 (polar agp1 (angle agp1 p1) (+ LarDC2 zy)))
(setq Djpx (polar Djp1 (angle Djp1 agp1) zy))
(setq DjpA (polar Djpx (angle agp1 #elp) zy))
(setq DLp1 (polar Djpx (angle agp1 #elp) (distance agp1 #elp)))

(setq Djp2 (polar agp1 (angle p1 agp1) LarDC2))
(setq DLp2 (polar Djp2 (angle agp1 #elp) (distance agp1 #elp)))
(setq Djpb (polar Djp2 (angle agp1 #elp) zy))
    
(grdraw djp1 djpa 141 1)
(grdraw djpa dlp1 141 1)
(grdraw djpa djpb 141 1)
(grdraw djp2 dlp2 141 1)

(if (eq DuctStyle "R")
  (progn
(setq djpm  (polar djpa (angle djpa djpb) (/ (distance djpa djpb) 2)))
(setq djrja (polar djpa (angle agp1 #elp) (* zy 0.75)))
(setq djrjb (polar djpb (angle agp1 #elp) (* zy 0.75)))
(grdraw djp1 djpm 1 1)
(grdraw djp2 djpm 1 1)
(grdraw djrja djrjb 33 1)
)
)
)

(if (eq (car grr) 3)
    (progn
(if (eq DuctStyle "R")
(if (and djp1 djpa dlp1 djp2 dlp2 djpa djpb djrja djrjb)
(progn
(Duct_entmake  djp1 djpa SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djpa dlp1 SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djp2 dlp2 SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djpa djpb SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djp1 djpm "continuous" 1 SDrain_8)
(Duct_entmake  djp2 djpm "continuous" 1 SDrain_8)

;;Assume that ACAD.LIN exist and contain the ACAD_ISO03W100 Linetype.
(if (not (member "ACAD_ISO03W100" (mapcar 'strcase (ai_table "LTYPE" 0))))
  (vl-cmdf "._linetype" "_L" "ACAD_ISO03W100" "acad.lin"))
(Duct_entmake  djrja djrjb "ACAD_ISO03W100" 33 SDrain_8)
;;------------------------------------------------------------------;;

(redraw)
)
)
  
(if (and djp1 djpa dlp1 djp2 dlp2 djpa djpb)
(progn
(Duct_entmake  djp1 djpa SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djpa dlp1 SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djp2 dlp2 SDrain_6 SDrain_62 SDrain_8)
(Duct_entmake  djpa djpb SDrain_6 SDrain_62 SDrain_8)     
(redraw)
)
)
)  

(redraw)
(simulductattach)
    )
    (progn (redraw) (exit)(princ))
  )
)
							;;
;;							;;
;;	   S I M U L D U C T A T T A C H		;;
;;			  				;;






;;							;;
;;		D U C T _ E N T M A K E			;;
;;			  				;;
							;;
(defun Duct_entmake (#10p #11p ForceLinetype ForceColor ForceLayer / ety_data)
(setq ety_data
       (list
	  '(0 . "LINE")		; Object type
          (cons 10 #10p)	; Start Point
          (cons 11 #11p)	; End Point
          (cons 8 SDrain_8)	; Layer
)
)  
  
(if ForceLinetype
  (setq ety_data (append  ety_data (list (cons 6 ForceLinetype))))
)   

(if ForceColor
 (setq ety_data (append  ety_data (list (cons 62 ForceColor)))) 
(if (and DuctColor (/= DuctColor 256)) 
  (setq ety_data (append  ety_data (list (cons 62 DuctColor))))
)
)
(entmake ety_data)
)
							;;
;;							;;
;;		D U C T _ E N T M A K E			;;
;;			  				;;





  

;;======================================;;
;;		AUTO-LOAD		;;
;;======================================;;
(princ
qstion0)


















;;By Andrea Andreetti	2008-11-27			;;
;;							;;
;;		D U C T A T T A C H 			;;
;;			  				;;
  							;;

(defun c:DuctAttach (/ Ent_10 Ent_11 Ent_62 Ent_0 Inter_L1 Inter_L2 entData Basepoint1 Basepoint2
                 Dduct grjp1 grjp2 ficp1 ficp2 ficpX #DCswitch Dway1 Dway2 Dway3 Dway4 ArcEnt
                 dr_sel1 dr_sel2 SLi_0 VLA_line1 VLA_line1_start VLA_line1_end ArcEntLIST
                 VLA_line2 VLA_line2_start VLA_line2_end 4po GEN_clayercolor Input entData arcX arcY
                     newLine 1_sp 1_sp 3_sp 4_sp)

(setq ArcEnt nil)
  
;;	Degre Conversion	;;
  				;;
(defun dtr (a)
  (* pi (/ a 180.0))
)

(defun rtd (a)
(/ (* a 180) pi)
)  
  				;;
;;	Degre Conversion	;;

  
(DCclean)
(if (not #DCswitch)(setq #DCswitch 0))  
(setq dr_sel1 nil
      dr_sel2 nil)
  
(while (or (= dr_sel1 nil)
           (/= (cdr (assoc 0 (setq dr_sel1data (entget (car dr_sel1))))) "LINE")
       )
(setq dr_sel1 (entsel "\nSlection des lignes du conduit..."))
)

(while (or (= dr_sel2 nil)
           (/= (cdr (assoc 0 (setq dr_sel2data (entget (car dr_sel2))))) "LINE")
       )
(setq dr_sel2 (entsel "\nSlection des lignes du conduit..."))
) 

(if (and dr_sel1 dr_sel2)
  (progn
(setq SLi_0   (cdr (assoc 8 (entget (car dr_sel1)))))
(setq VLA_line1 (vlax-ename->vla-object (car dr_sel1)))
(setq VLA_line1_start (vlax-get VLA_line1 'startpoint))
(setq VLA_line1_end   (vlax-get VLA_line1 'endpoint))

(setq VLA_line2 (vlax-ename->vla-object (car dr_sel2)))
(setq VLA_line2_start (vlax-get VLA_line2 'startpoint))
(setq VLA_line2_end   (vlax-get VLA_line2 'endpoint))      

(if (< (getvar "LUNITS") 3)
  (setq 4po 100)
  (setq 4po 4)
)
(setq GEN_clayercolor (cdr (assoc 62 (entget (tblobjname "layer" (getvar "clayer"))))))
    
(defun *error* (msg)
  (DCclean)
(if VLA_line0 (progn (vlax-release-object VLA_line0)(setq VLA_line0 nil)))   
(if VLA_line1 (progn (vlax-release-object VLA_line1)(setq VLA_line1 nil)))
(if Nline (progn (vlax-release-object Nline)(setq Nline nil)))
(if Sline (progn (vlax-release-object Sline)(setq Sline nil)))
  
  (princ msg)
  )
  
  (while (and (setq Input (grread T 4 4))
              (or (= (car Input) 5)
                  (and
                  (= (car Input) 2)  
                  (= (cadr Input) 9)
                  )
                  )
          )
    
;;SWITCH MODE				;;
    					;;
(if (and (= (car Input) 2)
         (= (cadr Input) 9)
    )
  (progn
(setq Input (grread T 4 4))
(if (> #DCswitch 5)
    (setq #DCswitch 0)
    (setq #DCswitch (1+ #DCswitch))
)

(progn
  (if (eq #DCswitch 0) (princ "\n- Switched to Square/Square excentric connection -"))
  (if (eq #DCswitch 1) (princ "\n- Switched to Square/Square Straight connection -"))
  (if (eq #DCswitch 2) (princ "\n- Switched to Square/Square Concentric connection -"))
  (if (eq #DCswitch 3) (princ "\n- Switched to Round/Square Concentric connection -"))
  (if (eq #DCswitch 4) (princ "\n- Switched to Round/Round Concentric connection -"))
  (if (eq #DCswitch 4) (princ "\n- Switched to Round/Round Straight connection -"))

)
)
)
(DCswitch)
   					;;
;;SWITCH MODE				;;


)
)
)
(DCclean)

(if (eq (car Input) 3)
  (Ductattach_exe)
)
  
)
  							;;
;;							;;
;;		D U C T A T T A C H 			;;
;;			  				;;

    



;;							;;
;;		D C S W I T C H				;;
;;			  				;;
							;;
(defun DCswitch (/ )
 

     (if (and (setq EntName    (car (nentselp (setq cursorLocation (cadr Input)))))
	 (not (eq Iname EntName))
         )
       (progn
        (DCclean)
(setq entData (entget EntName))
(setq Ent_0   (cdr (assoc 0 entData)))
(if (setq Ent_62 (assoc 62 entData))
(setq Ent_62   (cdr Ent_62))
(setq Ent_62 GEN_clayercolor)
)
(setq Ent_10   (cdr (assoc 10 entData)))        
(setq Ent_11   (cdr (assoc 11 entData)))
        
(if (eq Ent_0 "LINE")
  (progn
(setq VLA_line0 (vlax-ename->vla-object (cdr (car entData))))

(setq Inter_L1 (vlax-invoke VLA_line0 'intersectwith VLA_line1 acExtendBoth))
(setq Inter_L2 (vlax-invoke VLA_line0 'intersectwith VLA_line2 acExtendBoth))
(if (and Inter_L1 Inter_L2)
  (progn
    (if (<
          (distance Inter_L1 VLA_line1_start)
          (distance Inter_L1 VLA_line1_end)
         )
      (setq Basepoint1 VLA_line1_end)
      (setq Basepoint1 VLA_line1_start)
    )
    (if (<
          (distance Inter_L2 VLA_line2_start)
          (distance Inter_L2 VLA_line2_end)
         )
      (setq Basepoint2 VLA_line2_end)
      (setq Basepoint2 VLA_line2_start)
    )

(setq Dduct (+ (distance Basepoint1 Basepoint2) 4po))
    
  (setq grjp1 (polar Inter_L1 (angle Inter_L1 Basepoint1) 4po))
  (setq grjp2 (polar Inter_L2 (angle Inter_L2 Basepoint2) 4po))

  (if (/= (distance grjp1 Inter_L2)(distance grjp2 Inter_L1))
    (if (>  (distance grjp1 Inter_L2)(distance grjp2 Inter_L1))
   (progn

(setq ficp1 (polar grjp1 (+ (angle inter_L1 grjp1) (dtr 90)) 4po))
(setq ficp2 (polar grjp1 (- (angle inter_L1 grjp1) (dtr 90)) 4po))
(if (< (distance ficp1 grjp2)(distance ficp2 grjp2))
  (setq ficpx ficp1)
  (setq ficpx ficp2)
)
(setq grjp2 (inters grjp1 ficpx inter_L2 basepoint2 nil))     
     
)

(progn
(setq ficp1 (polar grjp2 (+ (angle inter_L2 grjp2) (dtr 90)) 4po))
(setq ficp2 (polar grjp2 (- (angle inter_L2 grjp2) (dtr 90)) 4po))
(if (< (distance ficp1 grjp1)(distance ficp2 grjp1))
  (setq ficpx ficp1)
  (setq ficpx ficp2)
)
(setq grjp1 (inters grjp2 ficpx inter_L1 basepoint1 nil))     
     
)    
)
)

;;joint Line
  (grdraw Basepoint1 grjp1 Ent_62 1)
  (grdraw Basepoint2 grjp2 Ent_62 1)
  (grdraw grjp1 grjp2 Ent_62 1)
    
    
;;Square/Square excentric connection			;;
    							;;
(if (= #DCswitch 0)
  (progn
  (if (and (/= #DCswitchOLD #DCswitch)
           ArcEnt)
    (progn (entdel ArcEnt)(setq ArcEnt nil))
  )
(if (<
      (distance cursorLocation Inter_L1)
      (distance cursorLocation Inter_L2)
    )
  (progn  
  (setq Dway1 grjp1)
  (setq Dway2 (polar Inter_L1 (angle Inter_L2 Inter_L1) 4po))
  (setq Dway3 grjp2
        Dway4 Inter_L2)
  )
  (progn
  (setq Dway1 grjp2)  
  (setq Dway2 (polar Inter_L2 (angle Inter_L1 Inter_L2) 4po))
  (setq Dway3 grjp1
        Dway4 Inter_L1)
  )
)
  (setq #DCswitchOLD #DCswitch)
)
)
    							;;
;;Square/Square excentric connection			;;


    


;;Switched to Square/Square Straight connection		;;
    							;;
(if (= #DCswitch 1)
  (progn
    (if (and (/= #DCswitchOLD #DCswitch)
           ArcEnt)
    (progn (entdel ArcEnt)(setq ArcEnt nil))
  )  
  (setq Dway1 grjp1
  	Dway2 Inter_L1
  	Dway3 grjp2
        Dway4 Inter_L2
)
    (setq #DCswitchOLD #DCswitch)
)
)
    							;;
;;Switched to Square/Square Straight connection		;;


    


;;Switched to Square/Square Concentric connection	;;
    							;;
(if (= #DCswitch 2)
  (progn
  (if (and (/= #DCswitchOLD #DCswitch)
           ArcEnt)
    (progn (entdel ArcEnt)(setq ArcEnt nil))
  )    
  (setq Dway1 grjp1
  	Dway2 (polar Inter_L1 (angle Inter_L2 Inter_L1) (/ 4po 2))
  	Dway3 grjp2
        Dway4 (polar Inter_L2 (angle Inter_L1 Inter_L2) (/ 4po 2))
)
  (setq #DCswitchOLD #DCswitch)
)    
)
    							;;
;;Switched to Square/Square Concentric connection	;;


    
    

;;Switched to Round/Square Concentric connection	;;
    							;;
(if (= #DCswitch 3)
  (progn
  (if (and (/= #DCswitchOLD #DCswitch)
           ArcEnt)
    (progn (entdel ArcEnt)(setq ArcEnt nil))
  )    
  (setq Dway1 grjp1
  	Dway2 (polar Inter_L1 (angle Inter_L2 Inter_L1) (/ 4po 2))
  	Dway3 grjp2
        Dway4 (polar Inter_L2 (angle Inter_L1 Inter_L2) (/ 4po 2))
  )
  (setq Rj1 (polar Dway1 (angle Inter_L1 Basepoint1) (* 4po 0.75)))
  (setq Rj2 (polar Dway3 (angle Inter_L2 Basepoint2) (* 4po 0.75)))
  (grdraw Rj1 Rj2 33 1)
  (setq #DCswitchOLD #DCswitch)
)
)
    							;;
;;Switched to Round/Square Concentric connection	;;




    

;;Switched to Round/Round Concentric connection		;;
    							;;
(if (= #DCswitch 4)
  (progn
  (if (and (/= #DCswitchOLD #DCswitch)
           ArcEnt)
    (progn (entdel ArcEnt)(setq ArcEnt nil))
  )    
  (setq Dway1 grjp1
  	Dway2 (polar Inter_L1 (angle Inter_L2 Inter_L1) (/ 4po 2))
  	Dway3 grjp2
        Dway4 (polar Inter_L2 (angle Inter_L1 Inter_L2) (/ 4po 2))
  )
  (setq Rj1 (polar Dway1 (angle Inter_L1 Basepoint1) (* 4po 0.75)))
  (setq Rj2 (polar Dway3 (angle Inter_L2 Basepoint2) (* 4po 0.75)))
  (grdraw Rj1 Rj2 33 1)

;;Draw ARC
(setq midjoint (polar Dway2 (angle Dway2 Dway4) (/ (distance Dway2 Dway4) 2)))  
(setq cenOFarc (polar midjoint (+ (angle Dway2 Dway4) (dtr 90)) 4po))
(if (> (distance cenOFarc Dway1)(distance midjoint Dway1))
  (setq cenOFarc (polar midjoint (- (angle Dway2 Dway4) (dtr 90)) 4po))
  )

(setq arcX (polar midjoint (- (angle cenOFarc midjoint) (dtr 90)) (distance midjoint Dway2)))
(setq arcY (polar midjoint (+ (angle cenOFarc midjoint) (dtr 90)) (distance midjoint Dway2)))  
(setq ArcEntLIST
	    (list
	      '(0 . "ARC");;Type
	      (cons 8 SLi_0);;Layer
	      '(100 . "AcDbCircle");;Catégorie
	      (cons 10 cenOFarc);;Center Point
	      (cons 40 (distance cenOFarc Dway2));;Radius
	      '(100 . "AcDbArc")
	      (cons 50 (angle cenOFarc arcX));;Dway2))
	      (cons 51 (angle cenOFarc arcY));;Dway4))
              (cons 62 Ent_62)
	    )
)

(setq ArcEnt (entmakex ArcEntLIST))      
(setq #DCswitchOLD #DCswitch)            
)  
)
    							;;
;;Switched to Round/Round Concentric connection		;;





;;Switched to Round/Round Straight connection		;;
    							;;
(if (= #DCswitch 5)
  (progn
  (if (and (/= #DCswitchOLD #DCswitch)
           ArcEnt)
    (progn (entdel ArcEnt)(setq ArcEnt nil))
  )    
  (setq Dway1 grjp1
  	Dway2 Inter_L1
  	Dway3 grjp2
        Dway4 Inter_L2
  )
  (setq Rj1 (polar Dway1 (angle Inter_L1 Basepoint1) (* 4po 0.75)))
  (setq Rj2 (polar Dway3 (angle Inter_L2 Basepoint2) (* 4po 0.75)))
  (grdraw Rj1 Rj2 33 1)

;;Draw ARC
(setq midjoint (polar Dway2 (angle Dway2 Dway4) (/ (distance Dway2 Dway4) 2)))  
(setq cenOFarc (polar midjoint (+ (angle Dway2 Dway4) (dtr 90)) 4po))
(if (> (distance cenOFarc Dway1)(distance midjoint Dway1))
(setq cenOFarc (polar midjoint (- (angle Dway2 Dway4) (dtr 90)) 4po))
)
 
(setq arcX (polar midjoint (- (angle cenOFarc midjoint) (dtr 90)) (distance midjoint Dway2)))
(setq arcY (polar midjoint (+ (angle cenOFarc midjoint) (dtr 90)) (distance midjoint Dway2)))
  
(setq ArcEntLIST
	    (list
	      '(0 . "ARC");;Type
	      (cons 8 SLi_0);;Layer
	      '(100 . "AcDbCircle");;Catégorie
	      (cons 10 cenOFarc);;Center Point
	      (cons 40 (distance cenOFarc Dway2));;Radius
	      '(100 . "AcDbArc")
	      (cons 50 (angle cenOFarc arcX))
	      (cons 51 (angle cenOFarc arcY))
              (cons 62 Ent_62)
	    )
 )
(setq ArcEnt (entmakex ArcEntLIST))
  (setq #DCswitchOLD #DCswitch)
)
)
    							;;
;;Switched to Round/Round Straight connection		;;
    
    
;;GRDRAW Connection
(grdraw Dway1 Dway2 Ent_62 1)
(grdraw Dway3 Dway4 Ent_62 1) 

  )
 )
)
)
(if (= (car Input) 11)(DCclean))
)
)
)


							;;
;;							;;
;;		D C S W I T C H				;;
;;			  				;;








;;							;;
;;		DUCTATTACH_EXE				;;
;;			  				;;
							;;

(defun Ductattach_exe ()

(if (and Dway1 Dway2 Dway3 Dway4)
  (progn

  (setq	actdoc	  (vla-get-activedocument (vlax-get-acad-object)))
  (setq	space	  (if (= (getvar "cvport") 1)
		    (vla-get-paperspace actdoc)
		    (vla-get-modelspace actdoc)
		  )
  )

    
(vla-put-StartPoint VLA_line1 (vlax-3d-point basepoint1))
(vla-put-EndPoint VLA_line1 (vlax-3d-point grjp1))

(vla-put-StartPoint VLA_line2 (vlax-3d-point basepoint2))
(vla-put-EndPoint VLA_line2 (vlax-3d-point grjp2))


(setq newLine (vlax-invoke space 'addline Dway1 Dway3))
(NLINE_chprop newline VLA_line1)

(setq newLine (vlax-invoke space 'addline Dway3 Dway4))
(NLINE_chprop newline VLA_line1)

(setq newLine (vlax-invoke space 'addline Dway1 Dway2))
(NLINE_chprop newline VLA_line1)  


;; Round Duct Joint connection
(if (> #DCswitch 2)
  (progn
  (setq newLine (vlax-invoke space 'addline Rj1 Rj2))
;;Layer
   (setq N_layer (vla-get-layer VLA_line1))
   (vla-put-layer newLine N_layer)
;;Color
   (vla-put-color newLine "33")

;;Linetype
;;Assume that ACAD.LIN exist and contain the ACAD_ISO03W100 Linetype.
(if (not (member "ACAD_ISO03W100" (mapcar 'strcase (ai_table "LTYPE" 0))))
  (vl-cmdf "._linetype" "_L" "ACAD_ISO03W100" "acad.lin" ""))
(vla-put-linetype newline "ACAD_ISO03W100")
  
  );_progn
);_if



;; Round Duct to Round Duct ARC design
(if (> #DCswitch 3)
  (progn
(if ArcEntLIST (entmakex ArcEntLIST))
(setq newarc (vlax-ename->vla-object (entlast)))
(NLINE_chprop newarc VLA_line1)

(setq 1_sp (vlax-get VLA_line0 'startpoint))
(setq 4_sp (vlax-get VLA_line0 'endpoint))

(if (> (distance 1_sp Dway2)(distance 1_sp Dway4))
  (progn
  (setq 2_sp Dway4)
  (setq 3_sp Dway2)
  )
  (progn
  (setq 3_sp Dway4)  
  (setq 2_sp Dway2)
  )
)
(vla-put-StartPoint VLA_line0 (vlax-3d-point 1_sp))
(vla-put-Endpoint VLA_line0 (vlax-3d-point 2_sp))
(setq newLine (vlax-invoke space 'addline 3_sp 4_sp))
(NLINE_chprop newline VLA_line0)
)
)
);_progn
)
)
							;;
;;							;;
;;		DUCTATTACH_EXE				;;
;;			  				;;





;;							;;
;;		NLINE_chprop				;;
;;			  				;;
							;;
(defun NLINE_chprop (Nline Sline)

;;Linetype
   (setq N_linetype (vla-get-linetype Sline))
   (vla-put-linetype NLine N_linetype)
;;Layer
   (setq N_layer (vla-get-layer Sline))
   (vla-put-layer NLine N_layer)
;;Color
   (setq N_color (vla-get-Color Sline))
   (vla-put-color NLine N_color)
;;Thickness
   (setq N_thickness (vla-get-Thickness Sline))
   (vla-put-Thickness NLine N_thickness)   
)
							;;
;;							;;
;;		NLINE_chprop				;;
;;			  				;;







;;							;;
;;			DCclean				;;
;;			  				;;
							;;
(defun DCclean ()
(if ArcEnt (progn (entdel ArcEnt)(setq ArcEnt nil)))
  (redraw)
)
							;;
;;							;;
;;			DCclean				;;
;;			  				;;

  



