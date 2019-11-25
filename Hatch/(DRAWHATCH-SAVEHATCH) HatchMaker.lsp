;;;CADALYST 10/05 Tip 2065: HatchMaker.lsp	Hatch Maker	(c) 2005 Larry Schiele

;;;* ======   B E G I N   C O D E   N O W    ======   
;;;* HatchMaker.lsp written by Lanny Schiele at TMI Systems Design Corporation
;;;* Lanny.Schiele@tmisystems.com
;;;* Tested on AutoCAD 2002 & 2006. -- does include a 'VL' function -- should work on Acad2000 on up.
 
(defun C:DrawHatch (/)
  (command "undo" "be")
  (setq os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "UCS" "w")
  (command "PLINE" "0,0" "0,1" "1,1" "1,0" "c")
  (command "zoom" "c" "0.5,0.5" 1.1)
  (setvar "OSMODE" os)
  (setvar "SNAPMODE" 1)
  (setvar "SNAPUNIT" (list 0.01 0.01))
  (command "undo" "e")
  (alert
    "Draw pattern within 1x1 box using LINE or POINT entities only..."
  )
  (princ)
)
 
(defun C:SaveHatch (/      round    dxf      ListToFile
      user     SelSet   SelSetSize ssNth
      Ent      EntInfo  EntType  pt1 pt2
      Dist     AngTo    AngFrom  XDir YDir
      Gap      DeltaX   DeltaY   AngZone Counter
      Ratio    Factor   HatchName  HatchDescr
      FileLines       FileLines  FileName
      Scaler   ScaledX  ScaledY  RF x
      y      h       _AB      _BC _AC
      _AD      _DE      _EF      _EH _FH
      DimZin
     )
;;;* BEGIN NESTED FUNCTIONS
 
  (defun round (num)
    (if (>= (- num (fix num)) 0.5)
      (fix (1+ num))
      (fix num)
    )
  )
 
  (defun dxf (code EnameOrElist / VarType)
    (setq VarType (type EnameOrElist))
    (if (= VarType (read "ENAME"))
      (cdr (assoc code (entget EnameOrElist)))
      (cdr (assoc code EnameOrElist))
    )
  )
 

  (defun ListToFile (TextList    FileName  DoOpenWithNotepad
       AsAppend    /   TextItem
       File    RetVal
      )
    (if (setq File (open FileName
    (if AsAppend
      "a"
      "w"
    )
     )
 )
      (progn
 (foreach TextItem TextList
   (write-line TextItem File)
 )
 (setq File (close File))
 (if DoOpenWithNotepad
   (startapp "notepad" FileName)
 )
      )
    )
    (FindFile FileName)
  )
 
;;;* END NESTED FUNCTIONS
  
  (princ
    (strcat
      "\n."
      "\n    0,1 ----------- 1,1"
      "\n     |               | "
      "\n     |  Lines and    | "
      "\n     |  points must  | "
      "\n     |  be snapped   | "
      "\n     |  to nearest   | "
      "\n     |  0.01         | "
      "\n     |               | "
      "\n    0,0 ----------- 1,0"
      "\n."
      "\nNote:  Lines must be drawn within 0,0 to 1,1 and lie on a 0.01 grid."
     )
  )
  (textscr)
  (getstring "\nHit [ENTER] to continue...")
 
  (princ
    "\nSelect 1x1 pattern of lines and/or points for new hatch pattern..."
  )
  (while (not (setq SelSet (ssget (list (cons 0 "LINE,POINT")))))
  )
  (setq ssNth    0
 SelSetSize (sslength SelSet)
 DimZin    (getvar "DIMZIN")
  )
  (setvar "DIMZIN" 11)
  (if (> SelSetSize 0)
    (princ "\nAnalyaing entities...")
  )
  (while (< ssNth SelSetSize)
    (setq Ent   (ssname SelSet ssNth)
   EntInfo (entget Ent)
   EntType (dxf 0 EntInfo)
   ssNth   (+ ssNth 1)
    )
    (cond
      ((= EntType "POINT")
       (setq pt1      (dxf 10 EntInfo)
      FileLine (strcat "0,"
         (rtos (car pt1) 2 6)
         ","
         (rtos (cadr pt1) 2 6)
         ",0,1,0,-1"
        )
       )
       (princ (strcat "\n" FileLine))
       (setq FileLines (cons FileLine FileLines))
      )
      ((= EntType "LINE")
       (setq pt1     (dxf 10 EntInfo)
      pt2     (dxf 11 EntInfo)
      Dist    (distance pt1 pt2)
      AngTo   (angle pt1 pt2)
      AngFrom (angle pt2 pt1)
      IsValid nil
       )
       (if
  (or (equal (car pt1) (car pt2) 0.0001)
      (equal (cadr pt1) (cadr pt2) 0.0001)
  )
   (setq DeltaX 0
  DeltaY 1
  Gap (- Dist 1)
  IsValid T
   )
   (progn
     (setq Ang   (if (< AngTo pi)
       AngTo
       AngFrom
     )
    AngZone (fix (/ Ang (/ pi 4)))
    XDir   (abs (- (car pt2) (car pt1)))
    YDir   (abs (- (cadr pt2) (cadr pt1)))
    Factor  1
    RF   1
     )
     (cond
       ((= AngZone 0)
        (setq DeltaY (abs (sin Ang))
       DeltaX (abs (- (abs (/ 1.0 (sin Ang))) (abs (cos Ang)))
       )
        )
       )
       ((= AngZone 1)
        (setq DeltaY (abs (cos Ang))
       DeltaX (abs (sin Ang))
        )
       )
       ((= AngZone 2)
        (setq DeltaY (abs (cos Ang))
       DeltaX (abs (- (abs (/ 1.0 (cos Ang))) (abs (sin Ang)))
       )
        )
       )
       ((= AngZone 3)
        (setq DeltaY (abs (sin Ang))
       DeltaX (abs (cos Ang))
        )
       )
     )
     (if (not (equal XDir YDir 0.001))
       (progn
  (setq Ratio  (if (< XDir YDir)
          (/ YDir XDir)
          (/ XDir YDir)
        )
        RF     (* Ratio Factor)
        Scaler (/ 1
    (if (< XDir YDir)
      XDir
      YDir
    )
        )
  )
  (if (not (equal Ratio (round Ratio) 0.001))
    (progn
      (while
        (and
   (<= Factor 100)
   (not (equal RF (round RF) 0.001))
        )
         (setq Factor (+ Factor 1)
        RF     (* Ratio Factor)
         )
      )
      (if (and (> Factor 1) (<= Factor 100))
        (progn
   (setq _AB (* XDir Scaler Factor)
         _BC (* YDir Scaler Factor)
         _AC (sqrt (+ (* _AB _AB) (* _BC _BC)))
         _EF 1
         x   1
   )
   (while (< x (- _AB 0.5))
     (setq y (* x (/ YDir XDir))
    h (if (< Ang (/ pi 2))
        (- (+ 1 (fix y)) y)
        (- y (fix y))
      )
     )
     (if (< h _EF)
       (setq _AD x
      _DE y
      _AE (sqrt (+ (* x x) (* y y)))
      _EF h
       )
     )
     (setq x (+ x 1))
   )
   (if (< _EF 1)
     (setq _EH (/ (* _BC _EF) _AC)
    _FH (/ (* _AB _EF) _AC)
    DeltaX (+ _AE
        (if (> Ang (/ pi 2))
          (- _EH)
          _EH
        )
     )
    DeltaY (+ _FH)
    Gap (- Dist _AC)
    IsValid T
     )
   )
        )
      )
    )
  )
       )
     )
     (if (= Factor 1)
       (setq Gap     (- Dist (abs (* Factor (/ 1 DeltaY))))
      IsValid T
       )
     )
   )
       )
       (if
  IsValid
   (progn
     (setq FileLine
     (strcat
       (angtos AngTo 0 6)
       ","
       (rtos (car pt1) 2 8)
       ","
       (rtos (cadr pt1) 2 8)
       ","
       (rtos DeltaX 2 8)
       ","
       (rtos DeltaY 2 8)
       ","
       (rtos Dist 2 8)
       ","
       (rtos Gap 2 8)
     )
     )
     (princ (strcat "\n" FileLine))
     (setq FileLines (cons FileLine FileLines))
   )
   (princ (strcat "\n * * *  Line with invalid angle "
    (angtos AngTo 0 6)
    (chr 186)
    " omitted.  * * *"
   )
   )
       )
      )
      ((princ
  (strcat "\n * * *  Invalid entity " EntType " omitted.")
       )
      )
    )
  )
  (setvar "DIMZIN" DimZin)
  (if
    (and
      FileLines
      (setq HatchDescr
      (getstring T
   "\nBriefly describe this hatch pattern: "
      )
      )
      (setq FileName (getfiled "Hatch Pattern File"
          "I:\\Acad\\Hatch\\"
          "pat"
          1
       )
      )
    )
     (progn
       (if (= HatchDescr "")
  (setq HatchDescr "Custom hatch pattern")
       )
       (setq HatchName (vl-filename-base FileName)
      FileLines (cons (strcat "*" HatchName "," HatchDescr)
        (reverse FileLines)
         )
       )
       (princ
  "\n============================================================"
       )
       (princ
  (strcat "\nPlease wait while the hatch file is created...\n"
  )
       )
       (ListToFile FileLines FileName nil nil)
       (command "delay" 1500)  ;delay required so file can be created and found (silly, but req.)
       (if (findfile FileName)
  (progn
    (setvar "HPNAME" HatchName)
    (princ (strcat "\nHatch pattern '"
     HatchName
     "' is ready to use!"
    )
    )
  )
  (progn
    (princ "\nUnable to create hatch pattern file:")
    (princ (strcat "\n  " FileName))
  )
       )
     )
     (princ
       (if FileLines
  "\nCancelled."
  "\nUnable to create hatch pattern from selected entities."
       )
     )
  )
  (princ)
)
 
(princ "\n ************************************************************** ")
(princ "\n**                                                            **")
(princ "\n*  HatchMaker.lsp written by Lanny Schiele -- enjoy!           *")
(princ "\n*                                                              *")
(princ "\n*  Type in DRAWHATCH to have the environment created to draw.  *")
(princ "\n*  Type in SAVEHATCH to save the pattern you created.          *")
(princ "\n**                                                            **")
(princ "\n ************************************************************** ")
(princ)
