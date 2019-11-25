;;; ------------------------------------------------------------------------
 
;;; TEXTLEADER.LSP Version 1.2
 
;;;
 
;;; Copyright© August, 2007
 
;;; Timothy G. Spangler
 
;;;
 
;;; Permission to use, copy, modify, and distribute this software
 
;;; for any purpose and without fee is hereby granted, provided
 
;;; that the above copyright notice appears in all copies and
 
;;; that both that copyright notice and the limited warranty and
 
;;; restricted rights notice below appear in all supporting
 
;;; documentation.
 
;;;
 
;;; Add leader to text (Non Associating Leader).
 
;;;
 
;;; ------------------------------------------------------------------------
 
(defun C:TL (/) (C:TEXTLEADER)); Program Shortcut
 
;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun C:TEXTLEADER (/ *error* OldCmdecho OldOsmode OldOrthoMode OldClayer ExludeList)
 
;; Set Env
 
(TEXT_LEADER_SET_ENV)
 
;;; Error Handling Routine ;;;
 
(defun *error* (MSG)
 
(if(not(member MSG '("Function cancelled" "quit / exit abort")))
 
(princ (strcat "\n*** Program Error: " (strcase MSG) " ***"))
 
(princ "\n... Program Cancelled ...")
 
)
 
(while (< 0 (getvar "CMDACTIVE"))
 
(command)
 
)
 
(TEXT_LEADER_RESET_ENV)
 
(princ)
 
)
 
;; Main Code
 
(TEXT_LEADER_RUN)
 
)
 
;;; ------------ Begin Main Routine
 
(defun TEXT_LEADER_RUN (/ TextEnt TextEntList TextLayer DtextBox DtextEntList TextRotate MtextLine
 
DtextInsPoint TempPoint LeaderEnd)
 
;; Get text object from selection
 
(while (null (setq TextEnt (entsel "\n Select Top or Bottom Line of Text: ")))
 
(princ "\n Nothing Selected...")
 
)
 
;; Get entity list from text
 
(setq TextEntList (entget (car TextEnt)))
 
;; Get text layer
 
(setq TextLayer (cdr(assoc 8 TextEntList)))
 
;; Set layer to text layer
 
(setvar "CLAYER" TextLayer)
 
;; If selected text is "TEXT"
 
(if (= (cdr (assoc 0 TextEntList)) "TEXT")
 
(progn
 
(setq DtextBox (textbox (entget (car TextEnt))))
 
(setq DtextEntList (entget (car TextEnt)))
 
(setq TextRotate (cdr(assoc 50 DtextEntList)))
 
)
 
)
 
;; If selected text is "MTEXT"
 
(if (= (cdr (assoc 0 TextEntList)) "MTEXT")
 
(progn
 
(command "explode" TextEnt)
 
(setq MtextLine (ssget (cadr TextEnt)))
 
(setq DtextEntList (entget (ssname MtextLine 0)))
 
(setq DtextBox (textbox (entget (ssname MtextLine 0))))
 
(setq TextRotate (cdr(assoc 50 DtextEntList)))
 
(command "u")
 
)
 
)
 
;; If selected entity is not "TEXT" or "MTEXT"
 
(if (member (cdr (assoc 0 TextEntList)) ExludeList)
 
(progn
 
(alert "Selected entity is not TEXT or MTEXT")
 
(TEXT_LEADER_RUN)
 
)
 
)
 
;; Get insertion point of text
 
(setq DtextInsPoint (cdr (assoc 10 DtextEntList)))
 
;; Check the rotation of the text
 
(cond
 
((equal TextRotate 1.5708 0.0001)
 
;; Get center point of textbox (from 0,0)
 
(setq TempPoint
 
(list
 
(/ (+ (cadar DtextBox)(cadadr DtextBox)) 2.0)
 
(/ (+ (caadr DtextBox)(caar DtextBox)) 2.0)
 
(cadddr (assoc 10 DtextEntList))
 
)
 
)
 
;; Get the center point of the selected text object
 
(setq InsertPoint
 
(list
 
(- (car DtextInsPoint)(car TempPoint))
 
(+ (cadr DtextInsPoint)(cadr TempPoint))
 
(+ (caddr DtextInsPoint)(caddr TempPoint))
 
)
 
)
 
;; Set the leader end point
 
(setq LeaderEnd
 
(+ (/ (- (caadr DtextBox) (caar DtextBox)) 2.0)
 
(* 0.0625 (getvar "dimscale")) ;CHANGE THIS TO CHANGE GAT BETWEEN TEXT AND LEADER
 
)
 
)
 
;; Prompt to create the leader
 
(prompt "\n Select Leader Start and Bend Points: ")
 
;; Run the leader command with the point filter
 
(command "leader"
 
PAUSE
 
".X"
 
InsertPoint
 
PAUSE
 
(polar InsertPoint (angle InsertPoint (getvar "lastpoint")) LeaderEnd)
 
""
 
""
 
"n"
 
)
 
)
 
((equal TextRotate 0.0 0.0001)
 
;; Get center point of textbox (from 0,0)
 
(setq TempPoint
 
(list
 
(/ (+ (caadr DtextBox) (caar DtextBox)) 2.0)
 
(/ (+ (cadar DtextBox) (cadadr DtextBox)) 2.0)
 
(cadddr (assoc 10 DtextEntList))
 
)
 
)
 
;; Get the center point of the selected text object
 
(setq InsertPoint
 
(list
 
(+ (car DtextInsPoint) (car TempPoint))
 
(+ (cadr DtextInsPoint) (cadr TempPoint))
 
(+ (caddr DtextInsPoint) (caddr TempPoint))
 
)
 
)
 
;; Set the leader end point
 
(setq LeaderEnd
 
(+ (/ (- (caadr DtextBox) (caar DtextBox)) 2.0)
 
(* 0.0625 (getvar "dimscale")) ;CHANGE THIS TO CHANGE GAP BETWEEN TEXT AND LEADER
 
)
 
)
 
;; Prompt to create the leader
 
(prompt "\n Select Leader Start and Bend Points: ")
 
;; Run the leader command with the point filter
 
(command "leader"
 
PAUSE
 
".Y"
 
InsertPoint
 
PAUSE
 
(polar InsertPoint (angle InsertPoint (getvar "lastpoint")) LeaderEnd)
 
""
 
""
 
"n"
 
)
 
)
 
((/= (or (equal TextRotate 0.0 0.0001)(equal TextRotate 1.5708 0.0001)))
 
(alert "Selected text not at a suitable angle")
 
(TEXT_LEADER_RUN)
 
)
 
)
 
(TEXT_LEADER_RESET_ENV)
 
)
 
;;; ------------ Set Environment Settings
 
(defun TEXT_LEADER_SET_ENV (/)
 
&nbsp;
 
;; Set sysetem variables
 
(setq OldCmdecho (getvar "CMDECHO"))
 
(setq OldOsmode (getvar "OSMODE"))
 
(setq OldOrthoMode (getvar "ORTHOMODE"))
 
(setq OldClayer (getvar "CLAYER"))
 
(setvar "CMDECHO" 0)
 
(setvar "ORTHOMODE" 0)
 
(setvar "OSMODE" 513)
 
;;; Undo marker
 
(command "_UNDO" "BEGIN")
 
;; Set the exclusion list
 
(setq ExludeList (list "3DFACE" "3DSOLID" "ARC" "ATTDEF" "ATTRIB" "BODY" "CIRCLE" "DIMENSION" "ELLIPSE"
 
"HATCH" "IMAGE" "INSERT" "LEADER" "LINE" "LWPOLYLINE" "MLINE" "OLEFRAME" "OLE2FRAME" "POINT" "POLYLINE"
 
"RAY" "REGION" "SEQUEND" "SHAPE" "SOLID" "SPLINE" "TOLERANCE" "TRACE" "VERTEX" "VIEWPORT" "XLINE"))
 
;; Add program description to status line
 
(grtext -2 (strcat "Text Leader " "v1.2" " Copyright© 2007"))
 
)
 
;;; ------------ Reset Environment Settings
 
(defun TEXT_LEADER_RESET_ENV (/)
 
;;; Undo marker
 
(command "_UNDO" "END")
 
;; Reset system variable
 
(grtext -2 "")
 
(setvar "CLAYER" OldClayer)
 
(setvar "OSMODE" OldOsmode)
 
(setvar "ORTHOMODE" OldOrthoMode)
 
(setvar "CMDECHO" OldCmdecho)
 
(princ)
 
)
 
;;;
 
;;; Echos to the command line
 
(princ "\n Text Leader v1.2© \n Timothy Spangler, \nAugust, 2007....loaded.")
 
(terpri)
 
(princ "Type \"TL\" to run")
 
(print)
 
;;; End echo