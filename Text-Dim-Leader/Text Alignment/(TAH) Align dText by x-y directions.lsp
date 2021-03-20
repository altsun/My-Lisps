;TEXTALIGN.lsp modified 04/07/97 Jeff Foster
;
;OBJECTIVE***
;The purpose of this routine is to allow the user to align
;multiple text objects based on either an x or y ordinate.
;The text is aligned based upon it's justification
;
;TO RUN***
;At the command line, type (load "c:/lispdir/textalign")
;where c:/ is the drive where TEXTALIGN.lsp is contained
;where lispdir/ is the directory where TEXTALIGN.lsp is contained
;
;
;If you find this routine to be helpful, please give consideration
;to making a cash contribution of $10.00 to:
; Jeff Foster
; 590 Penny Rd.
; Angier, NC 27501
;

(DEFUN C:TAH ()
(INITGET 1 "X x Y y")
(SETQ XY_ORD (GETKWORD "\nALIGN <X> OR <Y> ORDINATE OF TEXT?: "))
(SETQ ORD (GETPOINT "\nPICK ORDINATE: "))
(PRINC "\nSELECT TEXT TO ALIGN")
(SETQ SS (SSGET))
(WHILE (> (SSLENGTH SS) 0)
(SETQ EN (SSNAME SS 0))
(SETQ ED (ENTGET EN))
(SETQ AS (CDR (ASSOC '0 ED)))
(if (= AS "TEXT")
(progn
(setq code72 (cdr (assoc 72 ed)))
(setq code73 (cdr (assoc 73 ed)))
)
)
(cond ((and (= AS "TEXT") (= code72 0) (= code73 0))
(setq pt1 (cdr (assoc '10 ed)))
(setq pt1_x (car pt1))
(setq pt1_y (cadr pt1))
)
((and (= AS "TEXT") (/= code72 0) (/= code73 0))
(setq pt1 (cdr (assoc '11 ed)))
(setq pt1_x (car pt1))
(setq pt1_y (cadr pt1))
)
((and (= AS "TEXT") (= code72 0) (/= code73 0))
(setq pt1 (cdr (assoc '11 ed)))
(setq pt1_x (car pt1))
(setq pt1_y (cadr pt1))
)
((and (= AS "TEXT") (/= code72 0) (= code73 0))
(setq pt1 (cdr (assoc '11 ed)))
(setq pt1_x (car pt1))
(setq pt1_y (cadr pt1))
)
)
(COND ((and (= AS "TEXT") (= XY_ORD "X"))
(COMMAND "MOVE" EN "" PT1 (LIST (CAR ORD) PT1_Y))
(SSDEL EN SS)
)
((AND (= AS "TEXT") (= XY_ORD "Y"))
(COMMAND "MOVE" EN "" PT1 (LIST PT1_X (CADR ORD)))
(SSDEL EN SS)
)
((/= AS "TEXT")
(SSDEL EN SS)
)
)
)
(PRIN1)
)