;;------------------------=={ Burst Upgraded }==------------------------;;
;;                                                                      ;;
;;  This program operates in much the same way as the familiar          ;;
;;  Express Tools' Burst command, however invisible block attributes    ;;
;;  are not displayed with the resulting exploded components.           ;;
;;                                                                      ;;
;;  Following a valid selection of blocks to burst, the program         ;;
;;  converts all visible single-line & multi-line attributes into Text  ;;
;;  and MText respectively, before proceeding to explode the block,     ;;
;;  and deleting the original attribute objects.                        ;;
;;                                                                      ;;
;;  The core function accepts a selection set argument and may hence    ;;
;;  be called from within other custom programs to burst all blocks     ;;
;;  in a supplied selection set.                                        ;;
;;                                                                      ;;
;;  The methods used by the program should also perform much faster &   ;;
;;  more efficiently than those used by the Express Tools' Burst.lsp.   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-11-25                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2013-08-29                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-02-23                                      ;;
;;                                                                      ;;
;;  - Program restructured to accept selection set argument.            ;;
;;  - Program now also explodes non-attributed blocks.                  ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2015-10-31                                      ;;
;;                                                                      ;;
;;  - Program modified to account for non-uniformly scaled blocks.      ;;
;;  - Command syntax changed to 'myburst'.                              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2018-01-06                                      ;;
;;                                                                      ;;
;;  - Program modified to retain visible constant attributes.           ;;
;;  - Corrected LM:usblock-p function to account for mirrored blocks.   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2018-07-09                                      ;;
;;                                                                      ;;
;;  - Accounted for multiline attributes whose text content occupies    ;;
;;    multiple group 1 & 3 DXF groups.                                  ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2018-12-10                                      ;;
;;                                                                      ;;
;;  - Accounted for invisible objects created when bursting dynamic     ;;
;;    blocks with visibility states.                                    ;;
;;  - Fixed bug causing attributes with transparency to be removed.     ;;
;;  - Integrated Nested Burst program.                                  ;;
;;----------------------------------------------------------------------;;
;;  Version 1.7    -    2018-12-22                                      ;;
;;                                                                      ;;
;;  - Accounted for nested xrefs (excluding them from burst operation). ;;
;;----------------------------------------------------------------------;;

(defun c:pburst nil (LM:burst nil))
(defun c:nburst nil (LM:burst   t))

;;----------------------------------------------------------------------;;

(defun LM:burst ( nst / *error* )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (LM:startundo (LM:acdoc))
    (LM:burstsel
        (LM:ssget "\nSelect blocks to burst: "
            (list "_:L"
                (append '((0 . "INSERT"))
                    (
                        (lambda ( / def lst )
                            (while (setq def (tblnext "block" (null def)))
                                (if (= 4 (logand 4 (cdr (assoc 70 def))))
                                    (setq lst (vl-list* "," (cdr (assoc 2 def)) lst))
                                )
                            )
                            (if lst (list '(-4 . "<NOT") (cons 2 (apply 'strcat (cdr lst))) '(-4 . "NOT>")))
                        )
                    )
                    (if (= 1 (getvar 'cvport))
                        (list (cons 410 (getvar 'ctab)))
                       '((410 . "Model"))
                    )
                )
            )
        )
        nst
    )
    (LM:endundo (LM:acdoc)) (princ)
)

(defun LM:burstsel ( sel nst / idx )
    (if (= 'pickset (type sel))
        (repeat (setq idx (sslength sel))
            (LM:burstobject (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))) nst)
        )
    )
)

(defun LM:burstobject ( obj nst / cmd col ent err lay lin lst qaf tmp )
    (if
        (and
            (= "AcDbBlockReference" (vla-get-objectname obj))
            (not (vlax-property-available-p obj 'path))
            (vlax-write-enabled-p  obj)
            (or (and (LM:usblock-p obj)
                     (not (vl-catch-all-error-p (setq err (vl-catch-all-apply 'vlax-invoke (list obj 'explode)))))
                     (setq lst err)
                )
                (progn
                    (setq tmp (vla-copy obj)
                          ent (LM:entlast)
                          cmd (getvar 'cmdecho)
                          qaf (getvar 'qaflags)
                    )
                    (setvar 'cmdecho 0)
                    (setvar 'qaflags 0)
                    (vl-cmdf "_.explode" (vlax-vla-object->ename tmp))
                    (setvar 'qaflags qaf)
                    (setvar 'cmdecho cmd)
                    (while (setq ent (entnext ent))
                        (setq lst (cons (vlax-ename->vla-object ent) lst))
                    )
                    lst
                )
            )
        )
        (progn
            (setq lay (vla-get-layer    obj)
                  col (vla-get-color    obj)
                  lin (vla-get-linetype obj)
            )
            (foreach att (vlax-invoke obj 'getattributes)
                (if (vlax-write-enabled-p att)
                    (progn
                        (if (= "0" (vla-get-layer att))
                            (vla-put-layer att lay)
                        )
                        (if (= acbyblock (vla-get-color att))
                            (vla-put-color att col)
                        )
                        (if (= "byblock" (strcase (vla-get-linetype att) t))
                            (vla-put-linetype att lin)
                        )
                    )
                )
                (if
                    (and
                        (= :vlax-false (vla-get-invisible att))
                        (= :vlax-true  (vla-get-visible   att))
                    )
                    (   (if (and (vlax-property-available-p att 'mtextattribute) (= :vlax-true (vla-get-mtextattribute att)))
                            LM:burst:matt2mtext 
                            LM:burst:att2text
                        )
                        (entget (vlax-vla-object->ename att))
                    )
                )
            )
            (foreach new lst
                (cond
                    (   (not (vlax-write-enabled-p new)))
                    (   (= :vlax-false (vla-get-visible new))
                        (vla-delete new)
                    )
                    (   t
                        (if (= "0" (vla-get-layer new))
                            (vla-put-layer new lay)
                        )
                        (if (= acbyblock (vla-get-color new))
                            (vla-put-color new col)
                        )
                        (if (= "byblock" (strcase (vla-get-linetype new) t))
                            (vla-put-linetype new lin)
                        )
                        (if (= "AcDbAttributeDefinition" (vla-get-objectname new))
                            (progn
                                (if
                                    (and
                                        (= :vlax-true  (vla-get-constant  new))
                                        (= :vlax-false (vla-get-invisible new))
                                    )
                                    (   (if (and (vlax-property-available-p new 'mtextattribute) (= :vlax-true (vla-get-mtextattribute new)))
                                            LM:burst:matt2mtext 
                                            LM:burst:att2text
                                        )
                                        (entget (vlax-vla-object->ename new))
                                    )
                                )
                                (vla-delete new)
                            )
                            (if nst (LM:burstobject new nst))
                        )
                    )
                )
            )
            (vla-delete obj)
        )
    )
)

(defun LM:burst:removepairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (member (car x) itm)) lst)
)

(defun LM:burst:remove1stpairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (if (member (car x) itm) (progn (setq itm (vl-remove (car x) itm)) t))) lst)
)
  
(defun LM:burst:att2text ( enx )
    (entmakex
        (append '((0 . "TEXT"))
            (LM:burst:removepairs '(000 002 003 070 074 100 280 440)
                (subst (cons 73 (cdr (assoc 74 enx))) (assoc 74 enx) enx)
            )
        )
    )
)

(defun LM:burst:matt2mtext ( enx )
    (entmakex
        (append '((0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText"))
            (LM:burst:remove1stpairs
                (if (= "ATTDEF" (cdr (assoc 0 enx)))
                   '(001 003 007 010 040 041 050 071 072 073 210)
                   '(001 007 010 040 041 050 071 072 073 210)
                )
                (LM:burst:removepairs '(000 002 011 042 043 051 070 074 100 101 102 280 330 360 440) enx)
            )
            (list (assoc 011 (reverse enx)))
        )
    )
)

;; Uniformly Scaled Block  -  Lee Mac
;; Returns T if the supplied VLA Block Reference is uniformly scaled
;; obj - [vla] VLA Block Reference

(defun LM:usblock-p ( obj / s )
    (if (vlax-property-available-p obj 'xeffectivescalefactor)
        (setq s "effectivescalefactor")
        (setq s "scalefactor")
    )
    (eval
        (list 'defun 'LM:usblock-p '( obj )
            (list 'and
                (list 'equal
                    (list 'abs (list 'vlax-get-property 'obj (strcat "x" s)))
                    (list 'abs (list 'vlax-get-property 'obj (strcat "y" s)))
                    1e-8
                )
                (list 'equal
                    (list 'abs (list 'vlax-get-property 'obj (strcat "x" s)))
                    (list 'abs (list 'vlax-get-property 'obj (strcat "z" s)))
                    1e-8
                )
            )
        )
    )
    (LM:usblock-p obj)
)

;; entlast  -  Lee Mac
;; A wrapper for the entlast function to return the last subentity in the database

(defun LM:entlast ( / ent tmp )
    (setq ent (entlast))
    (while (setq tmp (entnext ent)) (setq ent tmp))
    ent
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: BurstUpgraded.lsp | Version 1.7 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"pburst\" to burst primary  |  \"nburst\" to burst primary + nested ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;