;;------------------------=={ Field Arithmetic }==----------------------;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Program Overview                                                    ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;
;;  This program enables the user to perform arithmetic operations      ;;
;;  (add, subtract, multiply, divide) on numerical text or fields,      ;;
;;  with the result of the calculation represented using a field        ;;
;;  expression.                                                         ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'fieldmath' at the command-line,    ;;
;;  the program prompts the user to select a text object with           ;;
;;  numerical content. At this prompt, the user may select a            ;;
;;  single-line text object (DText), multiline text (MText), block      ;;
;;  attribute, multileader (MLeader), or dimension containing numerical ;;
;;  content, or choose the 'Constant' option to enter an arbitrary      ;;
;;  fixed number for use in the calculation.                            ;;
;;                                                                      ;;
;;  The program then prompts the user to choose an operator (addition,  ;;
;;  subtraction, multiplication, division) to follow the selected or    ;;
;;  entered numerical value in the calculation. At this prompt, the     ;;
;;  user is also provided with the option to 'Undo' the last numerical  ;;
;;  value added to the calculation, or to complete the calculation by   ;;
;;  choosing the 'Result' option.                                       ;;
;;                                                                      ;;
;;  Upon selecting the 'Result' option, the program prompts the user    ;;
;;  to specify a point or table cell to insert a field expression       ;;
;;  representing the result of the calculation.                         ;;
;;                                                                      ;;
;;  At this prompt, the user may also choose the 'Object' option in     ;;
;;  order to populate the content of an existing annotation object      ;;
;;  with the field expression.                                          ;;
;;                                                                      ;;
;;  Upon choosing this option, the user may select any single-line      ;;
;;  text (DText), multiline text (MText), single-line or multiline      ;;
;;  attribute, attributed block, or multileader (MLeader) with either   ;;
;;  multiline text or attributed block content.                         ;;
;;                                                                      ;;
;;  If the user selects an attributed block or attributed multileader   ;;
;;  with more than one attribute, the user is presented with a dialog   ;;
;;  interface listing the available attributes, and is prompted to      ;;
;;  select a destination for the field expression.                      ;;
;;                                                                      ;;
;;  The user may optionally predefine the target block/multileader      ;;
;;  attribute by specifying the attribute tag where noted at the top    ;;
;;  of the program source code.                                         ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Nested Field Expressions                                            ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;
;;  The program also supports nested field expressions: that is,        ;;
;;  annotation objects which themselves contain field expressions       ;;
;;  referencing numerical values.                                       ;;
;;                                                                      ;;
;;  When encountering such objects, by default the program references   ;;
;;  the nested field expression directly in the calculation (ignoring   ;;
;;  any field formatting), as opposed to referencing the text content   ;;
;;  of the object containing the field expression.                      ;;
;;                                                                      ;;
;;  This default behaviour may be toggled by changing the 'nst'         ;;
;;  parameter within the 'Program Parameters' section of the program    ;;
;;  source code below.                                                  ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2017  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2017-05-14                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2017-06-13                                      ;;
;;                                                                      ;;
;;  - Updated LM:fieldcode function to account for field expressions    ;;
;;    greater than 250 characters in length.                            ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2018-10-29                                      ;;
;;                                                                      ;;
;;  - Incorporated LM:outputtext function to facilitate output of the   ;;
;;    resulting field to existing table cells, text, mtext,             ;;
;;    multileaders, attributes, or as a new mtext object.               ;;
;;----------------------------------------------------------------------;;


(defun c:FA ( / *error* fmt ini itm lst msg nst opr tag tmp )

    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Field formatting for result ("" for no formatting)
        fmt "%lu6%qf1"

        ;; Reference nested fields directly (t=Yes/nil=No)
        nst t

        ;; Optional predefined attribute tag
        tag nil

;;----------------------------------------------------------------------;;

    ) 

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (if (car (setq lst (list (fieldmath:selectnumericalcontent "Exit" nst))))
        (progn
            (while
                (progn
                    (princ (apply 'strcat (cons "\n" (reverse (mapcar 'cadr lst)))))
                    (if (cddr lst)
                        (setq ini "Add Subtract Multiply Divide Undo Result"
                              msg "\nSpecify operator [Add/Subtract/Multiply/Divide] or [Undo/Result] <"
                        )
                        (setq ini "Add Subtract Multiply Divide Result"
                              msg "\nSpecify operator [Add/Subtract/Multiply/Divide] or [Result] <"
                    	)
                    )
                    (initget ini)
                    (setq opr (cond ((getkword (strcat msg (cond (opr) ("Exit")) ">: "))) (opr)))
                    (cond
                        (   (or (null opr) (= "Result" opr)) nil)
                        (   (= "Undo" opr)
                            (setq lst (cddr lst))
                        )
                        (   (setq itm (fieldmath:selectnumericalcontent "Back" nst))
                            (setq tmp (cdr (assoc opr '(("Add" . " + ") ("Subtract" . " - ") ("Multiply" . " * ") ("Divide" . " / "))))
                                  lst (vl-list* itm (list tmp tmp) lst)
                            )
                        )
                    )
                )
            )
            (LM:outputtext tag
                (if (cddr lst)
                    (strcat "%<\\AcExpr (" (apply 'strcat (reverse (mapcar 'car lst))) ")" (if (= "" fmt) "" (strcat " \\f \"" fmt "\"")) ">%")
                    (if (= "" fmt)
                        (caar lst)
                        (strcat (substr (caar lst) 1 (- (strlen (caar lst)) 2)) " \\f \"" fmt "\">%")
                    )
                )
            )
        )
    )
    
    (LM:endundo (LM:acdoc))
    (princ)
)

;;----------------------------------------------------------------------;;

(defun fieldmath:selectnumericalcontent ( bck nst / ent fld num rtn sel tmp typ )
    (while
        (not
            (progn
                (setvar 'errno 0)
                (initget (strcat  "Constant " bck))
                (setq sel (nentsel (strcat "\nSelect an object with numerical content [Constant/" bck "] <" bck ">: ")))
                (cond
                    (   (= 7 (getvar 'errno))
                        (prompt "\nMissed, try again.")
                    )
                    (   (or (null sel) (= bck sel)))
                    (   (= "Constant" sel)
                        (initget (strcat "Object " bck))
                        (cond
                            (   (null (setq tmp (getreal (strcat "\nEnter a number [Object/" bck "] <" bck ">: ")))))
                            (   (= bck tmp))
                            (   (= "Object" tmp) nil)
                            (   (setq tmp (LM:num->str tmp)
                                      rtn (list tmp tmp)
                                )
                            )
                        )
                    )
                    (   (progn
                            (if (= 4 (length sel))
                                (setq ent (last (last sel)))
                                (setq ent (car sel))
                            )
                            (not (wcmatch (setq typ (cdr (assoc 0 (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION"))
                        )
                        (prompt "\nPlease select a Text, MText, Attribute, Multileader or Dimension.")
                    )
                    (   (not
                            (setq num
                                (LM:numericalfield
                                    (setq fld
                                        (cond
                                            (   (and nst (setq tmp (LM:fieldcode ent)))
                                                (LM:removefieldformatting tmp)
                                            )
                                            (   (strcat
                                                    "%<\\AcObjProp Object(%<\\_ObjId "
                                                    (LM:objectid (vlax-ename->vla-object ent))
                                                    ">%)." (if (wcmatch typ "*DIMENSION") "Measurement" "TextString") ">%"
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (prompt "\nThe selected object does not contain numerical content.")
                    )
                    (   (setq rtn (list fld (LM:num->str num))))
                )
            )
        )
    )
    rtn
)

;; Output Text  -  Lee Mac
;; Prompts the user to specify a point at which to create an MText object containing the supplied string or to
;; select a table cell, text, mtext, multileader, attribute, or attributed block to be populated with the supplied string.
;; tag - [str] Optional target attribute tag
;; str - [str] Field expression or other text content

(defun LM:outputtext ( tag str / ent enx flg idx obj oid sel tab tmp typ )
    (if
        (setq tmp
            (ssget "_X"
                (list '(0 . "ACAD_TABLE")
                    (if (= 1 (getvar 'cvport))
                        (cons 410 (getvar 'ctab))
                       '(410 . "Model")
                    )
                )
            )
        )
        (repeat (setq idx (sslength tmp))
            (setq tab (cons (vlax-ename->vla-object (ssname tmp (setq idx (1- idx)))) tab))
        )
    )
    (while
        (not
            (progn
                (if flg
                    (progn
                        (setvar 'errno 0)
                        (initget "Point eXit")
                        (setq sel (nentsel "\nSelect text, mtext, mleader, attribute or attributed block [Point/eXit] <eXit>: "))
                    )
                    (progn
                        (initget "Object eXit")
                        (setq sel (getpoint "\nSpecify point or table cell [Object/eXit] <eXit>: "))
                    )
                )
                (cond
                    (   (= 7 (getvar 'errno))
                        (prompt "\nMissed, try again.")
                    )
                    (   (or (null sel) (= "eXit" sel)))
                    (   (= "Point" sel)
                        (setq flg nil)
                    )
                    (   (= "Object" sel)
                        (not (setq flg t))
                    )
                    (   flg
                        (setq ent (car sel)
                              enx (entget ent)
                              typ (cdr (assoc 0 enx))
                              obj (vlax-ename->vla-object ent)
                        )
                        (cond
                            (   (and (= 2 (length sel)) (wcmatch typ "TEXT,MTEXT"))
                                (if (vlax-write-enabled-p obj)
                                    (LM:outputtext:puttextstring obj str)
                                    (prompt "\nThe selected text object is on a locked layer.")
                                )
                            )
                            (   (and (= "ATTRIB" typ)
                                     (/= 'str (type tag))
                                )
                                (if (vlax-write-enabled-p obj)
                                    (progn
                                        (LM:outputtext:puttextstring obj str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (LM:outputtext:updatefield ent))
                                    )
                                    (prompt "\nThe selected attribute is on a locked layer.")
                                )
                            )
                            (   (and
                                    (or
                                        (and (= "ATTRIB" typ)
                                             (setq tmp (cdr (assoc 330 enx)))
                                        )
                                        (and (setq tmp (last (cadddr sel)))
                                             (= "INSERT" (cdr (assoc 0 (entget tmp))))
                                        )
                                    )
                                    (setq tmp (vlax-invoke (vlax-ename->vla-object tmp) 'getattributes))
                                    (or
                                        (and (= 'str (type tag))
                                             (setq idx (vl-position (strcase tag) (mapcar 'vla-get-tagstring tmp)))
                                             (setq obj (nth idx tmp))
                                        )
                                        (and (not (cdr tmp))
                                             (setq obj (car tmp))
                                        )
                                        (and (setq idx (LM:listbox "Choose Attribute" (mapcar 'vla-get-tagstring tmp) 2))
                                             (setq obj (nth (car idx) tmp))
                                        )
                                    )
                                )
                                (if (vlax-write-enabled-p obj)
                                    (progn
                                        (LM:outputtext:puttextstring obj str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (LM:outputtext:updatefield (vlax-vla-object->ename obj)))
                                    )
                                    (prompt "\nThe selected attribute is on a locked layer.")
                                )
                            )
                            (   (and (= 2 (length sel)) (= "MULTILEADER" typ))
                                (setq typ (cdr (assoc 172 (reverse enx))))
                                (cond
                                    (   (and (<= acblockcontent typ acmtextcontent) (not (vlax-write-enabled-p obj)))
                                        (prompt "\nThe selected multileader is on a locked layer.")
                                    )
                                    (   (= acmtextcontent typ)
                                        (LM:outputtext:puttextstring obj str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (vla-regen (LM:acdoc) acactiveviewport))
                                        t
                                    )
                                    (   (and
                                            (= acblockcontent typ)
                                            (setq tmp (LM:getmleaderattributes obj))
                                            (or
                                                (and (= 'str (type tag))
                                                     (setq oid (cdr (assoc (strcase tag) tmp)))
                                                )
                                                (and (not (cdr tmp))
                                                     (setq oid (cdar tmp))
                                                )
                                                (and (setq idx (LM:listbox "Choose Attribute" (mapcar 'car tmp) 2))
                                                     (setq oid (cdr (nth (car idx) tmp)))
                                                )
                                            )
                                        )
                                        (LM:setmleaderattributevalue obj oid str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (vla-regen (LM:acdoc) acactiveviewport))
                                        t
                                    )
                                    (   (prompt "\nThe select multileader has no editable content."))
                                )
                            )
                            (   (prompt "\nThe selected object is not text, mtext, multileader, attribute or attributed block."))
                        )
                    )
                    (   (setq tmp (LM:getcell tab (trans sel 1 0)))
                        (if (vlax-write-enabled-p (car tmp))
                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-settext (append tmp (list str)))))
                            (prompt "\nThe selected table cell belongs to a table on a locked layer.")
                        )
                    )
                    (   (vla-addmtext
                            (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                            (vlax-3D-point (trans sel 1 0))
                            0.0
                            str
                        )
                    )
                )
            )
        )
    )
)

(defun LM:outputtext:puttextstring ( obj str )
    (vla-put-textstring obj "") ;; To clear any existing field
    (vla-put-textstring obj str)
    t
)

(defun LM:outputtext:updatefield ( ent / cmd rtn )
    (setq cmd (getvar 'cmdecho))
    (setvar 'cmdecho 0)
    (setq rtn (vl-cmdf "_.updatefield" ent ""))
    (setvar 'cmdecho cmd)
    rtn
)

;; Get MLeader Attributes  -  Lee Mac
;; Returns an association list of attribute tags & object IDs for all attributes held by an mleader block
;; mld - [vla] MLeader vla-object
;; Returns: [lst] List of ((<Attribute Tag> . <Object ID>) ... )

(defun LM:getmleaderattributes ( mld / rtn )
    (vlax-for obj (vla-item (vla-get-blocks (vla-get-document mld)) (vla-get-contentblockname mld))
        (if
            (and
                (= "AcDbAttributeDefinition" (vla-get-objectname obj))
                (= :vlax-false (vla-get-constant obj))
            )
            (setq rtn (cons (cons (strcase (vla-get-tagstring obj)) (LM:intobjectid obj)) rtn))
        )
    )
    (reverse rtn)
)

;; Object ID (integer)  -  Lee Mac
;; Returns an integer representing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:intobjectid ( obj )
    (if (vlax-property-available-p obj 'objectid32)
        (defun LM:intobjectid ( obj ) (vla-get-objectid32 obj))
        (defun LM:intobjectid ( obj ) (vla-get-objectid   obj))
    )
    (LM:intobjectid obj)
)

;; Set MLeader Attribute Value  -  Lee Mac
;; obj - [vla] MLeader vla-object
;; idx - [int] Attribute Definition Object ID
;; str - [str] Attribute value

(defun LM:setmleaderattributevalue ( obj idx str )
    (if (vlax-method-applicable-p obj 'setblockattributevalue32)
        (defun LM:setmleaderattributevalue ( obj idx str ) (vla-setblockattributevalue32 obj idx str))
        (defun LM:setmleaderattributevalue ( obj idx str ) (vla-setblockattributevalue   obj idx str))
    )
    (LM:setmleaderattributevalue obj idx str)
)

;; List Box  -  Lee Mac
;; Displays a DCL list box allowing the user to make a selection from the supplied data.
;; msg - [str] Dialog label
;; lst - [lst] List of strings to display
;; bit - [int] 1=allow multiple; 2=return indexes
;; Returns: [lst] List of selected items/indexes, else nil
 
(defun LM:listbox ( msg lst bit / dch des tmp rtn )
    (cond
        (   (not
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open tmp "w"))
                    (write-line
                        (strcat "listbox:dialog{label=\"" msg "\";spacer;:list_box{key=\"list\";multiple_select="
                            (if (= 1 (logand 1 bit)) "true" "false") ";width=50;height=15;}spacer;ok_cancel;}"
                        )
                        des
                    )
                    (not (close des))
                    (< 0 (setq dch (load_dialog tmp)))
                    (new_dialog "listbox" dch)
                )
            )
            (prompt "\nError Loading List Box Dialog.")
        )
        (   t     
            (start_list "list")
            (foreach itm lst (add_list itm))
            (end_list)
            (setq rtn (set_tile "list" "0"))
            (action_tile "list" "(setq rtn $value)")
            (setq rtn
                (if (= 1 (start_dialog))
                    (if (= 2 (logand 2 bit))
                        (read (strcat "(" rtn ")"))
                        (mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" rtn ")")))
                    )
                )
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    (if (and tmp (setq tmp (findfile tmp)))
        (vl-file-delete tmp)
    )
    rtn
)

;; Get Cell  -  Lee Mac
;; If the supplied point lies within a cell boundary,
;; returns a list of: (<VLA Table Object> <Row> <Col>)
 
(defun LM:getcell ( lst pnt / dir )
    (setq dir (vlax-3D-point (trans (getvar 'viewdir) 1 0))
          pnt (vlax-3D-point pnt)
    )
    (vl-some
       '(lambda ( tab / row col )
            (if (= :vlax-true (vla-hittest tab pnt dir 'row 'col))
                (list tab row col)
            )
        )
        lst
    )
)

;; Number to String  -  Lee Mac
;; Converts a supplied numerical argument to a string

(defun LM:num->str ( num / dim rtn )
    (if (equal num (atoi (rtos num 2 0)) 1e-8)
        (rtos num 2 0)
        (progn
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq rtn (rtos num 2 8))
            (setvar 'dimzin dim)
            rtn
        )
    )
)

;; Numerical Field  -  Lee Mac
;; Returns the numerical content described by a supplied field expression

(defun LM:numericalfield ( fld / obj rtn )
    (vl-catch-all-apply
       '(lambda nil
            (setq obj (vla-addmtext (vla-get-modelspace (LM:acdoc)) (vlax-3D-point 0 0) 0.0 fld)
                  rtn (distof (vla-get-textstring obj) 2)
            )
        )
    )
    (if (= 'vla-object (type obj)) (vla-delete obj))
    rtn
)

;; Remove Field Formatting  -  Lee Mac
;; Removes all formatting codes from a field expression

(defun LM:removefieldformatting ( fld / ps1 ps2 )
    (if
        (and
            (setq ps1 (vl-string-search " \\f \"" fld))
            (setq ps2 (vl-string-search "\">%" (substr fld (+ 6 ps1))))
        )
        (strcat (substr fld 1 ps1) ">%" (LM:removefieldformatting (substr fld (+ 9 ps1 ps2))))
        fld
    )
)

;; Field Code  -  Lee Mac
;; Returns the field expression associated with an entity

(defun LM:fieldcode ( ent / replacefield replaceobject fieldstring enx )

    (defun replacefield ( str enx / ent fld pos )
        (if (setq pos (vl-string-search "\\_FldIdx" (setq str (replaceobject str enx))))
            (progn
                (setq ent (assoc 360 enx)
                      fld (entget (cdr ent))
                )
                (strcat
                    (substr str 1 pos)
                    (replacefield (fieldstring fld) fld)
                    (replacefield (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
                )
            )
            str
        )
    )

    (defun replaceobject ( str enx / ent pos )
        (if (setq pos (vl-string-search "ObjIdx" str))
            (strcat
                (substr str 1 (+ pos 5)) " "
                (LM:ObjectID (vlax-ename->vla-object (cdr (setq ent (assoc 331 enx)))))
                (replaceobject (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
            )
            str
        )
    )

    (defun fieldstring ( enx / itm )
        (if (setq itm (assoc 3 enx))
            (strcat (cdr itm) (fieldstring (cdr (member itm enx))))
            (cond ((cdr (assoc 2 enx))) (""))
        )
    )
    
    (if (and (wcmatch  (cdr (assoc 0 (setq enx (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION")
             (setq enx (cdr (assoc 360 enx)))
             (setq enx (dictsearch enx "ACAD_FIELD"))
             (setq enx (dictsearch (cdr (assoc -1 enx)) "TEXT"))
        )
        (replacefield (fieldstring enx) enx)
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
            (if (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*")
                (if (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                    (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
                   '(LM:ename->objectid (vlax-vla-object->ename obj))
                )
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:objectid obj)
)

;; Entity Name to ObjectID  -  Lee Mac
;; Returns the 32-bit or 64-bit ObjectID for a supplied entity name

(defun LM:ename->objectid ( ent )
    (LM:hex->decstr
        (setq ent (vl-string-right-trim ">" (vl-prin1-to-string ent))
              ent (substr ent (+ (vl-string-position 58 ent) 3))
        )
    )
)

;; Hex to Decimal String  -  Lee Mac
;; Returns the decimal representation of a supplied hexadecimal string

(defun LM:hex->decstr ( hex / foo bar )
    (defun foo ( lst rtn )
        (if lst
            (foo (cdr lst) (bar (- (car lst) (if (< 57 (car lst)) 55 48)) rtn))
            (apply 'strcat (mapcar 'itoa (reverse rtn)))
        )
    )
    (defun bar ( int lst )
        (if lst
            (if (or (< 0 (setq int (+ (* 16 (car lst)) int))) (cdr lst))
                (cons (rem int 10) (bar (/ int 10) (cdr lst)))
            )
            (bar int '(0))
        )
    )
    (foo (vl-string->list (strcase hex)) nil)
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
        "\n:: FieldArithmetic.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2017")
        " www.lee-mac.com ::"
        "\n:: Type \"fieldmath\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;