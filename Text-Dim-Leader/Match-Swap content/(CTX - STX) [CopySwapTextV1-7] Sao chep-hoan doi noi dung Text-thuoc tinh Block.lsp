;;-----------------------=={ Copy or Swap Text }==----------------------;;
;;                                                                      ;;
;;  This program enables a user to either copy the text content from    ;;
;;  a multitude of 'source' objects to a selection of 'destination'     ;;
;;  objects, or swap the text content between two objects.              ;;
;;                                                                      ;;
;;  To copy text, the program may be called with 'ctx' at the AutoCAD   ;;
;;  command line. The user may then select either a Text, MText,        ;;
;;  Attribute, Dimension or Multileader object and proceed to copy the  ;;
;;  associated text content to a selection of any of the aforementioned ;;
;;  objects.                                                            ;;
;;                                                                      ;;
;;  The objects to which the text is copied may be selected             ;;
;;  individually or, alternatively, upon selecting the 'Multiple'       ;;
;;  option, the user may make a selection of multiple objects to which  ;;
;;  the text string will be copied.                                     ;;
;;                                                                      ;;
;;  The program also provides functionality to allow the user to        ;;
;;  switch the text content between two objects. Upon calling the       ;;
;;  program with 'stx' at the command line, the user may select two     ;;
;;  objects whose text content will be swapped.                         ;;
;;                                                                      ;;
;;  If the user choose the 'Settings' option when running the           ;;
;;  program, they may alter whether formatting overrides present in     ;;
;;  selected source objects is applied to selected destination objects  ;;
;;  which permit the use of such formatting. If the user opts to remove ;;
;;  the formatting, the program will remove all formatting from the     ;;
;;  text string prior to copying or swapping it.                        ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-12-16                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2010-12-17                                      ;;
;;                                                                      ;;
;;  - Added ability to retain/remove MText formatting. Setting is       ;;
;;    stored as a global variable (*retain*)                            ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2010-12-20                                      ;;
;;                                                                      ;;
;;  - Entire program rewritten to include SwapText capability.          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2011-01-05                                      ;;
;;                                                                      ;;
;;  - Fixed minor formatting bugs.                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2015-02-23                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2016-03-19                                      ;;
;;                                                                      ;;
;;  - Program mostly rewritten to incorporate full compatibility with   ;;
;;    Dimensions & Multileader Block Attributes.                        ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2016-03-21                                      ;;
;;                                                                      ;;
;;  - Fixed bug which reported that an MLeader with Attributed Block    ;;
;;    content had 'no editable content' if an Attribute Definition      ;;
;;    was not the last component found within the Block Definition of   ;;
;;    the attributed block used by the MLeader.                         ;;
;;  - Fixed bug causing the program to crash when detecting name of     ;;
;;    Block Container for Dimension entities.                           ;;
;;----------------------------------------------------------------------;;
;;  Version 1.7    -    2017-04-12                                      ;;
;;                                                                      ;;
;;  - Fixed a bug which resulted in an incorrect output when processing ;;
;;    MText whose text content occupied multiple DXF group 3 entries.   ;;
;;----------------------------------------------------------------------;;

(defun c:ctx nil (copyswaptext:main nil))
(defun c:stx nil (copyswaptext:main   t))

;;----------------------------------------------------------------------;;

(defun copyswaptext:main ( flg / *error* des ent fun idx mt1 mt2 ret src st1 st2 )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type copyswaptext:regexobject))
                 (not (vlax-object-released-p copyswaptext:regexobject))
            )
            (progn
                (vlax-release-object copyswaptext:regexobject)
                (setq copyswaptext:regexobject nil)
            )
        )
        (copyswaptext:endundo (copyswaptext:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (copyswaptext:startundo (copyswaptext:acdoc))
    (or (setq copyswaptext:retain (getenv "LMac\\copytext-retain"))
        (setq copyswaptext:retain (setenv "LMac\\copytext-retain" "Yes"))
    )
    
    (cond
        (   (not (copyswaptext:regex)))
        (   (not      (setq src (copyswaptext:getselection (if flg "\nSelect text to swap [Settings/Exit] <Exit>: " "\nSelect source text [Settings/Exit]: ") "Settings Exit"))))
        (   (and  flg (setq des (copyswaptext:getselection    "\nAnd text to swap it with [Settings/Exit] <Exit>: " "Settings Exit")))
            (setq mt1 (copyswaptext:allowsformatting (entget (car src)))
                  mt2 (copyswaptext:allowsformatting (entget (car des)))
                  st1 (copyswaptext:unformat (cadr src) mt1)
                  st2 (copyswaptext:unformat (cadr des) mt2)
                  ret (= "Yes" copyswaptext:retain)
            )
            (mapcar 'copyswaptext:settextcontent (list src des)
                (cond
                    (   (and mt1 mt2 ret) (list (cadr des) (cadr src)))
                    (   (and mt1 mt2)     (list (car  st2) (car  st1)))
                    (   mt1               (list (car  st2) (cadr st1)))
                    (   mt2               (list (cadr st2) (car  st1)))
                    (   ret               (list (cadr des) (cadr src)))
                    (                     (list (cadr st2) (cadr st1)))
                )
            )
        )
        (   (setq mt1 (copyswaptext:allowsformatting (entget (car src)))
                  st1 (copyswaptext:unformat (cadr src) mt1)
                  fun
                (lambda ( mt2 ret )
                    (cond
                        (   (and mt1 mt2 ret) (cadr src))
                        (   (and mt1 mt2)     (car  st1))
                        (   mt1               (cadr st1))
                        (   mt2               (car  st1))
                        (   ret               (cadr src))
                        (                     (cadr st1))
                    )
                )
            )
            (while (setq des (copyswaptext:getselection "\nSelect destination text [Multiple/Settings/Exit] <Exit>: " "Multiple Settings Exit"))
                (if (= 'pickset (type des))
                    (repeat (setq idx (sslength des))
                        (copyswaptext:settextcontent
                            (list (setq ent (ssname des (setq idx (1- idx)))))
                            (fun (copyswaptext:allowsformatting (entget ent)) (= "Yes" copyswaptext:retain))
                        )
                    )
                    (copyswaptext:settextcontent des
                        (fun (copyswaptext:allowsformatting (entget (car des))) (= "Yes" copyswaptext:retain))
                    )
                )
            )
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:getselection ( msg ini / rtn sel str tmp )
    (while
        (progn
            (setvar 'errno 0)
            (initget ini)
            (setq sel (nentselp (strcat "\nFormatting retained: " copyswaptext:retain msg)))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (or (null sel) (= "Exit" sel))
                    nil
                )
                (   (= "Settings" sel)
                    (initget "Yes No")
                    (if (setq tmp (getkword (strcat "\nRetain mtext formatting? [Yes/No] <" (getenv "LMac\\copytext-retain") ">: ")))
                        (setenv "LMac\\copytext-retain" (setq copyswaptext:retain tmp))
                    )
                    t
                )
                (   (= "Multiple" sel)
                    (not
                        (setq rtn
                            (copyswaptext:ssget "\nSelect destination text <back>: "
                               '(   "_:L"
                                    (
                                        (-4 . "<OR")
                                            (0 . "TEXT,MTEXT,MULTILEADER,*DIMENSION")
                                            (-4 . "<AND")
                                                (00 . "INSERT")
                                                (66 . 1)
                                            (-4 . "AND>")
                                        (-4 . "OR>")
                                    )
                                )
                            )
                        )
                    )
                )
                (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 (entget (car sel)))))))))
                    (princ "\nSelected object is on a locked layer.")
                )
                (   (null (setq rtn (copyswaptext:gettextcontent sel))))
            )
        )
    )
    rtn
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:settextcontent ( lst str / con enx obj par tmp typ )
    (setq enx (entget (car lst))
          typ (cdr (assoc 0 enx))
          obj (vlax-ename->vla-object (car lst))
    )
    (cond
        (   (wcmatch typ "*TEXT,ATTRIB")
            (vla-put-textstring obj str)
        )
        (   (wcmatch typ "*DIMENSION")
            (vla-put-textoverride obj str)
        )
        (   (and (= "INSERT" typ)
                 (= 1 (cdr (assoc 66 enx)))
            )
            (foreach att (vlax-invoke obj 'getattributes)
                (if (vlax-write-enabled-p att)
                    (vla-put-textstring att str)
                )
            )
        )
        (   (= "MULTILEADER" typ)
            (setq con (cdr (assoc 172 (reverse enx))))
            (cond
                (   (= acmtextcontent con)
                    (vla-put-textstring obj str)
                )
                (   (= acblockcontent con)
                    (if (caddr lst)
                        (copyswaptext:setblockattributevalue obj (caddr lst) str)
                        (foreach oid (copyswaptext:getattributetagids (vla-get-contentblockname obj))
                            (copyswaptext:setblockattributevalue obj oid str)
                        )
                    )
                )
            )
        )
    )
    (while (setq tmp (member '(102 . "}") enx)) (setq enx (cdr tmp)))
    (if (and (setq par (cdr (assoc 330 enx)))
             (setq par (cdr (assoc 002 (entget par))))
             (wcmatch (strcase par t) "~`**_space*")
        )
        (vla-regen (copyswaptext:acdoc) acallviewports)
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:gettextcontent ( sel / con ent enx tmp typ )
    (if (and (= 4 (length sel)) (wcmatch (cdr (assoc 0 (entget (car (last sel))))) "*DIMENSION"))
        (setq ent (car (last sel)))
        (setq ent (car sel))
    )
    (setq enx (entget ent)
          typ (cdr (assoc 0 enx))
    )
    (cond
        (   (= "TEXT" typ)
            (list ent (cdr (assoc 1 enx)))
        )
        (   (wcmatch typ "*DIMENSION")
            (list ent
                (if (= "" (cdr (assoc 1 enx)))
                    (copyswaptext:getdimtext (cdr (assoc 2 enx)))
                    (cdr (assoc 1 enx))
                )
            )
        )
        (   (wcmatch typ "ATTRIB,MTEXT")
            (list ent (copyswaptext:gettext enx))
        )
        (   (= "MULTILEADER" typ)
            (setq con (cdr (assoc 172 (reverse enx))))
            (cond
                (   (= acmtextcontent con)
                    (list ent (cdr (assoc 304 enx)))
                )
                (   (= acblockcontent con)
                    (if (setq tmp (copyswaptext:getmleaderattribute (vlax-ename->vla-object ent)))
                        (cons ent tmp)
                    )
                )
            )
        )
        (   (and (= "INSERT" typ)
                 (= 1 (cdr (assoc 66 enx)))
            )
            (copyswaptext:getattribute ent)
        )
        (   (= 4 (length sel))
            (copyswaptext:gettextcontent (list (car (last sel))))
        )
        (   (prompt "\nInvalid object selected."))
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:getattribute ( ent / enx idx tmp )
    (setq ent (entnext ent)
          enx (entget  ent)
    )
    (while (= "ATTRIB" (cdr (assoc 0 enx)))
        (setq tmp (cons (list ent (copyswaptext:gettext enx)) tmp)
              ent (entnext ent)
              enx (entget  ent)
        )
    )
    (cond
        (   (null (cdr (setq tmp (reverse tmp))))
            (car tmp)
        )
        (   (setq idx
                (copyswaptext:listbox "Select Attribute"
                    (mapcar
                       '(lambda ( itm )
                            (cadr
                                (copyswaptext:unformat (cadr itm)
                                    (copyswaptext:allowsformatting (entget (car itm)))
                                )
                            )
                        )
                        tmp
                    )
                )
            )
            (nth idx tmp)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:getmleaderattribute ( obj / idx tmp )
    (setq tmp
        (mapcar
           '(lambda ( oid ) (list (copyswaptext:getblockattributevalue obj oid) oid))
            (copyswaptext:getattributetagids (vla-get-contentblockname obj))
        )
    )
    (cond
        (   (null tmp)
            (prompt "\nSelected MLeader has no editable content.")
        )
        (   (null (cdr tmp))
            (car tmp)
        )
        (   (setq idx
                (copyswaptext:listbox "Select Attribute"
                    (mapcar '(lambda ( itm ) (cadr (copyswaptext:unformat (car itm) t))) tmp)
                )
            )
            (nth idx tmp)
        )
    )
)

;;----------------------------------------------------------------------;;
 
(defun copyswaptext:getattributetagids ( blk )
    (eval
        (list 'defun 'copyswaptext:getattributetagids '( blk / itm tmp )
            (list 'cond '((cdr (assoc (strcase blk) copyswaptext:attributetagids)))
                (list 't
                    (list 'vlax-for 'obj (list 'vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) 'blk)
                       '(if
                            (and
                                (= "AcDbAttributeDefinition" (vla-get-objectname obj))
                                (= :vlax-false (vla-get-constant obj))
                            )
                            (setq tmp (cons (copyswaptext:objectid obj) tmp))
                        )
                    )
                   '(setq copyswaptext:attributetagids (cons (cons (strcase blk) (reverse tmp)) copyswaptext:attributetagids))
                   '(copyswaptext:getattributetagids blk)
                )
            )
        )
    )
    (copyswaptext:getattributetagids blk)
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:getblockattributevalue ( obj idx )
    (if (vlax-method-applicable-p obj 'getblockattributevalue32)
        (defun copyswaptext:getblockattributevalue ( obj idx ) (vla-getblockattributevalue32 obj idx))
        (defun copyswaptext:getblockattributevalue ( obj idx ) (vla-getblockattributevalue   obj idx))
    )
    (copyswaptext:getblockattributevalue obj idx )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:setblockattributevalue ( obj idx str )
    (if (vlax-method-applicable-p obj 'setblockattributevalue32)
        (defun copyswaptext:setblockattributevalue ( obj idx str ) (vla-setblockattributevalue32 obj idx str))
        (defun copyswaptext:setblockattributevalue ( obj idx str ) (vla-setblockattributevalue   obj idx str))
    )
    (copyswaptext:setblockattributevalue obj idx str)
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:objectid ( obj )
    (if (vlax-property-available-p obj 'objectid32)
        (defun copyswaptext:objectid ( obj ) (vla-get-objectid32 obj))
        (defun copyswaptext:objectid ( obj ) (vla-get-objectid   obj))
    )
    (copyswaptext:objectid obj)
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:listbox ( msg lst / dch des tmp rtn )
    (cond
        (   (not
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open tmp "w"))
                    (write-line
                        (strcat
                            "listbox:dialog{label=\"" msg "\";spacer;"
                            ":list_box{key=\"list\";multiple_select=false;width=50;height=15;}spacer;ok_cancel;}"
                        )
                        des
                    )
                    (not (close des))
                    (< 0 (setq dch (load_dialog tmp)))
                    (new_dialog "listbox" dch)
                )
            )
            (prompt "\nError loading List Box dialog.")
        )
        (   t     
            (start_list "list")
            (foreach itm lst (add_list itm))
            (end_list)
            (setq rtn (set_tile "list" "0"))
            (action_tile "list" "(setq rtn $value)")
            (setq rtn (if (= 1 (start_dialog)) (atoi rtn)))
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

;;----------------------------------------------------------------------;;

(defun copyswaptext:gettext ( enx / itm str )
    (setq enx (reverse enx)
          str (cdr (assoc 1 enx))
    )
    (while (setq itm (assoc 3 enx))
        (setq str (strcat (cdr itm) str)
              enx (cdr (member itm enx))
        )
    )
    str
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:getdimtext ( blk / ent rtn )
    (if (setq ent (tblobjname "block" blk))
        (while (and (setq ent (entnext ent)) (null rtn))
            (if (= "MTEXT" (cdr (assoc 0 (entget ent))))
                (setq rtn (cadr (copyswaptext:unformat (copyswaptext:gettext (entget ent)) t)))
            )
        )
    )
    rtn
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:allowsformatting ( enx )
    (or (wcmatch (cdr (assoc 0 enx)) "MTEXT,MULTILEADER,*DIMENSION")
        (and (= "ATTRIB" (cdr (assoc 0 enx)))
             (member '(101 . "Embedded Object") enx)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:regex ( / rgx )
    (cond
        (   copyswaptext:regexobject   )
        (   (or (null (setq rgx (vl-catch-all-apply 'vlax-get-or-create-object '("vbscript.regexp"))))
                (vl-catch-all-error-p rgx)
            )
            (prompt (strcat "\nUnable to interface with RegExp object: " (vl-catch-all-error-message rgx)))
        )
        (   t
            (vlax-put-property rgx 'global     actrue)
            (vlax-put-property rgx 'ignorecase acfalse)
            (vlax-put-property rgx 'multiline  actrue)
            (setq copyswaptext:regexobject rgx)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:unformat ( str mtx / rtn )
    (if
        (null
            (vl-catch-all-error-p
                (setq rtn
                    (vl-catch-all-apply
                       '(lambda nil
                            (foreach pair
                                (if mtx
                                   '(
                                        ("\032"     . "\\\\\\\\")
                                        (" "        . "\\\\P|\\n|\\t")
                                        ("$1"       . "\\\\(\\\\[ACcFfHKkLlOopQTW])|\\\\[ACcFfHKkLlOopQTW][^\\\\;]*;|\\\\[ACcFfKkHLlOopQTW]")
                                        ("$1$2/$3"  . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                        ("$1$2"     . "\\\\(\\\\S)|[\\\\](})|}")
                                        ("$1"       . "[\\\\]({)|{")
                                    )
                                   '(
                                        ("\032"     . "\\\\")
                                        (""         . "%%[OoUu]")
                                    )
                                )
                                (vlax-put-property (copyswaptext:regex) 'pattern (cdr pair))
                                (setq str (vlax-invoke (copyswaptext:regex) 'replace str (car pair)))
                            )
                            (mapcar
                               '(lambda ( lst / tmp )
                                    (setq tmp str)
                                    (foreach pair lst
                                        (vlax-put-property (copyswaptext:regex) 'pattern (cdr pair))
                                        (setq tmp (vlax-invoke (copyswaptext:regex) 'replace tmp (car pair)))
                                    )
                                    tmp
                                )
                               '(
                                    (
                                        ("\\$1$2$3" . "(\\\\[ACcFfHKkLlOoPpQSTW])|({)|(})")
                                        ("\\\\"     . "\032")
                                    )
                                    (
                                        ("\\"       . "\032")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        rtn
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:startundo ( doc )
    (copyswaptext:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun copyswaptext:acdoc nil
    (eval (list 'defun 'copyswaptext:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (copyswaptext:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: CopySwapText.lsp | Version 1.7 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"ctx\" to Copy | \"stx\" to Swap ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;