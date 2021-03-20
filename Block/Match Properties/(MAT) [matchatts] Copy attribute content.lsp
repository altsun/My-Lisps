;;;CADALYST 01/07	Tip 2175: matchatts.LSP		Attribute Match		(c) 2007 Will DeLoach 

;;;---------------------------------------------------------------------;;;
;;;  MatchAtts.lsp                                                      ;;;
;;;  Created by Will DeLoach                                            ;;;
;;;  Copyright 2005                                                     ;;;
;;;                                                                     ;;;
;;;FUNCTION                                                             ;;;
;;;Creates a variable sized dialog box with attribute tagstrings from the;;
;;;the Source Attribute.  The user selects the values they want to match;;;
;;;and then selects blocks withe same name as the source block and the  ;;;
;;;selected attributes will be changed to match the source attribute.   ;;;
;;;                                                                     ;;;
;;;USAGE                                                                ;;;
;;;(load "matchatts)                                                    ;;;
;;;matchatts                                                            ;;;
;;;                                                                     ;;;
;;;PLATFORMS                                                            ;;;
;;;Tested on 2006; but could work for earlier versions.                 ;;;
;;;                                                                     ;;;
;;;VERSION                                                              ;;;
;;; 1.0   October 3, 2005                                               ;;;
;;; 1.01  October 4, 2005   Fixed Att Selection for Attributes only.    ;;;
;;; 1.02  October 5, 2005   Fixed bug in cond statement                 ;;;
;;; 1.03  November 15, 2005   Replaced get_att SUBR (recmd by T. Willey);;;
;;; 1.1   April 6, 2006   Modifications made by CAB from theSwamp.org   ;;;
;;;                       Fixed duplicate tagname bug.                  ;;;
;;; 1.11  Added the write_line function.                                ;;;
;;; 1.2   Fixed block selection to work with Dynamic Blocks.            ;;;
;;; 1.21  Corrected an error with 'EffectiveName' assumption.           ;;;
;;;                                                                     ;;;
;;;                                                                     ;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED      ;;;
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR   ;;;
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.             ;;;
;;;                                                                     ;;;
;;;  You are hereby granted permission to use, copy and modify this     ;;;
;;;  software without charge, provided you do so exclusively for        ;;;
;;;  your own use or for use by others in your organization in the      ;;;
;;;  performance of their normal duties, and provided further that      ;;;
;;;  the above copyright notice appears in all copies and both that     ;;;
;;;  copyright notice and the limited warranty and restricted rights    ;;;
;;;  notice below appear in all supporting documentation.               ;;;
;;;                                                                     ;;;
;;;---------------------------------------------------------------------;;;
(vl-load-com)
(defun c:MAT (/	   blk
 ;The entity name for the Reference Attribute
		    eblk ;Entity list of blk
		    atts ;List of attributes for 'blk'
		    tags ;List of 'tagstrings' for atts
		    keys ;List of 'handle' for atts
		    vals ;List of 'textstring' for atts
		    dcl_id ;Id number for the dialog box
		    dcl_name ;Name for the dialog box
		    strt ;Id from the done_dialog call
		    lst ;List of values for the toggles
		    ssa ;List of attributes the user selects
		    morder ;flag to chack att order, for future use
		    ename ;entity name
		    lst ;List of Keys
		    rname ;Reference Block Name
		    i ;Increment
		   )
;;;---------------------------------------------------------------------;;;
;;;A rewrite of the entsel function.                                    ;;;
;;;---------------------------------------------------------------------;;;
  (defun ent_sel (msg / ent)
    (while (not ent)
      (cond ((setq ent (entsel msg)))
	    ((= (getvar "ErrNo") 7)
	     (princ "\nSelection missed.  Please try again.")
	    )
	    ((= (getvar "ErrNo") 52) (exit))
      )
    )
    ent
  )
;;;---------------------------------------------------------------------;;;
;;;This is a utility function to return Attribute References for the    ;;;
;;;supplied object or entity name.                                      ;;;
;;;---------------------------------------------------------------------;;;
  (defun get_atts (obj)
    (vlax-invoke (vlax-ename->vla-object obj) 'getattributes)
  )
;;;---------------------------------------------------------------------;;;
;;;This creates a variable dialog box that changes size based on how    ;;;
;;;many attributes are passed to it.  The dialog box is temporary and is;;;
;;;created on the fly.  It is stored in your Acad Temp path location.   ;;;
;;;---------------------------------------------------------------------;;;
  (defun createdialog (lst keys / num tfn fn wid cnt str)
    (defun write_line (lst file) (foreach x lst (write-line x file)))
    (setq num (length tags))
    (cond ((> num 24) (setq wid 4))
	  ((> num 12) (setq wid 3))
	  ((> num 6) (setq wid 2))
	  (T (setq wid 1))
    )
    (setq tfn (vl-filename-mktemp "match_attribute_value.dcl"))
    (setq fn (open tfn "w"))
    (write_line
      (list
	"temp : dialog { label = \"Match Attributes\";"
	": boxed_column { label = \"Select Attributes Values to Match: \";"
	": row {"
      )
      fn
    )
    (setq cnt 0)
    (repeat (1+ wid)
      (write-line ": column {" fn)
      (repeat (/ num wid)
	(if (setq str (nth cnt lst))
	  (progn (write-line
		   (strcat ": toggle { label = "
			   "\""
			   str
			   "\""
			   "; key = "
			   "\""
			   (nth cnt keys)
			   "\""
			   ";}"
		   )
		   fn
		 )
		 (setq cnt (1+ cnt))
	  )
	)
      )
      (write-line "}" fn)
    )
    (write_line
      (list
	"}"
	"spacer_1;"
	" : toggle { key = \"All\"; label = \"Select All\";}"
	"}"
	"ok_cancel;"
	"}"
       )
      fn
    )
    (close fn)
    tfn
  )
;;;---------------------------------------------------------------------;;;
;;;This is the action for 'Select All' Check Box.                       ;;;
;;;---------------------------------------------------------------------;;;
  (defun on_all_pick (lst / str)
    (if	(eq (get_tile "All") "0")
      (setq str "0")
      (setq str "1")
    )
    (mapcar '(lambda (x) (set_tile x str)) lst)
  )
;;;---------------------------------------------------------------------;;;
;;;This takes two lists and compares the two.  Every item in vals that  ;;;
;;;equals "1" is replace with the corresponding item in objs.           ;;;
;;;---------------------------------------------------------------------;;;
;;; CAB modified, create a list of pairs (tagname flag)  where flag 1 0 t/nil
  (defun create_list (vals objs)
    (vl-remove nil
	       (mapcar '(lambda	(x y)
			  (cons	y
				(if (eq x "1")
				  1
				  0
				)
			  )
			)
		       vals
		       objs
	       )
    )
  )
;;;---------------------------------------------------------------------;;;
;;;Receives a Selection Set and returns Entity Names                    ;;;
;;;---------------------------------------------------------------------;;;
  (defun ssnames (selection_set / num lst)
    (repeat (setq num (sslength selection_set))
      (setq num	(1- num)
	    lst	(cons (ssname selection_set num) lst)
      )
    )
    lst
  )
;;;---------------------------------------------------------------------;;;
;;;Accepts a Group code and an Entity List and returns the value.       ;;;
;;;---------------------------------------------------------------------;;;
  (defun dxf (gcode elist) (cdr (assoc gcode elist)))
;;;---------------------------------------------------------------------;;;
;;;---------------------------------------------------------------------;;;
;;;---------------------------------------------------------------------;;;
;;;Main program starts here.
  (while (not blk)
    (setq blk (car (ent_sel "\nSelect Source Object:  ")))
    (cond ((not (setq eblk (entget blk))))
	  ((not (eq (dxf 0 eblk) "INSERT"))
	   (setq blk nil)
	   (princ "\nPlease select a block with attributes. ")
	  )
	  ((not (eq (dxf 66 eblk) 1))
	   (setq blk nil)
	   (princ "\nPlease select a block with attributes. ")
	  )
	  (T blk)
    )
  )
  (setq	atts (get_atts blk)
	tags (mapcar 'vla-get-tagstring atts)
	keys (mapcar 'vla-get-handle atts)
	vals (mapcar 'vla-get-textstring atts)
  )
  (setvar "ErrNo" 0)
  (and ; Creates a temporary dialog box with toggles for each
 ; of the tagstrings in tags.  This is a variable dialog
 ; box that changes size based on the number of attributes.
    (setq dcl_name (createdialog tags keys)) ; Loads the dialog box
    (> (setq dcl_id (load_dialog dcl_name)) 0)
    (new_dialog "temp" dcl_id)
 ; If any toggle other than 'Select All' is pressed then
 ; this will turn off the 'Select All' toggle.
    (mapcar '(lambda (x) (action_tile x "(set_tile \"All\" \"0\")"))
	    keys
    )
    (action_tile "All" "(on_all_pick keys)")
    (action_tile "cancel" "(done_dialog 0)")
    (action_tile
      "accept"
      "(setq lst (mapcar '(lambda (x)(get_tile x)) keys))(done_dialog 1)"
      ;;  CAB removed morder code
      ;; "(setq morder (= (get_tile \"morder\") \"1\")lst (mapcar '(lambda (x)(get_tile x)) keys))(done_dialog 1)"
    )
    ;;  (set_tile "morder" "1")
    (setq strt (start_dialog))
    (not (unload_dialog dcl_id))
    (vl-file-delete dcl_name)
    (setq morder t) ; overridefor now, keep for future use
    (if	(eq strt 1)
      (and ; creates a list of attribute objects from the
 ; reference object that were selected in the dialog box.
	;;  CAB start =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	;;  replace the attriburtes in one insert
	;;  if morder is true the tag and the order in the list must match
	;;  else the first matching tag will get the new value
	(defun replaceatts (ent atlst tags vals / blkobj cnt idx)
	  (setq	blkobj (vlax-ename->vla-object ent)
		cnt    0
	  )
	  (foreach atto	(vlax-invoke blkobj 'getattributes)
	    (cond
	      ((and morder ; order must match
		    (= 1 (cdr (nth cnt atlst))) ; flag, ok to replace
		    (eq (vla-get-tagstring atto) (nth cnt tags))
	       )
	       (vla-put-textstring atto (nth cnt vals))
	      )
	      ((and
		 (not morder) ; ignore order
		 (= 1 (cdr (nth cnt atlst))) ; flag, ok to replace
		 (setq idx (vl-position (vla-get-tagstring atto) tags))
	       )
	       (vla-put-textstring atto (nth idx vals))
	      )
	    )
	    (setq cnt (1+ cnt))
	  )
	  (vla-update blkobj)
	)
	;; CAB - a list of pairs (tagname flag)  where flag 1 0 t/nil
	(setq atts (create_list lst atts))
	(not
	  (prompt
	    "\nSelect blocks to update or enter ALL for all matching blocks."
	  )
	)
	(if
	  (and
	    (setq ssa (ssget (list '(0 . "INSERT") '(66 . 1))))
	    (setq rname (vlax-ename->vla-object blk))
	    (if	(vlax-property-available-p rname 'effectivename)
	      (progn (setq rname (vla-get-effectivename rname))
		     (mapcar '(lambda (x)
				(if (/=	rname
					(vla-get-effectivename
					  (vlax-ename->vla-object x)
					)
				    )
				  (ssdel x ssa)
				)
			      )
			     (ssnames ssa)
		     )
	      )
	      (progn
		(setq rname (vla-get-name rname))
		(mapcar
		  '(lambda (x)
		     (if (/= rname
			     (vla-get-name (vlax-ename->vla-object x))
			 )
		       (ssdel x ssa)
		     )
		   )
		  (ssnames ssa)
		)
	      )
	    )
	  )
	   (progn ;  itterate through the selected inserts
	     (setq i -1)
	     (while (setq ename (ssname ssa (setq i (1+ i))))
	       (if (not (eq ename blk)) ; ignore doner block
		 (replaceatts ename atts tags vals)
	       )
	     )
	     (if (= 0 (sslength ssa)) ;(not ssa);(null ssa)
	       (princ "\nInvalid blocks selected.  Please start over. ")
	     )
	   )
	)
	;;  CAB end =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ) ; This is returned if the user hits Cancel.
      (princ "\nFunction Terminated by User!")
    )
  )
  (princ)
)