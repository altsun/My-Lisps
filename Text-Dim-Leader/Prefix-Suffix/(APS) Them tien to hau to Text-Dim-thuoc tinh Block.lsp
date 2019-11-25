;; http://www.cadviet.com/forum/topic/93259-hoi-ve-ham-trong-lisp/
;----- Add/Change Prefix and/or Suffix for DIMENSION, TEXT, MTEXT, ATTDEF. De tim hieu code >> nen mo trong Notepad++.
;----- Doan Van Ha - CadMagic - Ver.1: 15/9/2013
(vl-load-com)
(defun C:APS ( / dial flag lstobj lstkey lstvar fn Add_Prefix_Suffix SelectObj Ss->Lst Old_APS New_APS HA:SetVal Make_File_Dcl)
;----- Sub Functions
 (defun Add_Prefix_Suffix (lst pre suf add)	;Add Prefix vµ/hoÆc Suffix cho lstobj.
  (command "undo" "be")
  (if (and lst pre suf)
   (mapcar
   '(lambda (obj / typ txt pre1 suf1)
     (setq typ (cdr (assoc 0 (entget (vlax-vla-object->ename obj)))))
     (cond
      ((wcmatch typ "MTEXT,TEXT") (vla-put-TextString obj (strcat pre (vla-get-TextString obj) suf)))	;MultiLeader ???
      ((wcmatch typ "ATTDEF") (vla-put-TagString obj (strcat pre (vla-get-TagString obj) suf)))
      ((wcmatch typ "DIMENSION")
	   (setq txt (cdr (assoc 1 (entget (vlax-vla-object->ename obj)))) pre1 (vla-get-TextPrefix obj) suf1 (vla-get-TextSuffix obj))
       (cond
        ((and (= txt "") (= add "0"))																; Nguyen thuy hoac da add pre/suf: Change
		 (vla-put-TextPrefix obj pre) (vla-put-TextSuffix obj suf))								
		((and (= txt "") (= add "1"))																; Nguyen thuy hoac da add pre/suf: Add
		 (vla-put-TextPrefix obj (strcat pre pre1)) (vla-put-TextSuffix obj (strcat suf1 suf)))		
 	    (T																						; Override: Add (not Change)
		 (vla-put-TextOverride obj (strcat pre txt suf))))))													
     (vlax-release-object obj))
    lst))
  (command "undo" "e"))
 (defun SelectObj (lstvar lstkey / txt lst)	;Chän ®èi t­îng.
  (setq txt (apply 'strcat (mapcar '(lambda(var key) (if (= var "1") (strcat key ",") "")) (mapcar 'eval lstvar) lstkey)))
  (setq lst (Ss->Lst (ssget (list (cons 0 txt))) T)))
 (defun Ss->Lst (ss flag / lst)	;Convert selection set to list vla-object
  (and ss (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
   (if flag (setq lst (mapcar 'vlax-ename->vla-object lst))))
  lst)
 (defun Old_APS()	;®Æt chÕ ®é nh­ cò.
  (setq text_old text mtext_old mtext attdef_old attdef dimension_old dimension pre_old pre suf_old suf add_old add change_old change))
 (defun New_APS()	;®Æt chÕ ®é míi nh­ cò".
  (setq text text_old mtext mtext_old attdef attdef_old dimension dimension_old pre pre_old suf suf_old add add_old change change_ols))
 (defun HA:SetVal (lstkey lstvar lstval)	;Set value_default or set value_old for var + Set_tile for key. EX: (HA:SetVal '("key1" "key2" "key3") '(var1 var2 var3) '("0" "1" "a"))
  (mapcar '(lambda (var val) (if (not (eval var)) (set var val))) lstvar lstval)
  (mapcar '(lambda (key val) (set_tile key (set (read key) val))) lstkey (mapcar 'eval lstvar)))
 (defun Make_File_Dcl ( / fn ow dial) 
  (setq fn (vl-filename-mktemp "APS" nil ".dcl"))
  (setq ow (open fn "w"))
  (mapcar
  '(lambda (x) (write-line x ow))
   (list
"APS : dialog { label = \"CadMagic - Add prefix and suffix for objects\";"
" : boxed_column { label = \"Set variable\";"
"    : row {"
"      : boxed_column { label = \"Dimension\";"
"        : toggle { label = \"Dimension\"; key = \"dimension\"; }"
"        : radio_row {"
"          : radio_button { label = \"Add\";  key = \"add\"; }"
"          : radio_button { label = \"Change\";  key = \"change\"; }"
"        }"
"      }"
"      : boxed_row { label = \"Text/Mtext/Attdef\";"
"        : toggle { label = \"Text\"; key = \"text\"; }"
"        : toggle { label = \"Mtext\"; key = \"mtext\"; }"
"        : toggle { label = \"Attdef\"; key = \"attdef\"; }"
"      }"
"    }"
"    : column {"
"      : edit_box { label = \"Prefix:\"; key = \"pre\"; edit_width = 45; }"
"      : edit_box { label = \"Suffix:\"; key = \"suf\"; edit_width = 45; }"
"    }"
"	: button { label = \"Select objects\"; key = \"chon\"; fixed_width = true; alignment = centered; }"
"  }"
"  ok_cancel;"
"}"))
  (close ow)
  fn)
;----- Main Function.
 (setq dial (load_dialog (setq fn (Make_File_Dcl))) flag 3)
 (while (> flag 1)
  (if (not (new_dialog "APS" dial)) (exit))
  (Old_APS)
  (HA:SetVal (setq lstkey '("text" "mtext" "attdef" "dimension" "pre" "suf" "add" "change"))
            (setq lstvar '(text mtext attdef dimension pre suf add change)) '("0" "0" "0" "0" "Prefix" "Suffix" "1" "0"))
  (action_tile "text" "(setq text $value)")
  (action_tile "mtext" "(setq mtext $value)")
  (action_tile "attdef" "(setq attdef $value)")
  (action_tile "dimension" "(setq dimension $value)")
  (action_tile "pre" "(setq pre $value)")
  (action_tile "suf" "(setq suf $value)")
  (action_tile "add" "(setq add $value change \"0\")")
  (action_tile "change" "(setq change $value add \"0\")")
  (action_tile "Cancel" "(done_dialog 0)")
  (action_tile "Accept" "(done_dialog 1)")
  (action_tile "chon" "(done_dialog 2)")
  (setq flag (start_dialog))
  (cond ((= 0 flag) (New_APS))
        ((= 2 flag) (setq lstobj (SelectObj lstvar lstkey)))
        ((= 1 flag) (Add_Prefix_Suffix lstobj pre suf add))))
 (unload_dialog dial) (vl-file-delete fn) (princ))
;--------------------------------------------------------------------------------------------------------------------------------------