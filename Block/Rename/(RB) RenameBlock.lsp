;;  RenameBlock.lsp [command name: RB]
;;  To Rename a selected Block to a User-specified new name, without
;;    needing to know its current name as with regular Rename command.
;;  Kent Cooper, last edited 24 April 2015

(defun C:RB (/ *error* getblk doc bent ss bobj old new cmde); = Rename user-selected Block

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (if bent (redraw bent 4)); un-highlight
    (vla-endundomark doc)
    (princ)
  ); defun - *error*

  (defun getblk ()
    (while (not bent)
      (setq bent (car (entsel "\nSelect Block to Rename: ")))
    ); while
  ); defun -- getblk

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)

  (while
    (not
      (and
        (cond ; object selection
          ( (and
              (setq ss (ssget "_I" '((0 . "INSERT")))); something pre-selected including Block(s)
              (= (sslength ss) 1); only one Block object
            ); and
            (setq bent (ssname ss 0))
          ); first condition [single pre-selected object]
          ( (ssget "_I"); more than one object selected
            (sssetfirst nil); un-select multiple objects
            (getblk); User select
          ); second condition
          ((getblk)); User select [nothing pre-selected]
        ); cond
        (setq
          bobj (vlax-ename->vla-object bent)
          old
            (if (vlax-property-available-p bobj 'effectivename); dynamic?
              (vla-get-EffectiveName bobj)
              (vla-get-Name bobj)
            ); if & old
        ); setq
        (= (vla-get-ObjectName bobj) "AcDbBlockReference")
        (not (assoc 1 (tblsearch "block" old))); not an Xref
      ); and
    ); not
    (setq bent nil); clear for next try
    (prompt "\nNothing selected, or not a Block.")
  ); while
  (sssetfirst nil); un-select/un-highlight any pre-selected objects in addition to Block
  (redraw bent 3); highlight Block [only]
  (prompt "\nEnter new Block name in Text Editor [do not use Full Editor].")
  (princ); [somehow needed for prompt to show at Command: line before (lisped) comes up]
  (while
    (not
      (and
        (setq new (lisped old)); can keep current name with just Enter
        (setq new (vl-string-trim " " new)); remove any leading/trailing space(s)
        (/= new ""); didn't either clear out or enter only space(s)
        (or (= new old) (not (tblsearch "block" new))); name kept or not already in use
      ); and
    ); not
    (alert
      (cond
        ((= new "") "Empty string or only space(s) not allowed;\nyou must supply a Block name.")
        ((/= new old) "\nThat Block name is already in use.")
      ); cond
    ); alert
  ); while
  (if (/= new old); User entered a new name [didn't keep original]
    (progn ; then
      (setq cmde (getvar 'cmdecho))
      (setvar 'cmdecho 0)
      (command "_.rename" "_block" old new)
      (setvar 'cmdecho cmde)
      (prompt (strcat "\nBlock \"" old "\" renamed \"" new "\"."))
    ); progn
    (prompt (strcat "\nBlock name \"" old "\" retained.")); else
  ); if
  (redraw bent 4); un-highlight

  (vla-endundomark doc)
  (princ)
); defun

(vl-load-com)
(prompt "\nType RB to Rename a Block.")
(princ)