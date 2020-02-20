;;  BlockSParts0Bylayer.lsp
;;  = change all Parts of definitions of Selected Block(s) [other
;;    than on Layer Defpoints] to Layer 0 with Color ByLayer
;;  Kent Cooper, last edited 4 November 2014

(vl-load-com)
(defun C:BP0B (/ *error* doc nametolist blkss inc blk blknames ent edata obj)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (vla-endundomark doc)
    (princ)
  ); defun - *error*

  (defun nametolist (blk / blkobj blkname); get Block name and put it into list of names
    (if (= (logand (cdr (assoc 70 (entget blk))) 4) 0) ; not an Xref
      (progn
        (setq
          blkobj (vlax-ename->vla-object blk)
          blkname
            (vlax-get-property blkobj
              (if (vlax-property-available-p blkobj 'EffectiveName) 'EffectiveName 'Name)
                ; to work with older versions that don't have dynamic Blocks
            ); ...get-property & blkname
        ); setq
        (if
          (not (member blkname blknames)); name not already in list
          (setq blknames (append blknames (list blkname))); then -- add to end of list
        ); if
      ); progn
    ); if
  ); defun -- nametolist

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc); = Undo Begin

  (if (setq blkss (ssget '((0 . "INSERT")))); User selection of any number of Blocks/Minserts/Xrefs
    (progn ; then
      (repeat (setq inc (sslength blkss)); get names from initial selection
        (setq blk (ssname blkss (setq inc (1- inc))))
        (nametolist blk)
      ); repeat
      (while (setq blk (car blknames)); as long as there's another Block name in list
        ;; done this way instead of via (repeat) or (foreach), so it can add nested Blocks' names to list
        (setq ent (tblobjname "block" blk)); Block definition as entity
        (while (setq ent (entnext ent)); then -- proceed through sub-entities in definition
          (setq edata (entget ent))
          (if (member '(0 . "INSERT") edata) (nametolist ent)); if nested Block, add name to end of list
          (if (not (member '(8 . "Defpoints") edata)); process all entities NOT on Layer Defpoints
            (progn
              (setq obj (vlax-ename->vla-object ent))
              (vla-put-layer obj "0"); to Layer 0
              (vla-put-color obj 256); color ByLayer
              (if (wcmatch (vla-get-ObjectName obj) "*Dimension,*Leader")
                (foreach prop '(DimensionLineColor ExtensionLineColor TextColor)
                  ;; not all such entity types have all 3 properties, but all have at least one
                  (if (vlax-property-available-p obj prop)
                    (vlax-put obj prop 256); ByLayer
                  ); if
                ); foreach
              ); if
            ); progn
          ); if
        ); while -- sub-entities
        (setq blknames (cdr blknames)); take first one off
      ); while
      (command "_.regen")
    ); progn
    (prompt "\nNo Block(s) selected.")
  ); if [user selection]

  (vla-endundomark doc); = Undo End
  (princ)
); defun