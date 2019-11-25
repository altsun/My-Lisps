;;-----------------=={ Copy/Rename Block Reference }==------------------;;
;;                                                                      ;;
;;  This program allows a user to copy and/or rename a single block     ;;
;;  reference in the working drawing.                                   ;;
;;                                                                      ;;
;;  Many existing programs enable the user to rename the block          ;;
;;  definition for a given block reference, with the new name           ;;
;;  subsequently reflected across all references of the block           ;;
;;  definition in the drawing. However, this program will allow a       ;;
;;  single selected block reference to be renamed (or for the user to   ;;
;;  create a renamed copy of the selected block reference), by          ;;
;;  generating a duplicate renamed block definition for the selected    ;;
;;  block.                                                              ;;
;;                                                                      ;;
;;  The program may be called from the command-line using either 'CB'   ;;
;;  to create a renamed copy of a selected block reference, or 'RB' to  ;;
;;  simply rename the selected block reference.                         ;;
;;                                                                      ;;
;;  Following selection of a block reference, the user is prompted to   ;;
;;  specify a name for the selected/copied block reference; a default   ;;
;;  block name composed of the original block name concatenated with    ;;
;;  both an underscore and the minimum integer required for uniqueness  ;;
;;  within the block collection of the active drawing is offered.       ;;
;;                                                                      ;;
;;  The program will then proceed to duplicate the block definition     ;;
;;  using the new block name. To accomplish this without resulting in   ;;
;;  a duplicate key in the block collection of the active drawing, the  ;;
;;  program utilises an ObjectDBX interface to which the block          ;;
;;  definition of the selected block reference is deep-cloned, renamed, ;;
;;  and then deep-cloned back to the active drawing. This method also   ;;
;;  enables Dynamic Block definitions to be successfully copied         ;;
;;  & renamed.                                                          ;;
;;                                                                      ;;
;;  Finally, this program will perform successfully in all UCS/Views    ;;
;;  and is compatible with Anonymous Blocks, Dynamic Blocks & XRefs.    ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    05-07-2013                                      ;;
;;----------------------------------------------------------------------;;

(defun c:cb nil (LM:RenameBlockReference   t))
(defun c:rb nil (LM:RenameBlockReference nil))

(defun LM:RenameBlockReference ( cpy / *error* abc app dbc dbx def doc dxf new old prp src tmp vrs )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn
            (setvar 'errno 0)
            (setq src (car (entsel (strcat "\nSelect block reference to " (if cpy "copy & " "") "rename: "))))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'ename (type src))
                    (setq dxf (entget src))
                    (cond
                        (   (/= "INSERT" (cdr (assoc 0 dxf)))
                            (princ "\nPlease select a block reference.")
                        )
                        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 dxf)))))))
                            (princ "\nSelected block is on a locked layer.")
                        )
                    )
                )
            )
        )
    )
    (if (= 'ename (type src))
        (progn
            (setq app (vlax-get-acad-object)
                  doc (vla-get-activedocument app)
                  src (vlax-ename->vla-object src)
                  old (vlax-get-property src (if (vlax-property-available-p src 'effectivename) 'effectivename 'name))
                  tmp 0
            )
            (while (tblsearch "block" (setq def (strcat (vl-string-left-trim "*" old) "_" (itoa (setq tmp (1+ tmp)))))))
            (while
                (and (/= "" (setq new (getstring t (strcat "\nSpecify new block name <" def ">: "))))
                    (or (not (snvalid new))
                        (tblsearch "block" new)
                    )
                )
                (princ "\nBlock name invalid or already exists.")
            )
            (if (= "" new)
                (setq new def)
            )
            (setq dbx
                (vl-catch-all-apply 'vla-getinterfaceobject
                    (list app
                        (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                            "objectdbx.axdbdocument"
                            (strcat "objectdbx.axdbdocument." (itoa vrs))
                        )
                    )
                )
            )
            (if (or (null dbx) (vl-catch-all-error-p dbx))
                (princ "\nUnable to interface with ObjectDBX.")
                (progn
                    (setq abc (vla-get-blocks doc)
                          dbc (vla-get-blocks dbx)
                    )
                    (vlax-invoke doc 'copyobjects (list (vla-item abc old)) dbc)
                    (if (wcmatch old "`**")
                        (vla-put-name (vla-item dbc (1- (vla-get-count dbc))) new)
                        (vla-put-name (vla-item dbc old) new)
                    )
                    (vlax-invoke dbx 'copyobjects (list (vla-item dbc new)) abc)
                    (vlax-release-object dbx)
                    (if cpy (setq src (vla-copy src)))
                    (if
                        (and
                            (vlax-property-available-p src 'isdynamicblock)
                            (= :vlax-true (vla-get-isdynamicblock src))
                        )
                        (progn
                            (setq prp (mapcar 'vla-get-value (vlax-invoke src 'getdynamicblockproperties)))
                            (vla-put-name src new)
                            (mapcar
                               '(lambda ( a b )
                                    (if (/= "ORIGIN" (strcase (vla-get-propertyname a)))
                                        (vla-put-value a b)
                                    )
                                )
                                (vlax-invoke src 'getdynamicblockproperties) prp
                            )
                        )
                        (vla-put-name src new)
                    )
                    (if (= :vlax-true (vla-get-isxref (setq def (vla-item (vla-get-blocks doc) new))))
                        (vla-reload def)
                    )
                    (if cpy (sssetfirst nil (ssadd (vlax-vla-object->ename src))))
                )
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: CopyRenameBlock.lsp | Version 1.5 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Available Commands:"
        "\n::    \"CB\"  -  Copy & Rename Block Reference."
        "\n::    \"RB\"  -  Rename Block Reference."
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;