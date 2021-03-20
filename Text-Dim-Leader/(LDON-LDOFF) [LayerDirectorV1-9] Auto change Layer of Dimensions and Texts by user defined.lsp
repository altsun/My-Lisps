;;------------------------=={ Layer Director }==------------------------;;
;;                                                                      ;;
;;  This program uses a Visual LISP Command Reactor to automatically    ;;
;;  set the current layer when a command is called, proceeding to       ;;
;;  reset to the previous current layer when the command is completed.  ;;
;;                                                                      ;;
;;  The chosen layer will be created with the specified properties if   ;;
;;  it doesn't already exist in the active drawing.                     ;;
;;                                                                      ;;
;;  The data controlling the layer to be set when certain commands are  ;;
;;  called and the corresponding layer properties for new layers is     ;;
;;  tabulated at the top of the code - this may be altered and expanded ;;
;;  to suit the user's requirements.                                    ;;
;;                                                                      ;;
;;  The first item of each list in the table is the name of a command   ;;
;;  to trigger a layer change. This command name should be the full     ;;
;;  command name, not a command alias. The command is not               ;;
;;  case-sensitive and may use wildcards.                               ;;
;;                                                                      ;;
;;  To give a few examples, "[DM]TEXT,TEXT" will cue a layer change     ;;
;;  for the Text, DText and MText commands; "[QM]LEADER,LEADER" will    ;;
;;  cue a layer change for the Leader, QLeader and MLeader commands.    ;;
;;                                                                      ;;
;;  The second item is the name of the layer to be set to current when  ;;
;;  the command is called. This layer will be created if not present    ;;
;;  in the active drawing, using the layer properties specified in the  ;;
;;  remainder of the list.                                              ;;
;;                                                                      ;;
;;  The program will automatically enable the Command Reactor when      ;;
;;  loaded, and the current layer will be automatically changed when    ;;
;;  any of the listed commands are called. The reactor may be           ;;
;;  subsequently disabled or enabled manually using the commands        ;;
;;  'LDOFF' & 'LDON' respectively.                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2011  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-04-16                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2012-04-24                                      ;;
;;                                                                      ;;
;;  - Program rewritten & updated.                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-06-15                                      ;;
;;                                                                      ;;
;;  - Added the ability to specify basic layer properties to be         ;;
;;    applied to new layers created by the program.                     ;;
;;  - Changed loading method to use the s::startup post-initialisation  ;;
;;    loading function.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2015-12-02                                      ;;
;;                                                                      ;;
;;  - Added layerdirector:forcelayprops parameter to control whether    ;;
;;    the properties of existing layers are modified by the program.    ;;
;;  - Added layerdirector:sysvars parameter to allow the user to        ;;
;;    automatically configure other system variable values when the     ;;
;;    layer change is triggered.                                        ;;
;;  - Added layer description to Layer Data list.                       ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2016-01-10                                      ;;
;;                                                                      ;;
;;  - Added the ability to cue a layer change following the use of a    ;;
;;    custom AutoLISP command.                                          ;;
;;  - Added Plot Style property to Layer Data list.                     ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2016-05-09                                      ;;
;;                                                                      ;;
;;  - Added xref-dependent layering option to enable the user to        ;;
;;    automatically insert external references on a layer with the      ;;
;;    same name as the xref, with an optional prefix & suffix.          ;;
;;  - LM:loadlinetypes function replaced with the more efficient        ;;
;;    dedicated function LM:layerdirector:loadlinetype.                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2016-05-10                                      ;;
;;                                                                      ;;
;;  - Xref-dependent layering option modified to account for the use    ;;
;;    of the CLASSICXREF command.                                       ;;
;;----------------------------------------------------------------------;;
;;  Version 1.7    -    2017-04-02                                      ;;
;;                                                                      ;;
;;  - Xref-dependent layering option modified to account for the        ;;
;;    simultaneous insertion of multiple Xrefs.                         ;;
;;----------------------------------------------------------------------;;
;;  Version 1.8    -    2017-04-08                                      ;;
;;                                                                      ;;
;;  - Program updated to account for the use of the 3DORBIT command     ;;
;;    invoked transparently from within a command which has been        ;;
;;    configured to trigger a layer change.                             ;;
;;  - Added CETRANSPARENCY to the default system variable list, to be   ;;
;;    set to -1 (ByLayer) when a layer change is triggered.             ;;
;;----------------------------------------------------------------------;;
;;  Version 1.9    -    2018-12-23                                      ;;
;;                                                                      ;;
;;  - Added layerdirector:lspcommand parameter to control whether a     ;;
;;    layer change can be triggered by commands invoked by an AutoLISP  ;;
;;    program.                                                          ;;
;;----------------------------------------------------------------------;;

(setq

;;----------------------------------------------------------------------;;
;;  Layer Data                                                          ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  Populate this list with commands for which the current layer        ;;
;;  should be changed.                                                  ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  COMMAND PATTERN                                                     ;;
;;  -----------------------------------                                 ;;
;;  The first item is the name of a standard AutoCAD command or custom  ;;
;;  AutoLISP command that will cue a layer change. The command name     ;;
;;  should be the full command name, not an alias. This command name    ;;
;;  is not case-sensitive and may use wildcards.                        ;;
;;                                                                      ;;
;;  e.g. "[DM]TEXT,TEXT" will cue a layer change for the TEXT, DTEXT    ;;
;;  and MTEXT commands.                                                 ;;
;;                                                                      ;;
;;  e.g. "LINE,[XP]LINE" will cue a layer change for the LINE, PLINE,   ;;
;;  and XLINE commands.                                                 ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER NAME                                                          ;;
;;  -----------------------------------                                 ;;
;;  The second item is the name of the layer to be set current when     ;;
;;  the command is called. This layer will be created if not present    ;;
;;  in the active drawing.                                              ;;
;;                                                                      ;;
;;  The remaining items are basic layer properties to be applied to     ;;
;;  layers that are created by the program.                             ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER DESCRIPTION                                                   ;;
;;  -----------------------------------                                 ;;
;;  The third item is an optional layer description; this item may be   ;;
;;  an arbitrary string value, or an empty string for no description.   ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER COLOUR                                                        ;;
;;  -----------------------------------                                 ;;
;;  The fourth item is the layer colour and should be a positive        ;;
;;  non-zero integer less than 256, representing the ACI colour of      ;;
;;  the layer.                                                          ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER LINETYPE                                                      ;;
;;  -----------------------------------                                 ;;
;;  The fifth item is the layer linetype; this is a non-case-sensitive  ;;
;;  string representing a linetype defined in a .lin file, or           ;;
;;  "Continuous" if the layer should use the default continuous         ;;
;;  linetype. The program will attempt to load specified linetypes      ;;
;;  which are not defined in the drawing, and will use the Continuous   ;;
;;  linetype if the specified linetype cannot be loaded.                ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER LINEWEIGHT                                                    ;;
;;  -----------------------------------                                 ;;
;;  The sixth item is the layer lineweight; this should be an integer   ;;
;;  representing one of the standard lineweight values multiplied by    ;;
;;  100 (i.e. 2.11mm becomes 211). Use -3 to specify the 'Default'      ;;
;;  lineweight.                                                         ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER PLOT FLAG                                                     ;;
;;  -----------------------------------                                 ;;
;;  The seventh item is the layer plot flag; this should be an integer  ;;
;;  value of either 1 or 0. A value of 1 indicates that the layer       ;;
;;  should be plotted; a value of 0 means the layer will not plot.      ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  LAYER PLOT STYLE                                                    ;;
;;  -----------------------------------                                 ;;
;;  Finally, for STB drawings (PSTYLEMODE=0), this property may be used ;;
;;  to specify the Plot Style to be associated with each layer. This    ;;
;;  value should either be a string corresponding to the name of a      ;;
;;  Plot Style, or nil to use the default Plot Style or for             ;;
;;  CTB drawings (PSTYLEMODE=1).                                        ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;

layerdirector:data
   '(
                           
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
;;  Command Pattern  |  Layer Name    |       Description       |    Colour    |   Linetype   |    Lineweight    |       Plot       |    Plot Style    ;;
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
;;     [string]      |   [string]     |         [string]        | 0 < int <256 |   [string]   | -3 = Default     |  1 = Will Plot   |     [string]     ;;
;;                   |                |     Use "" for none     |              |              |  0 <= int <= 211 |  0 = Won't Plot  |  Use nil for CTB ;;
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;

("[DM]TEXT,TEXT"       "LD-GHI CHU"           "Text Layer"                   81        "Continuous"           -3                 1                 nil         )
("DIM*,*LEADER"        "LD-KICH THUOC"     "Dimension Layer"              21        "Continuous"           -3                 1                 nil         )
("*VPORT*"             "DEFPOINTS"      ""                             7        "Continuous"           -3                 0                 nil         )
("XLINE"               "XLINE"          "Construction Lines"          12        "HIDDEN"                0                 0                 nil         )

;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;

    )

;;----------------------------------------------------------------------;;
;;  Force Layer Properties  [ t / nil ]                                 ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  If set to T the properties of existing layers will be modified to   ;;
;;  match those found in the Layer Data list above.                     ;;
;;----------------------------------------------------------------------;;

layerdirector:forcelayprops nil

;;----------------------------------------------------------------------;;
;;  System Variable Settings                                            ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  Populate this list with system variables whose values should be     ;;
;;  automatically changed when a layer change is triggered.             ;;
;;                                                                      ;;
;;  The first item should be a symbol or string corresponding to the    ;;
;;  name of a system variable; the second item should represent the     ;;
;;  value to which the system variable should be set when a layer       ;;
;;  change is triggered.                                                ;;
;;                                                                      ;;
;;  This parameter is optional: remove all list items if no system      ;;
;;  variable changes are to be performed.                               ;;
;;----------------------------------------------------------------------;;

layerdirector:sysvars
   '(

;;---------------------------------;;
;;  System Variable  |    Value    ;;
;;---------------------------------;;

(cecolor              "bylayer")
(celtype              "bylayer")
(celweight               -1    ) ;; -1 = bylayer
(cetransparency          -1    ) ;; -1 = bylayer

;;----------------------------------------------------------------------;;

    )

;;----------------------------------------------------------------------;;
;;  XRef-Dependent Layering                                             ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  This option will cause external references (xrefs) to be inserted   ;;
;;  on a layer whose layer name is dependent on the name of the xref.   ;;
;;                                                                      ;;
;;  The first and second items in the below list represent an optional  ;;
;;  prefix and suffix which will surround the xref name in the name of  ;;
;;  the layer generated by the program.                                 ;;
;;                                                                      ;;
;;  The remaining items in the list determine the properties of the     ;;
;;  layers generated by the program for each xref; the order and        ;;
;;  permitted values of such properties are identical to those used by  ;;
;;  the Layer Data list above.                                          ;;
;;                                                                      ;;
;;  To disable this option, simply replace the below list with nil.     ;;
;;----------------------------------------------------------------------;;

layerdirector:xreflayer

;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
;;   Layer Prefix   |  Layer Suffix   |       Description       |    Colour    |   Linetype   |    Lineweight    |       Plot       |    Plot Style    ;;
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;
;;    [string]      |    [string]     |         [string]        | 0 < int <256 |   [string]   | -3 = Default     |  1 = Will Plot   |     [string]     ;;
;; Use "" for none  | Use "" for none |     Use "" for none     |              |              |  0 <= int <= 211 |  0 = Won't Plot  |  Use nil for CTB ;;
;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;

'("XREF-"             ""               "XRef Layer"                   250        "Continuous"           -3                 1                 nil        )

;;-----------------------------------------------------------------------------------------------------------------------------------------------------;;

;;----------------------------------------------------------------------;;
;;  Layer Change on AutoLISP Command Calls  [ t / nil ]                 ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  If set to T, a layer change may be triggered by command calls used  ;;
;;  in AutoLISP programs. If set to nil, a layer change will only be    ;;
;;  triggered by the AutoLISP command itself, and not by commands       ;;
;;  invoked during evaluation of the AutoLISP program.                  ;;
;;----------------------------------------------------------------------;;

layerdirector:lspcommand nil

;;----------------------------------------------------------------------;;
;;  Print Command (Debug Mode)  [ t / nil ]                             ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  If set to T the program will print the command name when a command  ;;
;;  is called. This is useful when determining the correct command name ;;
;;  to use in the Layer Data list.                                      ;;
;;----------------------------------------------------------------------;;

layerdirector:printcommand nil

)

;;----------------------------------------------------------------------;;
;;  Commands:  [ LDON / LDOFF ]                                         ;;
;;  ==================================================================  ;;
;;                                                                      ;;
;;  Use these to manually turn the Layer Director on & off.             ;;
;;----------------------------------------------------------------------;;

(defun c:ldon  nil (LM:layerdirector  t ))
(defun c:ldoff nil (LM:layerdirector nil))

;;----------------------------------------------------------------------;;

(if layerdirector:sysvars
    (setq layerdirector:sysvars
        (apply 'mapcar
            (cons 'list
                (vl-remove-if-not '(lambda ( x ) (getvar (car x)))
                    layerdirector:sysvars
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector ( on )
    (foreach grp (vlr-reactors :vlr-command-reactor :vlr-lisp-reactor)
        (foreach obj (cdr grp)
            (if (= "LM:layerdirector" (vlr-data obj))
                (vlr-remove obj)
            )
        )
    )
    (or
        (and on
            (vlr-command-reactor "LM:layerdirector"
               '(
                    (:vlr-commandwillstart . LM:layerdirector:set)
                    (:vlr-commandended     . LM:layerdirector:reset)
                    (:vlr-commandcancelled . LM:layerdirector:reset)
                    (:vlr-commandfailed    . LM:layerdirector:reset)
                )
            )
            (vlr-lisp-reactor "LM:layerdirector"
               '(
                    (:vlr-lispwillstart . LM:layerdirector:set)
                    (:vlr-lispended     . LM:layerdirector:reset)
                    (:vlr-lispcancelled . LM:layerdirector:reset)
                )
            )
            (princ "\nLayer Director enabled.")
        )
        (princ "\nLayer Director disabled.")
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:lispcommand ( str )
    (if (wcmatch str "(C:*)") (substr str 4 (- (strlen str) 4)) str)
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:set ( obj arg / lst tmp )
    (if (and (or layerdirector:lspcommand (not layerdirector:lspflg))
             (setq arg (car arg))
             (setq arg (LM:layerdirector:lispcommand (strcase arg)))
             (not (wcmatch arg "U,UNDO,NUDGE,3DORBITTRANSPARENT,SETVAR"))
        )
        (progn
            (setq layerdirector:lspflg (= ':vlr-lispwillstart (vlr-current-reaction-name)))
            (if (and (setq lst (cdar (vl-member-if '(lambda ( x ) (wcmatch arg (strcase (car x)))) layerdirector:data)))
                     (setq tmp (LM:layerdirector:createlayer lst))
                     (zerop (logand 1 (cdr (assoc 70 tmp))))
                )
                (progn
                    (setq layerdirector:oldlayer (cons (getvar 'clayer) layerdirector:oldlayer)
                          layerdirector:oldvars  (cons (mapcar 'getvar (car layerdirector:sysvars)) layerdirector:oldvars)
                    )
                    (if layerdirector:sysvars
                        (apply 'mapcar (cons 'setvar layerdirector:sysvars))
                    )
                    (setvar 'clayer (car lst))
                )
            )
            (if (and (= 'list (type layerdirector:xreflayer)) (wcmatch arg "XATTACH,CLASSICXREF"))
                (setq layerdirector:entlast (LM:layerdirector:entlast))
            )
            (if layerdirector:printcommand (print arg))
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:reset ( obj arg / ent lay tmp var )
    (if (member (vlr-current-reaction-name) '(:vlr-lispended :vlr-lispcancelled))
        (setq layerdirector:lspflg nil)
    )
    (if (and (or layerdirector:lspcommand (not layerdirector:lspflg))
             (or (null (car arg)) (not (wcmatch (strcase (car arg)) "U,UNDO,NUDGE,3DORBITTRANSPARENT,SETVAR")))
        )
        (progn
            (if (= 'list (type layerdirector:oldlayer))
                (setq lay (car layerdirector:oldlayer)
                      layerdirector:oldlayer (cdr layerdirector:oldlayer)
                )
                (setq layerdirector:oldlayer nil)
            )
            (if
                (and
                    (= 'str (type lay))
                    (setq tmp (tblsearch "layer" lay))
                    (zerop (logand 1 (cdr (assoc 70 tmp))))
                )
                (setvar 'clayer lay)
            )
            (if (= 'list (type layerdirector:oldvars))
                (setq var (car layerdirector:oldvars)
                      layerdirector:oldvars (cdr layerdirector:oldvars)
                )
                (setq layerdirector:oldvars nil)
            )
            (if (= 'list (type var))
                (mapcar 'setvar (car layerdirector:sysvars) var)
            )
            (if
                (and
                    (car arg)
                    (= 'list (type layerdirector:xreflayer))
                    (wcmatch (strcase (car arg)) "XATTACH,CLASSICXREF")
                )
                (if (= 'ename (type (setq ent layerdirector:entlast)))
                    (while (setq ent (entnext ent)) (LM:layerdirector:xreflayer ent layerdirector:xreflayer))
                    (LM:layerdirector:xreflayer (entlast) layerdirector:xreflayer)
                )
            )
            (setq layerdirector:entlast nil)
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:xreflayer ( ent lst / enx lay obj xrf )
    (if
        (and
            (setq enx (entget ent))
            (= "INSERT" (cdr (assoc 0 enx)))
            (setq xrf   (cdr (assoc 2 enx))
                  lay   (strcat (car lst) xrf (cadr lst))
            )
            (= 4 (logand 4 (cdr (assoc 70 (tblsearch "block" xrf)))))
            (LM:layerdirector:createlayer (cons lay (cddr lst)))
            (setq obj (vlax-ename->vla-object ent))
            (vlax-write-enabled-p obj)
        )
        (vla-put-layer obj lay)
    )
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:entlast ( / ent tmp )
    (setq ent (entlast))
    (while (setq tmp (entnext ent)) (setq ent tmp))
    ent
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:createlayer ( lst / def )
    (if (or layerdirector:forcelayprops (not (setq def (tblsearch "layer" (car lst)))))
        (apply
           '(lambda ( lay des col ltp lwt plt pst / dic )
                (   (lambda ( def / ent )
                        (if (setq ent (tblobjname "layer" (car lst)))
                            (entmod (cons (cons -1 ent) def))
                            (entmake def)
                        )
                    )
                    (vl-list*
                       '(000 . "LAYER")
                       '(100 . "AcDbSymbolTableRecord")
                       '(100 . "AcDbLayerTableRecord")
                       '(070 . 0)
                        (cons 002 lay)
                        (cons 062 (if (< 0 col 256) col 7))
                        (cons 006 (if (LM:layerdirector:loadlinetype ltp) ltp "Continuous"))
                        (cons 370 (if (or (= -3 lwt) (<= 0 lwt 211)) lwt -3))
                        (cons 290 plt)
                        (append
                            (if (and (= 'str (type pst))
                                     (zerop (getvar 'pstylemode))
                                     (setq dic (dictsearch (namedobjdict) "acad_plotstylename"))
                                     (setq dic (dictsearch (cdr (assoc -1 dic)) pst))
                                )
                                (list (cons 390 (cdr (assoc -1 dic))))
                            )
                            (if (and des (/= "" des))
                                (progn (regapp "AcAecLayerStandard")
                                    (list
                                        (list -3
                                            (list
                                                "AcAecLayerStandard"
                                               '(1000 . "")
                                                (cons 1000 des)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            lst
        )
        def
    )
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:loadlinetype ( ltp )
    (eval
        (list 'defun 'LM:layerdirector:loadlinetype '( ltp )
            (list 'cond
               '(   (tblsearch "ltype" ltp) ltp)
                (list
                    (list 'vl-some
                        (list 'quote
                            (list 'lambda '( lin )
                                (list 'vl-catch-all-apply ''vla-load
                                    (list 'list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) 'ltp 'lin)
                                )
                               '(tblsearch "ltype" ltp)
                            )
                        )
                        (list 'quote
                            (vl-remove-if
                               '(lambda ( x )
                                    (member (strcase x t)
                                        (if (zerop (getvar 'measurement))
                                           '("acadiso.lin"  "iso.lin") ;; Known metric   .lin files
                                           '("acad.lin" "default.lin") ;; Known imperial .lin files
                                        )
                                    )
                                )
                                (apply 'append
                                    (mapcar
                                       '(lambda ( dir ) (vl-directory-files dir "*.lin" 1))
                                        (vl-remove "" (LM:layerdirector:str->lst (getenv "ACAD") ";"))
                                    )
                                )
                            )
                        )
                    )
                    'ltp
                )
            )
        )
    )
    (LM:layerdirector:loadlinetype ltp)
)

;;----------------------------------------------------------------------;;

(defun LM:layerdirector:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:layerdirector:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

;;----------------------------------------------------------------------;;

(   (lambda ( ) (vl-load-com)
        (if (= 'list (type s::startup))
            (if (not (member '(LM:layerdirector t) s::startup))
                (setq s::startup (append s::startup '((LM:layerdirector t))))
            )
            (defun-q s::startup nil (LM:layerdirector t))
        )
        (princ)
    )
)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;