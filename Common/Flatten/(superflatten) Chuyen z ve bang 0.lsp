
;; by Joe Burke

;; Please send bug reports to me directly at lowercase@hawaii.rr.com 
;; or post to the appropriate topic at theswamp.

;; General notes:

;; The program is intended to be used with "vanilla" ACAD versions
;; 2000 and later. It has not been tested within vertical applications.
;; Testing with versions prior to 2006 is very limited at this point.
;; It has been tested with 2006 and 2008.

;; The program flattens objects to the WCS plane.
;; It may be run from any view. For instance an isometric view
;; may be useful so you can see what it's doing. Suggested method,
;; choose Top view then choose an isometric view like SouthWest.
;; This is still considered WCS, so the program will not switch 
;; views while the selection set is processed. The selection set
;; is processed before blocks are processed.

;; Some objects may be erased. For instance, a line which when
;; flattened has a length close or equal to zero.

;; Some objects may change type. For instance, an arc which
;; is parallel to Front view is converted to a line. A circle
;; or arc which is not parallel to WCS is converted to an an ellipse.
;; An ellipse with an odd normal is converted to a polyline.

;; Locked layers are temporarily unlocked.

;; A block reference (insert) with an odd normal is exploded and  
;; the resulting objects are flattened. Including any nested blocks.
;; A count of exploded references, if any, is provided at the command line.
;; Attributes in exploded blocks are converted to text.

;; Objects inside block definitions are flattened as needed.
;; A side effect of this is unexpected results may occur when the
;; entire drawing is not flattened. In that case, exploding selected 
;; blocks before running the routine may be helpful.

;; -----------------------------------
;; Program options at the command line:
;; > SuperFlatten options [Rename blocks/Explodable blocks/Overkill] < >:

;; All options are offered when using 2006 or later and 
;; Express Tools is loaded. Fewer options are offered when using
;; 2005 or ealier and depending on whether ET is loaded or not.

;; The Rename blocks option is always available.
;; > SuperFlatten options [Rename blocks/Explodable blocks/Overkill] < >: r
;; > Block name options: [Prefix/Suffix] <S>: p
;; > Block name prefix: flat-
;; Example: there's a block named "BlockA" which contains objects 
;; which need to be flattened. If Rename blocks is chosen, the block
;; name is changed to "flat-BlockA". Note, a block is not renamed
;; if it does not contain object which need to be flattened.

;; If the option isn't used, block definitions are flattened and not renamed.

;; Any renamed blocks are listed at the command line. 
;; Thanks to Steve Doman for his input here.

;; The Explodable blocks option is presented when using 2006 or later.
;; > SuperFlatten options [Rename blocks/Explodable blocks/Overkill] < >: e
;; > Temporarily set all blocks explodable? [Yes/No] <Y>:

;; 2006 introduced the idea a block definition may be set so any reference 
;; to it cannot be exploded. If Yes is chosen, all block defintions are 
;; temporarly set to allow explode. They are reset as needed after the 
;; program ends.

;; The Overkill option is presented when Express Tools is loaded.
;; > SuperFlatten options [Rename blocks/Explodable blocks/Overkill] < >: o
;; > Run Overkill after flattening? [Yes/No] <Y>:

;; If Yes is chosen Overkill is called. The option is particularly useful 
;; when 3D solids are exploded in order to flatten. Many duplicate objects 
;; may be deleted.
;; -----------------------------------

;; Note, some block references simply cannot be exploded. 
;; AutoCAD may create such blocks while exploding NUS (non uniformly scaled) 
;; nested blocks. These blocks are anonymous and have a name like this *E<number>.
;; I think these are rare so there's no attempt to deal with them.
;; The report at the end will indicate a block reference could not
;; be flattened and how many.

;; Use this program with CAUTION! Yes, it's DANGEROUS!
;; Recommended: make a copy of the original file and run it on
;; the copy. Xref the original to the copy to compare the two.

;; Disclaimer: if the program wrecks your drawing, it ain't my fault.

;; Programming notes:

;; Block definitions are flattened by copying the objects contained
;; in a definition to a temporary layout. This is done to allow use
;; of (command "explode") for objects which cannot be exploded using
;; the ActiveX Explode method. Thanks to Doug Broad for that idea.
;; The original objects in a definition are deleted and the flattened
;; objects are copied back to the definition.

;; When Overkill is called, the function used is (acet-ss-remove-dups...)
;; My testing indicates the function as called is not sensitive to the
;; current Overkill settings. Example, if Ignore Layers is checked in 
;; the Overkill dialog, duplicate objects on different layers are not deleted.
;; IOW, it seems safe in terms of doing the minimum comparison of objects
;; considered equal.

;; Object types supported: AcDbLine AcDbArc AcDbCircle AcDbEllipse
;; AcDbPolyline AcDb2dPolyline AcDb3dPolyline AcDbFace AcDbSpline
;; AcDbPoint AcDbBlockReference AcDbText AcDbMText AcDbAttribute
;; AcDbAttributeDefinition AcDbTable AcDbHatch AcDbSolid AcDbTrace
;; AcDbLeader Dimensions AcDbRay AcDbXline AcDbMline AcDbWipeout
;; AcDbRasterImage AcDbRegion AcDbShape AcDbPolyFaceMesh AcDbPolygonMesh 
;; AcDbMInsertBlock AcDbFcf (tolerance obj)
;; AcDb3dSolid and AcDbBody (types which can be exploded)
;; AcDb3dSurface.

;; Normals when objects are drawn in these planes.
;; Top: (0.0 0.0 1.0)
;; Bottom: (0.0 0.0 -1.0)
;; Front: (0.0 -1.0 0.0)
;; Back: (0.0 1.0 0.0)
;; Left (-1.0 0.0 0.0)
;; Right (1.0 0.0 0.0)

;; ----------------------------------------------------------------
;; Version history:

;; Version SuperFlatten 1.0 - 7/23/2007.
;; Changed the program name from ZZero to SuperFlatten. Thanks to 
;; Steve Doman for clearing my head on this issue.  :-)
;; Internalized toolbox functions.

;; Version SuperFlatten 1.0a - 7/24/2007.
;; Added support for AeccDbContour, AeccDbPoint and AcDbZombieEntity
;; object types. The zombie type is a proxy object. Some proxy objects
;; can be exploded using command explode, though I'm not sure which ones.
;; AeccDbContour and AeccDbPoint are two which can. Currently under testing.

;; Version SuperFlatten 1.0b - 7/25/2007.
;; Refined the changes/additions in version 1.0a. Limited testing indicates
;; the program should be able to flatten drawings created in Architectural
;; Desktop. The results will differ depending on what version of ACAD is in
;; use and/or whether an ADT object enabler is installed. 
;; If Properties says an ADT object is a proxy (zombie), the proxy is  
;; exploded and the resulting objects are flattened. The likely result will 
;; be a large file with many duplicate objects. Consider using the Overkill 
;; option, though it may take a long time to finish.
;; If Properties says an ADT object is something like an AEC_WALL (AecDbWall)
;; then the result of exploding the object is an anonymous block. The objects
;; within those block definitions are subsequently flattened. So the result 
;; is likely many anonymous blocks, without a huge increase in the number
;; of primary objects contained in the drawing. The Overkill option won't be
;; much help in this case.

;; Whether the program should deal with ADT proxy objects is open to question.
;; 2008 has the -ExportToAutoCAD command which converts ADT objects to standard
;; ACAD objects. But it does not flatten them as far as I've seen. So if that's
;; what you want, you would have to run SuperFlatten anyway, after 
;; -ExportToAutoCAD. Note, versions prior to 2008, 2006 for instance, also
;; support the -ExportToAutoCAD command when the 2006 ADT object enabler 
;; is installed.

;; IOW, you can flatten an ADT drawing using SuperFlatten alone, without 
;; the intermediate step of using ExportToAutoCAD. Hopefully this allows 
;; someone using ACAD 2002 to flatten an ADT drawing. Not tested under 2002, 
;; but it works as tested under 2006.

;; Other changes:

;; Bug fix at sub-function TwoPointObj.
;; Revised the version checking in program options.

;; Version SuperFlatten 1.0c - 7/26/2007.
;; Revised the FlatEllipse sub-function so the program works with the Autodesk 
;; sample file named 3D House.

;; Version SuperFlatten 1.0d - 7/27/2007.
;; Revised the FlatEllipse, FlatCircle and FlatArc sub-functions so the program 
;; works with the Autodesk sample file named Welding Fixture Model.

;; Version SuperFlatten 1.0e - 7/28/2007.
;; Removed the reverse function at the end of sub-function SSVLAList. 
;; Speed testing with the Hotel Model example file indicates the program is 
;; consistently about 14% faster when the selection set list is not reversed. 
;; 42 seconds vs 49. I'm not sure why. Plus the visual integrity of what 
;; ends up on top of what when objects overlap is more faithful to the 
;; pre-flattened drawing.

;; Version SuperFlatten 1.0f - 8/7/2007.
;; Object selection now occurs before program options.
;; The current options are displayed while the user sets options.
;; The variables *overkillans* (for run Overkill) and 
;; *expans* (allow unexplodable blocks to be exploded)
;; are global now so the Yes/No settings are recalled when the 
;; program is run more than once. The Prefix/Suffix string for
;; the Rename blocks option is not global by design.

;; Version SuperFlatten 1.0g - 8/9/2007.
;; Revised the FlatDimension sub-routine to check for dimension types
;; which have point data associated. For instance, an angular dimension
;; has an ExtLine1StartPoint property.
;; Added sub-function ValidItem. Used to check whether the name of a
;; block may already exist when attempting to rename a block.
;; Added the notrenamedlst variable. It's a list of block names
;; which could not be renamed due to an existing block name conflict.
;; Added a report at the end which lists such block names.
;; Restored reverse list call at the SSVLAList function.
;; Revised the FlatHatch function. Added error checking at the Evaluate
;; call. Also moved it into the "and" condition.

;; Version SuperFlatten 1.0h - 8/10/2007.
;; Revised the FlatHatch function. Added zoom to selection set of
;; recreated boundary objects.

;; Version SuperFlatten 1.1 - 8/16/2007.
;; Commented out the timer functions and calls to those functions.

;; Version SuperFlatten 1.1a - 8/20/2007.
;; Bug fix at FlatSpline sub-function.
;; Revised Text and MText handling in terms of the angle of the object
;; when it has an odd normal. An old set of issues finally solved.

;; ----------------------------------------------------------------

;; The shortcut is SF.

(vl-load-com)

(defun c:SuperFlatten ( / *error* doc cnt ss expm locklst blocks layouts views 
                      mspace mspacecnt lst blknamelst patlst hpa templayout 
                      blkdef inoutlst actlayout notflatlst expblklst 
                      expblkcnt renameflag newname newnamelst notrenamedlst 
                      optans presufstr templst orig ucsfol renameans

                      TestZNormal TestXYNormal ZZeroPoint ZZeroCoord 
                      ProcessList SF:MakeLWPolyline GetBlock PointList 
                      RotateToNormal TwoPointObj ApplyProps FlatMText FlatText 
                      FlatPointObj FlatLine FlatACE FlatCircle FlatArc 
                      FlatPline FlatSpline FlatDimension FlatXref FlatShape 
                      FlatHatch FlatSolidOrTrace FlatRayOrXline 
                      FlatWipeoutOrRaster FlatMline FlatTable FlatTolerance 
                      FlatRegion FlatPolyFaceMesh FlatCoordinates 
                      AttributesToText ExpBlockMethod CommandExplode 
                      ModBlockScale SF:TraceObject CheckRename PrefixSuffix 
                      Spinbar LstACADPAT UnlockLayers RelockLayers 
                      SSVLAList SSAfterEnt Round GetNestedNames ClosestPoint 
                      FarthestPoint Farthest2Points AveragePts ValidItem)

                      ;; Global variables: *expans* *overkillans*

  (defun *error* (msg)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*")
        (if blknamelst
          (princ "\n ** CANCELED - UNDO RECOMMENED ** \n")
        )
      )
      (T (princ (strcat "\nError: " msg)))
    )
    (foreach x expblklst (vlax-put x 'Explodable acFalse))    
    (vl-catch-all-apply 'vla-delete (list templayout))
    (RelockLayers locklst)
    (setvar "hpassoc" hpa)
    (setvar "explmode" expm)
    (setvar "ucsfollow" ucsfol)
    (setvar "cmdecho" 1)
    (vla-EndUndoMark doc)
    (princ)
  ) ;end error

  ;;;;;;; START SuperFlatten SUB-FUNCTIONS ;;;;;;;

  ;; Return T if normal is (0.0 0.0 1.0) or (0.0 0.0 -1.0) with fuzz.
  ;; Argument: an ename or vla-object.
  (defun TestZNormal (obj / n)
    (if (= (type obj) 'VLA-object)
      (setq n (vlax-get obj 'Normal))
      (setq n (cdr (assoc 210 (entget obj))))
    )
    (or
      (equal n '(0.0 0.0 1.0) 1e-8)
      (equal n '(0.0 0.0 -1.0) 1e-8)
    )
  ) ;end

  ;; Argument: an ename or vla-object.
  (defun TestXYNormal (obj / n)
    (if (= (type obj) 'VLA-object)
      (setq n (vlax-get obj 'Normal))
      (setq n (cdr (assoc 210 (entget obj))))
    )
    (or
      ;; object drawn in front view
      (equal n '(0.0 -1.0 0.0) 1e-8)
      ;; back
      (equal n '(0.0 1.0 0.0) 1e-8)
      ;; left
      (equal n '(-1.0 0.0 0.0) 1e-8)
      ;; right
      (equal n '(1.0 0.0 0.0) 1e-8)
    )
  ) ;end

  ;Argument: a single 3D point list.
  (defun ZZeroPoint (lst)
    (list (car lst) (cadr lst) 0.0)
  ) ;end

  ;; Argument: a flat 3D coordinate list.
  ;; (setq l '(414.576 572.128 0.0 494.558 637.135 20.0 562.58 575.117 30.0))
  ;; Returns:
  ;; (414.576 572.128 0.0 494.558 637.135 0.0 562.58 575.117 0.0)
  (defun ZZeroCoord (coord / lst)
    (repeat (/ (length coord) 3)
      (setq lst (cons (car coord) lst)
            lst (cons (cadr coord) lst)
            lst (cons 0.0 lst)
            coord (cdddr coord)
      )
    )
    (reverse lst)
  ) ;end

  (defun GetBlock ()
    (vlax-get (vla-get-ActiveLayout doc) 'Block)
  ) ;end

  ;; Called in the ProcessList sub-function.
  ;; Author unknown.
  (defun Spinbar (sbar) 
    (cond ((= sbar "\\") "|")
          ((= sbar "|") "/")
          ((= sbar "/") "-")
          (t "\\")
    )
  ) ;end

  ;; Arguments: an existing value and a test value.
  ;; The order of the arguments passed doesn't matter.
  ;; It determines whether a block definition should be renamed
  ;; or not by setting the renameflag variable.
  (defun CheckRename (exval testval)
    (if (and renameans presufstr)
      (or 
        (equal exval testval 1e-6)
        (setq renameflag T)
      )
    )
  ) ;end

  ;; Check for item in a collection by Doug Broad.
  (defun ValidItem (collection item / res)
    (vl-catch-all-apply
      '(lambda ()
        (setq res (vla-item collection item))))
    res
  )

  ;; Argument: either "prefix" or "suffix" string.
  ;; Called from program options.
  ;; snvalid returns nil when passed a string with 
  ;; leading or trailing spaces.
  (defun PrefixSuffix (argstr / str StripSpaces)
    ;Remove leading and trailing spaces for snvalid check.
    (defun StripSpaces (str)
      (vl-string-right-trim " " (vl-string-left-trim " " str))
    )

    (setq str (getstring T (strcat "\nBlock name " argstr ": ")))

    (if (eq argstr "prefix")
      (setq str (vl-string-left-trim " " str))
      (setq str (vl-string-right-trim " " str))
    )

    (cond
      ((eq "" str)
        (princ "\nBlocks will not be renamed. ")
      )
      ((not (snvalid (StripSpaces str) 0))
        (while
          (and 
            (not (eq "" str))
            (not 
              (snvalid
                (setq str (StripSpaces (getstring T (strcat "\nInvalid " argstr ": ")))) 0
              )
            )
          )
        )
      )
    )
    (if (not (eq "" str))
      str
    )
  ) ;end

  ;; Entmake a lwpline. 
  ;; Returns a lwpline vla-object if successful.
  (defun SF:MakeLWPolyline (ptlst width)
    (if 
      (and
        (> (length ptlst) 1)
        (apply 'and ptlst)
      )
      (if (entmake
            (append
              (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                 (cons 90 (length ptlst))
                 (cons 43 width)
              )
              (mapcar '(lambda (x) (cons 10 x)) ptlst)
            )
          )
        (progn
          (setq renameflag T)
          (vlax-ename->vla-object (entlast))
        )
      )
    )
  ) ;end

  ;; Arguments: two vla-objects.
  ;; Apply the properties of the old object to new object 
  ;; and delete the old object.
  (defun ApplyProps (old new)
    (if 
      (and 
        old 
        new
        (not (vlax-erased-p old))
        (not (vlax-erased-p new))
      )
      (progn
        (mapcar '(lambda (x) (vlax-put new x (vlax-get old x)))
          '("Color" "Layer" "Linetype" "LinetypeScale" "Lineweight")
        )
        (vl-catch-all-apply
          '(lambda () 
            (vlax-put new 'LinetypeGeneration (vlax-get old 'LinetypeGeneration))
          )
        )
        (vla-delete old)
        (setq renameflag T)
      )
    )
  ) ;end

  ;; Returns a nested point list from a flat point list.
  (defun PointList (obj / coord lst)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((eq "AcDbPolyline" (vlax-get obj 'ObjectName))
        (repeat (/ (length coord) 2)
          (setq lst (cons (list (car coord) (cadr coord)) lst)
                coord (cddr coord)
          )
        )
      )
      (T
        (repeat (/ (length coord) 3)
          (setq lst (cons (list (car coord) (cadr coord) (caddr coord)) lst)
                coord (cdddr coord)
          )
        )
      )
    )
    (reverse lst)
  ) ;end

  ;; Used when certain object types pass the TestXYNormal test.
  ;; Converts the argument object to a line. 
  ;; ApplyProps handles delete the source object.
  (defun TwoPointObj (obj / tracepts x y pts newobj)
    (if  
      (or
        (setq tracepts (SF:TraceObject obj))
        (setq tracepts (PointList obj))
      )
      (progn
        (setq x (caar tracepts)
              y (cadar tracepts)
        )
        (if
          (or 
            ;; Bug fixed 7/24/2007
            (vl-every '(lambda (n) (equal x (car n) 1e-2)) tracepts)
            (vl-every '(lambda (n) (equal y (cadr n) 1e-2)) tracepts)
          )
          (progn
            (if (= 2 (length tracepts))
              ;; Added 7/24/2007.
              (setq pts tracepts)
              (setq pts (Farthest2Points tracepts))
            )
            (if (= 2 (length pts))
              (progn
                (setq newobj 
                  (vlax-invoke (GetBlock) 'AddLine 
                    (ZZeroPoint (car pts)) (ZZeroPoint (cadr pts))
                  )
                )
                ;; Do this first, then send object to ProcessList.
                (ApplyProps obj newobj)
                (ProcessList (list newobj))
              )
            )
          )
        )
      )
    )
  ) ;end

  ;; Revised 8/20/2007.
  ;; Convert a list of attribute reference objects to text objects.
  (defun AttributesToText (attlst / elst n obj)
    (foreach x attlst
      (setq n (vlax-get x 'Normal))
      (setq elst (entget (vlax-vla-object->ename x)))
      (entmake
        (list
          '(0 . "TEXT")
          (cons 1 (vlax-get x 'TextString))
          (cons 7 (vlax-get x 'StyleName))
          (cons 8 (vlax-get x 'Layer))
          (cons 10 (vlax-get x 'InsertionPoint))
          (cons 11 (vlax-get x 'TextAlignmentPoint))
          (cons 40 (vlax-get x 'Height))
          (cons 41 (vlax-get x 'ScaleFactor))
          (cons 50 (vlax-get x 'Rotation))
          (cons 51 (vlax-get x 'ObliqueAngle))
          (cons 62 (vlax-get x 'Color))
          (cons 67 (cdr (assoc 67 elst)))
          (cons 71 (cdr (assoc 71 elst)))
          (cons 72 (cdr (assoc 72 elst)))
          (cons 73 (cdr (assoc 74 elst)))
          (cons 410 (cdr (assoc 410 elst)))
        )
      ) ;make
      (setq obj (vlax-ename->vla-object (entlast)))
      (vlax-put obj 'Normal n)
      (if (= 0 (vlax-get obj 'Alignment))
        (vlax-put obj 'InsertionPoint (vlax-get x 'InsertionPoint))
        (vlax-put obj 'TextAlignmentPoint (vlax-get x 'TextAlignmentPoint))
      )
      (FlatText obj)
    )
  ) ;end

  ;; Modify the X, Y and Z scale factors of a block
  ;; reference if they are close to equal so the explode
  ;; method can be used.
  ;; Return T if successful, otherwise nil.
  ;; If nil, call CommandExplode.
  ;; Note, the explode method works if a block has for
  ;; instance a negative X scale factor. The block was mirrored.
  ;; I'm not sure this is a complete solution in terms of
  ;; other possible negative values.
  (defun ModBlockScale (blk / xsf ysf zsf)
    (setq xsf (vlax-get blk 'XScaleFactor)
          ysf (vlax-get blk 'YScaleFactor)
          zsf (vlax-get blk 'ZScaleFactor)
    )
    (if
      (and
        (or
          (equal xsf ysf 1e-2)
          (equal (- xsf) ysf 1e-2)
        )
        (equal ysf zsf 1e-2)
      )
      (progn
        (vlax-put blk 'XScaleFactor (Round xsf 1e-2))
        (vlax-put blk 'YScaleFactor (Round ysf 1e-2))
        (vlax-put blk 'ZScaleFactor (Round zsf 1e-2))
        T
      )
    )
  ) ;end

  ;; Explode a block reference using the Explode method.
  ;; If the reference passes the TestZNormal test it's not exploded.
  (defun ExpBlockMethod (blkref / ip blkdef flag lay attlst exlst)
    (setq blkdef (vla-item blocks (vlax-get blkref 'Name)))
    (if 
      (or 
        (not (vlax-property-available-p blkdef 'Explodable))
        (eq acTrue (vlax-get blkdef 'Explodable))
      )
      (setq flag T)
    )
    (cond
      ((TestZNormal blkref)
        (setq ip (vlax-get blkref 'InsertionPoint))
        (CheckRename ip (ZZeroPoint ip))
        (vlax-put blkref 'InsertionPoint (ZZeroPoint ip))
        (setq attlst (vlax-invoke blkref 'GetAttributes))
        (foreach x attlst (FlatText x))
      )
      ;; Block can be exploded using the explode method and the Explodable property
      ;; is acTrue if that property exists.
      ((and flag (ModBlockScale blkref))    
        (setq lay (vlax-get blkref 'Layer)
              attlst (vlax-invoke blkref 'GetAttributes)
              exlst (vlax-invoke blkref 'Explode)
        )
        (if exlst
          (progn
            (setq renameflag T)
            (setq expblkcnt (1+ expblkcnt))
            (AttributesToText attlst)
            (vla-delete blkref)
            (foreach x exlst
              (if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                (vla-delete x)
              )
            )
            (setq exlst (vl-remove-if 'vlax-erased-p exlst))
            (foreach x exlst
              (if (eq "0" (vlax-get x 'Layer))
                (vlax-put x 'Layer lay)
              )
              (if (zerop (vlax-get x 'Color))
                (vlax-put x 'Color 256)
              )
            )
            (ProcessList exlst)
          )
          (progn
            (setq cnt (1+ cnt))
            (if (not (vl-position "AcDbBlockReference" notflatlst))
              (setq notflatlst (cons "AcDbBlockReference" notflatlst))
            )
          )
        )
      )
      ;; Otherwise use command because the block is not uniformly scaled.
      ;; Which the Exlpode method can't handle.
      (T (CommandExplode blkref))
    ) ;cond
  ) ;end

  ;; The types of objects which may be exploded with this function:
  ;; A hatch with an odd normal which can't be recreated because
  ;; the hatch pattern isn't available.
  ;; Dimensions with an odd normal.
  ;; AcDb3dSolid and AcDbBody objects. Some can be exploded, others can't.
  ;; Block references when the ExpBlockMethod function above can't do it 
  ;; because the reference is not uniformly scaled.
  (defun CommandExplode (obj / lay mark objname attlst name exlst)
    (setq mark (entlast)
          objname (vlax-get obj 'ObjectName)
    )
    (cond
      ((or 
         (eq "AcDb3dSolid" objname)
         (eq "AcDbBody" objname)
         (eq "AcDbSurface" objname)
         ;; Added 7/23/2007
         (eq "AcDbZombieEntity" objname)
         ;; Things like AeccDbContour and AeccDbPoint.
         ;; From either LandDestop or Civil 3D. I'm not sure.
         (wcmatch objname "AeccDb*")
        )
        (command "._explode" (vlax-vla-object->ename obj))
        (if (not (eq mark (entlast)))
          (setq exlst (SSVLAList (ssget "p")))
        )
      )
      ((or 
         (eq "AcDbHatch" objname)
         (wcmatch objname "*Dimension")
        )
        (command "._explode" (vlax-vla-object->ename obj))
        (if (not (eq mark (entlast)))
          (setq exlst (SSVLAList (ssget "p")))
        )
      )
      
      ;; Added 7/24/2007
      ((wcmatch objname "AecDb*,AecsDb*,Aecb*")
        (command "._explode" (vlax-vla-object->ename obj))
        (if (not (eq mark (entlast)))
          (setq exlst (SSVLAList (ssget "p")))
        )
        (if
          (and
            (= 1 (length exlst))
            (eq "AcDbBlockReference" (vlax-get (car exlst) 'ObjectName))
            (wcmatch (setq name (vlax-get (car exlst) 'Name)) "`**")
          )
          (setq blknamelst (cons name blknamelst))
        )
      )

      ((eq "AcDbBlockReference" objname)
         (setq lay (vlax-get obj 'Layer)
               attlst (vlax-invoke obj 'GetAttributes)
         )
         (command "._explode" (vlax-vla-object->ename obj))
         ;; Had some problems here with blocks which cannot be exploded.
         ;; The following test seems to fix it.
         (if 
           (and 
             (not (eq mark (entlast)))
             (setq exlst (SSVLAList (ssget "p")))
             ;(not (eq mark (entlast)))
           )
           (progn
             (setq expblkcnt (1+ expblkcnt))
             (AttributesToText attlst) ;seems OK here
             (foreach x exlst
               (if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                 (vla-delete x)
               )
             )
             (setq exlst (vl-remove-if 'vlax-erased-p exlst))
             ;If an exlpoded object is on layer 0, put it on the
             ;layer of the exploded object. If its color is byBlock, 
             ;change color to byLayer.
             (foreach x exlst
               (if (eq "0" (vlax-get x 'Layer))
                 (vlax-put x 'Layer lay)
               )
               (if (zerop (vlax-get x 'Color))
                 (vlax-put x 'Color 256)
               )
             )
           )
         )
       )
    ) ;cond
    (if exlst
      (progn
        (setq renameflag T)
        (ProcessList exlst)
      )
      ;Else set count of objects not processed and the ObjectName.
      (progn
        (setq cnt (1+ cnt))
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
  ) ;end

  ;; Arguments: vla-object and a normal vector.
  ;; Called from FlatXref and FlatShape.
  ;; The check for the normal Z value approaching 1 or -1 is because
  ;; it seems within that range the display of the object simply
  ;; shows its rotation. There's an example of this in Ken Luk's
  ;; test file from customer files.
  ;; Note, put rotation could be like this.
  ;; (vlax-put obj 'Rotation (+ (* pi 0.5) (atan (cadr n) (car n))))
  (defun RotateToNormal (obj n)
    (if
      (and
        (not (equal 1.0 (caddr n) 1e-5))
        (not (equal -1.0 (caddr n) 1e-5))
      )
      (vlax-put obj 'Rotation 
        (+ (vlax-get obj 'Rotation) (+ (* pi 0.5) (angle '(0 0) n)))
      )
    )
  ) ;end

  ;;; TRACE FUNCTION ;;;
  (defun SF:TraceObject (obj / typlst typ ZZeroList TracePline TraceACE 
                               TraceType1Pline TraceType23Pline)

    ;;;; start trace sub-functions ;;;;

    ;; Argument: 2D or 3D point list.
    ;; Returns: 3D point list with zero Z values.
    (defun ZZeroList (lst)
      (mapcar '(lambda (p) (list (car p) (cadr p) 0.0)) lst)
    )

    ;; Argument: vla-object, a heavy or lightweight pline.
    ;; Returns: WCS point list if successful.
    ;; Notes: Duplicate adjacent points are removed.
    ;;        The last closing point is included given a closed pline.
    (defun TracePline (obj / param endparam anginc tparam pt blg 
                             ptlst delta inc arcparam flag)

      (setq param (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            anginc (* pi (/ 7.5 180.0))
      )

      (while (<= param endparam)
        (setq pt (vlax-curve-getPointAtParam obj param))
        ;Avoid duplicate points between start and end.
        (if (not (equal pt (car ptlst) 1e-12))
          (setq ptlst (cons pt ptlst))
        )
        ;A closed pline returns an error (invalid index) 
        ;when asking for the bulge of the end param.
        (if 
          (and 
            (/= param endparam)
            (setq blg (abs (vlax-invoke obj 'GetBulge param)))
            (/= 0 blg)
          )
          (progn
            (setq delta (* 4 (atan blg)) ;included angle
                  inc (/ 1.0 (1+ (fix (/ delta anginc))))
                  arcparam (+ param inc)
            )
            (while (< arcparam (1+ param))
              (setq pt (vlax-curve-getPointAtParam obj arcparam)
                    ptlst (cons pt ptlst)
                    arcparam (+ inc arcparam)
              )
            )
          )
        )
        (setq param (1+ param))
      ) ;while

      (if (> (length ptlst) 1)
        (progn
          (setq ptlst (vl-remove nil ptlst))
          (ZZeroList (reverse ptlst))
        )
      )
    ) ;end

    ;; Argument: vla-object, an arc, circle or ellipse.
    ;; Returns: WCS point list if successful.
    (defun TraceACE (obj / startparam endparam anginc 
                           delta div inc pt ptlst)
      ;start and end angles
      ;circles don't have StartAngle and EndAngle properties.
      (setq startparam (vlax-curve-getStartParam obj)
            endparam (vlax-curve-getEndParam obj)
            ;anginc (* pi (/ 7.5 180.0))
            anginc (* pi (/ 2.5 180.0))
      )

      (if (equal endparam (* pi 2) 1e-6)
        (setq delta endparam)
        ;added abs 6/23/2007, testing
        (setq delta (abs (- endparam startparam)))
      )

      ;Divide delta (included angle) into an equal number of parts.
      (setq div (1+ (fix (/ delta anginc)))
            inc (/ delta div)
      )

      ;Or statement allows the last point on an open ellipse
      ;rather than using (<= startparam endparam) which sometimes
      ;fails to return the last point. Not sure why.
      (while
        (or
          (< startparam endparam)
          (equal startparam endparam 1e-12)
          ;(equal startparam endparam)
        )
        (setq pt (vlax-curve-getPointAtParam obj startparam)
              ptlst (cons pt ptlst)
              startparam (+ inc startparam)
        )
      )
      (ZZeroList (reverse ptlst))
    ) ;end

    ;; Explode curve fit pline and gather point list from arcs.
    ;; This sub-function deletes objects.
    (defun TraceType1Pline (obj / ptlst objlst lst)
      (setq ptlst (list (vlax-curve-getStartPoint obj))
            objlst (vlax-invoke obj 'Explode)
      )
      (foreach x objlst 
        (setq lst (TraceACE x))
        (if (not (equal (car lst) (last ptlst) 1e-8))
          (setq lst (reverse lst))
        )
        (setq ptlst (append ptlst (cdr lst)))
        (vla-delete x)
      )
      (ZZeroList ptlst)
    ) ;end

    ;; Explode quadratic and cubic plines and gather point list from lines.
    ;; Produces an exact trace.
    ;; This sub-function deletes objects.
    (defun TraceType23Pline (obj / objlst ptlst lastpt)
      (setq objlst (vlax-invoke obj 'Explode)
            lastpt (vlax-get (last objlst) 'EndPoint)
      )
      (foreach x objlst
        (setq ptlst (cons (vlax-get x 'StartPoint) ptlst))
        (vla-delete x)
      )
      (ZZeroList (reverse (cons lastpt ptlst)))
    ) ;end
    ;;;; end trace sub-functions ;;;;

    ;;;; primary trace function ;;;;
    (setq typlst '("AcDb2dPolyline" "AcDbPolyline"  
                   "AcDbCircle" "AcDbArc" "AcDbEllipse")
    )
    (or 
      (eq (type obj) 'VLA-OBJECT)
      (setq obj (vlax-ename->vla-object obj))
    )

    (setq typ (vlax-get obj 'ObjectName))

    (if (vl-position typ typlst)
      (cond
         ((or (eq typ "AcDb2dPolyline") (eq typ "AcDbPolyline")) 
            (cond
              ((or
                 (not (vlax-property-available-p obj 'Type))
                 (= 0 (vlax-get obj 'Type))
                )
                (TracePline obj)
              )
              ((or (= 3 (vlax-get obj 'Type)) (= 2 (vlax-get obj 'Type)))
                (TraceType23Pline obj)
              )
              ((= 1 (vlax-get obj 'Type))
                (TraceType1Pline obj)
              )
            )
         )
         ((or (eq typ "AcDbCircle") (eq typ "AcDbArc") (eq typ "AcDbEllipse"))
           (TraceACE obj)
         )
      )
    )
  ) ;end SF:TraceObject
  ;;; TRACE FUNCTION ;;;
  
  ;; Based on code by Luis Esquivel.
  ;; Returns a list of pattern names from acad.pat.
  (defun LstACADPAT ( / file line tmp lst )
    (setq file (open (findfile "acad.pat") "r"))
    (while (setq line (read-line file))
      (setq tmp (cons line tmp))
    )
    (close file)
    (setq tmp (reverse tmp))
    (setq lst (vl-remove-if-not
      '(lambda (string)
        (if (eq (substr string 1 1) "*") string)) tmp))

    (mapcar
      '(lambda (string)
        (substr string 2 (- (vl-string-search "," string) 1))) lst)
  ) ;end

  ;; Not used.
  ;(defun StartTimer ()
  ;  (setq *start* (getvar "date"))  
  ;) ;end

  ;; Not used.
  ;(defun EndTimer (/ end)
  ;  (setq end (* 86400 (- (getvar "date") *start*)))
  ;  (princ (strcat "\nTimer: " (rtos end 2 8) " seconds\n"))
  ;) ;end

  (defun UnlockLayers (doc / laylst)
    (vlax-for x (vla-get-Layers doc)
      ;filter out xref layers
      (if 
        (and 
          (not (vl-string-search "|" (vlax-get x 'Name)))
          (eq :vlax-true (vla-get-lock x))
        )
        (progn
          (setq laylst (cons x laylst))
          (vla-put-lock x :vlax-false)
        )
      )
    )
    laylst
  ) ;end

  (defun RelockLayers (lst)
    (foreach x lst
      (vl-catch-all-apply 'vla-put-lock (list x :vlax-true))
    )
  ) ;end

  (defun SSVLAList (ss / obj lst i)
    (setq i 0)
    (if ss
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              lst (cons obj lst)
              i (1+ i)
        )
      )
    )
    (reverse lst)
  ) ;end

  ;; Filter out sub-entities and entities not in current space.
  ;; Returns a selection set of primary entities after ename ent
  ;; or nil if ent equals entlast.
  (defun SSAfterEnt (ent / ss entlst)
    (and
      (setq ss (ssadd))
      (while (setq ent (entnext ent))
        (setq entlst (entget ent))
        (if 
          (and
            (not (wcmatch (cdr (assoc 0 entlst)) "ATTRIB,VERTEX,SEQEND"))
            (eq (cdr (assoc 410 entlst)) (getvar "ctab"))
          )
          (ssadd ent ss)
         )
       )
     )
    (if (> (sslength ss) 0) ss)
  ) ;end
  
  ;; Revised to eliminate duplicate block names 6/5/2007.
  (defun GetNestedNames (blkcol blkname / name namelst temp1 temp2)
    ;first nested level
    (vlax-for x (vla-item blkcol blkname)
      (if 
        (and
          (= "AcDbBlockReference" (vlax-get x 'ObjectName))
          (not (vl-position (setq name (vlax-get x 'Name)) namelst))
        )
        (setq namelst (cons name namelst))
      )
    )
    ;nested deeper
    (setq temp1 namelst)
    (while temp1
      (foreach x temp1
        (vlax-for x (vla-item blkcol x)
          (if 
            (and
              (= "AcDbBlockReference" (vlax-get x 'ObjectName))
              (not (vl-position (setq name (vlax-get x 'Name)) namelst))
            )
            (setq namelst (cons name namelst)
                  temp2 (cons name temp2)
            )
          )
        )
      )
      (setq temp1 temp2 temp2 nil)
    )
    (reverse namelst)
  ) ;end

  (defun ClosestPoint ( pt ptlst / range dist res )
    (setq range (* (distance pt (car ptlst)) 1.1))
    (foreach p ptlst
      (setq dist (distance p pt))
      (if (< dist range)
        (setq range dist res p)
      )
    )
    res
  ) ;end

  ;; Arguments - a point and a point list.
  ;; Returns - the point farthest from point.
  (defun FarthestPoint (pt ptlst / x dist res)
    (setq x 0)
    (foreach p ptlst
      (setq dist (distance p pt)) 
      ;revised 7/12/2007 - added equal to avoid errors elsewhere
      (if (>= dist x)
        (setq x dist res p)
      )
    )
    res
  ) ;end

  ;; Argument: a point list.
  ;; Returns: the farthest two points in point list.
  (defun Farthest2Points (ptlst / pt)
    (list
      (setq pt (FarthestPoint (AveragePts ptlst) ptlst))
      (FarthestPoint pt ptlst)
    )
  ) ;end

  ;; Average point from point list.
  (defun AveragePts (ptlist / lst)
    (if (= (length (car ptlist)) 2) ;2D point list
      (setq lst 
        (list
          (apply '+ (mapcar '(lambda (x) (car x)) ptlist))
          (apply '+ (mapcar '(lambda (x) (cadr x)) ptlist))
        )
      )
      (setq lst ;3D point list
        (list
          (apply '+ (mapcar '(lambda (x) (car x)) ptlist))
          (apply '+ (mapcar '(lambda (x) (cadr x)) ptlist))
          (apply '+ (mapcar '(lambda (x) (caddr x)) ptlist))
        )
      )
    )
    (mapcar '(lambda (x) (/ x (length ptlist) 1.0)) lst)
  ) ;end

  ;; Joe Burke 2/23/03
  (defun Round (value to)
    (if (zerop to) value
      (* (atoi (rtos (/ (float value) to) 2 0)) to)))


  ;;;;;;;; START FLATTEN SUB-FUNCTIONS ;;;;;;;;;

  (defun FlatPointObj (obj / coord)
    ; no reason for renameflag here?
    (if (not (TestZNormal obj))
      (vlax-put obj 'Normal '(0.0 0.0 1.0))
    )
    (setq coord (vlax-get obj 'Coordinates))
    (CheckRename coord (ZZeroPoint coord))
    (vlax-put obj 'Coordinates (ZZeroPoint coord))
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
  ) ;end

  (defun FlatLine (obj / stpt enpt)
    (if (not (TestZNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq renameflag T)
      )
    )
    (setq stpt (vlax-get obj 'StartPoint))
    (CheckRename stpt (ZZeroPoint stpt))
    (vlax-put obj 'StartPoint (ZZeroPoint stpt))
    (setq enpt (vlax-get obj 'EndPoint))
    (CheckRename enpt (ZZeroPoint enpt))
    (vlax-put obj 'EndPoint (ZZeroPoint enpt))
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    ;; If flattening made the length very short, delete line. 
    (if (equal 0.0 (vlax-get obj 'Length) 1e-6)
      (vla-delete obj)
    )
  ) ;end

  ;; Revised 8/19/2007.
  (defun FlatMText (obj / ip apt ang ip1 ip2)
    (setq ip (vlax-get obj 'InsertionPoint))
    (CheckRename ip (ZZeroPoint ip))
    (if (TestZNormal obj)
      (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
      (progn
        (setq apt (vlax-get obj 'AttachmentPoint))
        (vlax-put obj 'AttachmentPoint 1)
        (setq ip1 (vlax-get obj 'InsertionPoint))
        (vlax-put obj 'AttachmentPoint 2)
        (setq ip2 (vlax-get obj 'InsertionPoint))
        (setq ang (angle ip1 ip2))
        (vlax-put obj 'Normal '(0.0 0.0 1.0))       
        (vlax-put obj 'Rotation ang)
        (vlax-put obj 'AttachmentPoint apt)
        (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        (setq renameflag T)
      )
    )
  ) ;end

  ;; Revised 8/19/2007.
  (defun FlatText (obj / pt ip ap algn ang)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (if (TestZNormal obj)
      (if (= 0 (vlax-get obj 'Alignment))
        (progn
          (setq ip (vlax-get obj 'InsertionPoint))
          (CheckRename ip (ZZeroPoint ip))
          (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        )
        (progn
          (setq ap (vlax-get obj 'TextAlignmentPoint))
          (CheckRename ap (ZZeroPoint ap))
          (vlax-put obj 'TextAlignmentPoint (ZZeroPoint ap))
        )
      )
      ;; If the text object has an odd normal.
      (progn
        (setq algn (vlax-get obj 'Alignment))
        (if (= 0 algn)
          (setq pt (vlax-get obj 'InsertionPoint))
          (setq pt (vlax-get obj 'TextAlignmentPoint))
        )
        ; Center alignment to get the angle.
        (vlax-put obj 'Alignment 1)
        (setq ang 
          (angle 
            (vlax-get obj 'InsertionPoint)
            (vlax-get obj 'TextAlignmentPoint)
          )
        )
        ; Restore previous alignment.
        (vlax-put obj 'Alignment algn)
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (if (= 0 algn)
          (vlax-put obj 'InsertionPoint (ZZeroPoint pt))
          (vlax-put obj 'TextAlignmentPoint (ZZeroPoint pt))
        )
        (vlax-put obj 'Rotation ang)
        (setq renameflag T)
      ) ;progn odd normal
    ) ;if
  ) ;end

  ;; Convert a circle with odd normal to an ellipse or otherwise.
  (defun FlatCircle (obj / ratio cen pt rad newobj)
    (cond
      ((TestZNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestXYNormal obj)
        (TwoPointObj obj)
      )
      (T
        (setq ratio (abs (caddr (vlax-get obj 'Normal)))
              cen (ZZeroPoint (vlax-get obj 'Center))
              pt (ZZeroPoint (vlax-curve-getPointAtParam obj 0))
              rad (vlax-get obj 'Radius)
        )
        (cond
          ((equal ratio 0.0 1e-4)
            (FlatACE obj)
          ) 
          ((equal ratio 1.0 1e-4)
            (if (setq newobj (vlax-invoke (GetBlock) 'AddCircle cen rad))
              (ApplyProps obj newobj)
            )
          )
          (T
            (setq newobj (vlax-invoke (GetBlock) 
              'AddEllipse cen (mapcar '- cen pt) (abs ratio))
            )
            (ApplyProps obj newobj)
          )
        )
      )
    )
  ) ;end

  ;; Convert an arc with odd normal to an ellipse or otherwise.
  (defun FlatArc (obj / ratio cen pt stpt enpt pt rad
                        newobj stparam enparam flag)
    (cond
      ((TestZNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestXYNormal obj)
        (TwoPointObj obj)
      )
      (T
        (setq ratio (caddr (vlax-get obj 'Normal))
              cen (ZZeroPoint (vlax-get obj 'Center))
              stpt (ZZeroPoint (vlax-get obj 'StartPoint))
              enpt (ZZeroPoint (vlax-get obj 'EndPoint))
              rad (vlax-get obj 'Radius)
        )
        (if (minusp ratio)
          (setq ratio (abs ratio) flag T)
        )
        (cond
          ((< ratio 1e-4)
            (FlatACE obj)
          )
          ((equal ratio 1.0 1e-4)
            (if
              (setq newobj (vlax-invoke (GetBlock)
                'AddArc cen rad stpt (angle cen stpt) (angle cen enpt))
              )
              (ApplyProps obj newobj)
            )
          )
          (T
            (vlax-put obj 'StartAngle 0.0)
            (setq pt (ZZeroPoint (vlax-curve-getStartPoint obj)))
            (setq newobj (vlax-invoke (GetBlock)
              'AddEllipse cen (mapcar '- cen pt) ratio)
            )

            ;; This idea from BreakMethod seems to do the trick.
            (setq pt (vlax-curve-getClosestPointTo newobj stpt)
                  stparam (vlax-curve-getParamAtPoint newobj pt)
                  pt (vlax-curve-getClosestPointTo newobj enpt)
                  enparam (vlax-curve-getParamAtPoint newobj pt)
            )
            ;; If the ratio (last value of normal) 
            ;; was negative which param goes where is reversed.
            (if flag
              (progn
                (vlax-put newobj 'StartParameter enparam)
                (vlax-put newobj 'EndParameter stparam)
              )
              (progn
                (vlax-put newobj 'StartParameter stparam)
                (vlax-put newobj 'EndParameter enparam)
              )
            )
            (ApplyProps obj newobj)
          )
        ) ;cond
      ) ;progn
    ) ;if
  ) ;end

  ;; Revised 7/27/2007. An ellipse with an odd normal which fails the 
  ;; first two conditions is traced.
  ;; It avoids potential "degenerate geometry" errors which are
  ;; not worth the risk invloved trying to preserve an ellipse.
  (defun FlatEllipse (obj / cen)  
    (cond
      ((TestZNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
      )
      ((TestXYNormal obj)
        (TwoPointObj obj)
      )
      (T (FlatACE obj))
    )
  ) ;end

  ;; Revised 7/26/2007. Trace an object when there's
  ;; no other safe way to flatten it.
  (defun FlatACE (obj / ptlst newobj objname)
    (setq ptlst (SF:TraceObject obj))
    (if (setq newobj (SF:MakeLWpolyline ptlst 0.0))
      (ApplyProps obj newobj)
      (progn
        (setq objname (vlax-get obj 'ObjectName)
              cnt (1+ cnt)
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
  ) ;end

  ;; Heavy and lightweight plines.
  ;; A heavy pline is converted to lightweight if it is 
  ;; traced using SF:TraceObject.
  (defun FlatPline (obj / width ptlst newobj)
    (cond 
      ((TestZNormal obj)
        (CheckRename (vlax-get obj 'Elevation) 0)
        (vlax-put obj 'Elevation 0.0)
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestXYNormal obj)
        (TwoPointObj obj)
      )
      (T   
        ;; If a pline had various widths, the new width is zero.
        ;; Seems nothing can be done about that.
        (if 
          (vl-catch-all-error-p 
            (setq width 
              (vl-catch-all-apply 'vlax-get (list obj 'ConstantWidth))
            )
          )
          (setq width 0.0)
        )
        (setq ptlst (SF:TraceObject obj))
        (if (setq newobj (SF:MakeLWpolyline ptlst width))
          (ApplyProps obj newobj)
        )
      )
    )
  ) ;end

  ;; A PolyFaceMesh object with one face needs to be exploded to a 
  ;; 3D Face before FlatCoordinates below can process it.
  (defun FlatPolyFaceMesh (obj / mark)
    (if (/= 1 (vlax-get obj 'NumberOfFaces))
      (FlatCoordinates obj)
      (progn
        (setq mark (entlast))
        (command "._explode" (vlax-vla-object->ename obj))
        (if (not (eq mark (entlast)))
          (FlatCoordinates (vlax-ename->vla-object (entlast)))
        )
      )
    )
  ) ;end

  ;; Handles 3DPoly, 3DFace, PolyFaceMesh, PolygonMesh and Leader objects.
  ;; A PolyFaceMesh object may need pre-processing. See the above function.
  ;; Note, it seems changing the coordinates of a leader when not
  ;; in WCS can change its normal. One reason the program flattens in WCS.
  (defun FlatCoordinates (obj / coord objname)
    (setq coord (vlax-get obj 'Coordinates))
    (if 
      (vl-catch-all-error-p
        (vl-catch-all-apply
          '(lambda () (vlax-put obj 'Coordinates (ZZeroCoord coord)))
        )
      )
      (progn
        (setq cnt (1+ cnt)
              objname (vlax-get obj 'ObjectName)
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
    (CheckRename (vlax-get obj 'Coordinates) coord)
  ) ;end
  
  ;; The normal of a spline is not exposed under Active X.
  ;; There is no normal if the spline is not planar.
  ;; See the IsPlanar property. So if it is planar and the
  ;; coordinate lists are equal with fuzz, the spline does
  ;; not need to be modified.
  ;; If it is modified the spline should not change shape.
  ;; What will happen is any fit points may be lost.
  ;; Seems OK since fit points are sometimes removed during
  ;; spline edit operations anyway. Or it may not have 
  ;; fit points in the first place.
  (defun FlatSpline (obj / ctrlpts testpts kts)
    (setq ctrlpts (vlax-get obj 'ControlPoints)
          testpts (ZZeroCoord ctrlpts)
          
    )
    ;; Revised 8/17/2007 - bug fix.
    (if 
      (or 
        (eq acFalse (vlax-get obj 'IsPlanar))
        (not (equal ctrlpts testpts 1e-8))
      )
      (progn
        (setq kts (vlax-get obj 'Knots))
        (vlax-put obj 'ControlPoints testpts)
        (vlax-put obj 'Knots kts)
        (setq renameflag T)
      )
    )
  ) ;end

  ;; The explode method doesn't work with dimension objects.
  ;; Spent some time with this one. Explode dims with odd normals
  ;; seems the best approach for now.
  (defun FlatDimension (obj / z pt)
    (if (TestZNormal obj)
      (progn
        (setq z (caddr (vlax-get obj 'TextPosition)))
        (CheckRename z 0)
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
        ;; Added the following if statements 8/8/2007
        (if (vlax-property-available-p obj 'ExtLine1Point)
          (progn
            (setq pt (vlax-get obj 'ExtLine1Point))
            (CheckRename pt (ZZeroPoint pt))
            (vlax-put obj 'ExtLine1Point (ZZeroPoint pt))
          )
        )
        (if (vlax-property-available-p obj 'ExtLine2Point)
          (progn
            (setq pt (vlax-get obj 'ExtLine2Point))
            (CheckRename pt (ZZeroPoint pt))
            (vlax-put obj 'ExtLine2Point (ZZeroPoint pt))
          )
        )
        (if (vlax-property-available-p obj 'ExtLine1StartPoint)
          (progn
            (setq pt (vlax-get obj 'ExtLine1StartPoint))
            (CheckRename pt (ZZeroPoint pt))
            (vlax-put obj 'ExtLine1StartPoint (ZZeroPoint pt))
          )
        )
        (if (vlax-property-available-p obj 'ExtLine2StartPoint)
          (progn
            (setq pt (vlax-get obj 'ExtLine2StartPoint))
            (CheckRename pt (ZZeroPoint pt))
            (vlax-put obj 'ExtLine2StartPoint (ZZeroPoint pt))
          )
        )
        (if (vlax-property-available-p obj 'ExtLine1EndPoint)
          (progn
            (setq pt (vlax-get obj 'ExtLine1EndPoint))
            (CheckRename pt (ZZeroPoint pt))
            (vlax-put obj 'ExtLine1EndPoint (ZZeroPoint pt))
          )
        )
        (if (vlax-property-available-p obj 'ExtLine2EndPoint)
          (progn
            (setq pt (vlax-get obj 'ExtLine2EndPoint))
            (CheckRename pt (ZZeroPoint pt))
            (vlax-put obj 'ExtLine2EndPoint (ZZeroPoint pt))
          )
        )
      )
      (CommandExplode obj)
    )
  ) ;end

  ;; Change the normal first and then the IP.
  (defun FlatXref (obj / ip nrml)
    (setq ip (vlax-get obj 'InsertionPoint)
          nrml (vlax-get obj 'Normal)
    )
    (if (not (TestZNormal obj))
      (progn 
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (RotateToNormal obj nrml)
      )
    )
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end

  (defun FlatTolerance (obj / ip nrml)
    (setq ip (vlax-get obj 'InsertionPoint)
          nrml (vlax-get obj 'Normal)
    ) 
    (if (not (TestZNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end

  ;; Formerly named FlatBlockReference.
  (defun FlatMInsert (obj / nrml blkip ip attlst flag ip ap)
    (setq nrml (vlax-get obj 'Normal)
          blkip (vlax-get obj 'InsertionPoint)
    )
    (if (not (TestZNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq flag T
              renameflag T
        )
      )
    )
    (setq attlst (vlax-invoke obj 'GetAttributes))
    (foreach x attlst
      (CheckRename (vlax-get x 'Thickness) 0)
      (vlax-put x 'Thickness 0.0)
      (if (not (TestZNormal x))
        (progn
          (vlax-put x 'Normal '(0.0 0.0 1.0))
          (setq renameflag T)
        )
      )
      (if (= 0 (vlax-get x 'Alignment))
        (progn
          (setq ip (vlax-get x 'InsertionPoint))
          (CheckRename ip (ZZeroPoint ip))
          (vlax-put x 'InsertionPoint (ZZeroPoint ip))
        )
        (progn
          (setq ap (vlax-get x 'TextAlignmentPoint))
          (CheckRename ap (ZZeroPoint ap))
          (vlax-put x 'TextAlignmentPoint (ZZeroPoint ap))
        )
      )
    )
    (CheckRename blkip (ZZeroPoint blkip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint blkip))
    ;; Doing this twice seems screwy but it works.
    (foreach x attlst
      (if (= 0 (vlax-get x 'Alignment))
        (vlax-put x 'InsertionPoint (ZZeroPoint (vlax-get x 'InsertionPoint)))
        (vlax-put x 'TextAlignmentPoint (ZZeroPoint (vlax-get x 'TextAlignmentPoint)))
      )
    )
    ;; not calling RotateToNormal here because the Rotate method
    ;; needs to be used, rather than put Rotation property for 2008.
    ;; Because attributes behave different in that version.
    ;; See topic named "vla-transformby problem rotating blocks"
    ;; dated 4/26/2007 in the customization NG. Check Tony's comments.
    (if
      (and
        flag
        (not (equal 1.0 (caddr nrml) 1e-5))
        (not (equal -1.0 (caddr nrml) 1e-5))
      )
      (progn
        (vlax-invoke obj 'Rotate 
          (vlax-get obj 'InsertionPoint) (+ (* pi 0.5) (angle '(0 0) nrml))
        )
        (setq renameflag T)
      )
    )
  ) ;end

  ;; The explode method doesn't work with hatch objects.
  ;; The patlst var is local in primary function.
  ;; Need to look at gradient hatches.

  ;; If a hatch has an odd normal and the hatch pattern is
  ;; available, the hatch is recreated to flatten it.
  ;; If the pattern isn't available, the hatch is exploded
  ;; and the result is flattened.

  ;; The HatchObjectType property specifies either a regular
  ;; hatch (zero) or a gradient hatch (one). Gradient hatch
  ;; was new in ACAD 2004. Like a solid hatch, a gradient hatch
  ;; cannot be exploded. If a gradient hatch has an odd normal
  ;; it's converted to a solid hatch to allow flattening.
  ;; The -hatchedit command does not work with a gradient hatch.
  ;; Gradients must be edited using the hatchedit dialog.

  ;; The HatchStyle style property determines Normal, Outer, Ignore.
  ;; The Style of the argument object is applied to the new hatch
  ;; if one is created.

  ;; Fixed a problem here when the hatch spacing of the existing
  ;; hatch is too dense so new hatch (or recreate boundary) fails.
  ;; Due to someone changed a standard hatch pattern?

  (defun FlatHatch (obj / rtd patname mark ss sset newobj)

    ;radians to degrees
    (defun rtd (radians)
       (/ (* radians 180.0) pi)
    ) ;end

    (cond 
      ((TestZNormal obj)
        (CheckRename (vlax-get obj 'Elevation) 0)
        (vlax-put obj 'Elevation 0.0)
      )
      ((TestXYNormal obj)
        (vla-delete obj)
        (setq renameflag T)
      )
      ;; Added 7/21/2007.
      ;; A gradient hatch can be changed to a solid.
      ((and
         (vlax-property-available-p obj 'HatchObjectType)
         (= 1 (vlax-get obj 'HatchObjectType))
        )
        (vlax-put obj 'HatchObjectType 0)
        (ProcessList (list obj))
      )
      ;; Attempting to recreate the boundary and create a new hatch.
      ((and
         ;; Recreate boundary introduced at 2006.
         (>= (atoi (getvar "AcadVer")) 16)
         (or patlst (setq patlst (LstACADPAT)))
         (setq patname (vlax-get obj 'PatternName))
         (vl-position patname patlst)
         (setq mark (entlast))
         (not (command "._hatchedit" (vlax-vla-object->ename obj) "b" "p" "n"))
         ;; Selection set of the boundary object(s).
         (setq sset (SSAfterEnt mark))
         ;; Added 7/10/2007 to prevent what's likely a rare problem.
         ;; The hatch command includes an object which is not part of the
         ;; selection set when zoomed way out. Seems like an ACAD bug.
         (not (command "zoom" "object" sset ""))
         (setvar "hpassoc" 1) 
         ;(setvar "hpassoc" 0)
         (not (command "._hatch" patname 
                (vlax-get obj 'PatternScale)
                (rtd (vlax-get obj 'PatternAngle)) "s" sset ""
              )
         )
         ;; Restore previous zoom.
         (not (command "zoom" "previous"))
         ;; Delete boundary objects here rather than later.
         (if sset
           (mapcar 'vla-delete (SSVLAList sset))
         )
         (setq newobj (vlax-ename->vla-object (entlast)))
         (eq "AcDbHatch" (vlax-get newobj 'ObjectName))
         ;; updates the hatch
         (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'vlax-invoke 
             (list newobj 'Evaluate))))
       ) ;and
        (vlax-put newobj 'HatchStyle (vlax-get obj 'HatchStyle))
        (vlax-put newobj 'AssociativeHatch 0)
        (ApplyProps obj newobj)
      )
      (T (CommandExplode obj))
    ) ;cond
  ) ;end

  ;; AcDbSolid or AcDbTrace
  ;; Though Properties shows an Elevation value for a solid,
  ;; the vla-object does not have an elevation property.
  ;; Get the coord first, then change the normal, then put zzerocoord.
  (defun FlatSolidOrTrace (obj / coord)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((TestZNormal obj)
        (CheckRename coord (ZZeroCoord coord))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
      )
      ((TestXYNormal obj)
        (TwoPointObj obj)
      )
      (T
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
      )
    )
  ) ;end

  (defun FlatShape (obj / ip nrml)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq ip (vlax-get obj 'InsertionPoint)
          nrml (vlax-get obj 'Normal)
    )
    (if (not (TestZNormal obj))
      (progn 
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (RotateToNormal obj nrml)
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end

  (defun FlatRayOrXline (obj / bp sp dv)
    (setq bp (vlax-get obj 'BasePoint))
    (CheckRename bp (ZZeroPoint bp))
    (vlax-put obj 'BasePoint (ZZeroPoint bp))
    (setq sp (vlax-get obj 'SecondPoint))
    (CheckRename sp (ZZeroPoint sp))
    (vlax-put obj 'SecondPoint (ZZeroPoint sp))
    (setq dv (vlax-get obj 'DirectionVector))
    (CheckRename dv (ZZeroPoint dv))
    (vlax-put obj 'DirectionVector (ZZeroPoint dv))
  ) ;end

  ;; AcDbRasterImage or AcDbWipeout
  ;; Flatten raster (image) objects and wipeouts which are not 
  ;; parallel to WCS. Discovered by accident, changing the rotation 
  ;; property flattens ones which are not parallel to WCS.
  (defun FlatWipeoutOrRaster (obj / org)
    (vlax-put obj 'Rotation (vlax-get obj 'Rotation))
    (setq org (vlax-get obj 'Origin))
    (CheckRename org (ZZeroPoint org))
    (vlax-put obj 'Origin (ZZeroPoint org))
  ) ;end

  ;; Like a Table object the Normal property is not exposed.
  ;; Modify the normal using entmod.
  (defun FlatMline (obj / ename elst mark lst ptlst pts z line)
    (setq ename (vlax-vla-object->ename obj))
    (cond
      ;; Flatten to a single line.
      ((TestXYNormal ename)
         ;; Could use CommandExplode if that function was changed to
         ;; return the exploded objects list rather than passing that
         ;; list to ProcessList. Other functions would have to be modified.
         ;(setq lst (CommandExplode obj))
         (setq mark (entlast))
         (command "._explode" ename)
         ;; Test object was exploded.
         (if (setq lst (SSVLAList (SSAfterEnt mark)))
           (progn
             (foreach x lst
               (setq ptlst (cons (ZZeroPoint (vlax-get x 'StartPoint)) ptlst))
               (setq ptlst (cons (ZZeroPoint (vlax-get x 'EndPoint)) ptlst))
             )
             (setq pts (Farthest2Points ptlst))
             (setq line (car lst))
             (vlax-put line 'StartPoint (car pts))
             (vlax-put line 'EndPoint (cadr pts))
             (mapcar 'vla-delete (cdr lst))
             (setq renameflag T)
           )
         )
       )
      ;; Flatten mline with an odd normal.
      ((not (TestZNormal ename))
        (setq elst (entget ename))
        (entmod (subst (cons 210 '(0.0 0.0 1.0)) (assoc 210 elst) elst))
        ;; This is needed to flatten, though not at Z zero. Strange.
        (vlax-put obj 'Coordinates (ZZeroCoord (vlax-get obj 'Coordinates)))
        ;; All the Z values should be the same at this point.
        (setq z (caddr (vlax-get obj 'Coordinates)))
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
        (setq renameflag T)
      )
      ;; Flatten to Z zero coordinates. Move because this
      ;; (vlax-put obj 'Coordinates (ZZeroCoord (vlax-get obj 'Coordinates)))
      ;; does not work to change the Z vlaues.
      (T
        (setq z (caddr (vlax-get obj 'Coordinates)))
        (CheckRename z 0)
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
      )
    )
  ) ;end

  ;; The Direction property is similar to angle or rotation property.
  ;; The normal isn't exposed under Active X, but is
  ;; available using entget. Use entmod to change it.
  (defun FlatTable (obj / ename elst nrml ip dir)
    (setq ename (vlax-vla-object->ename obj)
          elst (entget ename)
          nrml (cdr (assoc 210 elst))
          ;The original ip for case where the normal is modified.
          ip (vlax-get obj 'InsertionPoint)
          dir (vlax-get obj 'Direction)
    )
    (if 
      (not
        (or
          ;; removed fuzz 6/6/2007
          (equal nrml '(0.0 0.0 1.0))
          (equal nrml '(0.0 0.0 -1.0))
        )
      )
      (progn
        (entmod (subst (cons 210 '(0.0 0.0 1.0)) (assoc 210 elst) elst))
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
    (CheckRename dir (ZZeroPoint dir))
    ;; Fix case when direction Z value is like this (0.569751 0.0 0.821818).
    (vlax-put obj 'Direction (ZZeroPoint dir))
    (vlax-invoke obj 'RecomputeTableBlock acTrue)
  ) ;end

  ;; Regions are exploded. Seems there's no other way to deal with them.
  (defun FlatRegion (obj / lst)
    (if (setq lst (vlax-invoke obj 'Explode))
      (progn
        (vla-delete obj)
        (setq renameflag T)
        (ProcessList lst)
      )
      ;Else set count of objects not processed and the ObjectName.
      (progn
        (setq cnt (1+ cnt))
        (if (not (vl-position "AcDbRegion" notflatlst))
          (setq notflatlst (cons "AcDbRegion" notflatlst))
        )
      )
    )
  ) ;end

  ;;;;;;;; END FLATTEN SUB-FUNCTIONS ;;;;;;;;;;;;;

  ;; ProcessList
  ;; Argument: a list of vla-objects.
  ;; It's primary function is send vla-objects to other sub-functions
  ;; for processing. All objects pass through this function.
  ;; It handles the progress indicator Spinbar. 
  ;; It also checks for objects passed which may be erased elsewhere.
  ;; And it does some pre-processing to check for very small objects,
  ;; which are deteted.
  
  (defun ProcessList (lst / objname)
    
    ;; Report either block definitions are being 
    ;; flattened or the selection set.
    (if inoutlst
      (princ 
        (strcat "\rFlattening blocks, please do not Cancel... " 
          (setq *sbar (Spinbar *sbar)) "\t")
      )    
      (princ 
        (strcat "\rFlattening selection... " 
          (setq *sbar (Spinbar *sbar)) "\t")
      )
    )

    (foreach x lst
      (if (not (vlax-erased-p x))
        (progn
          (setq objname (vlax-get x 'ObjectName))
          (cond
            ((eq "AcDbLine" objname)
              (if (< (vlax-get x 'Length) 1e-6)
                (vla-delete x)
                (FlatLine x)
              )
            )
            ((eq "AcDbCircle" objname)
              (if (< (vlax-get x 'Radius) 1e-6)
                (vla-delete x)
                (FlatCircle x)
              )
            )
            ((eq "AcDbArc" objname)
              (if 
                (or
                  (< (vlax-get x 'TotalAngle) 1e-6)
                  (< (vlax-get x 'Radius) 1e-6)
                )
                (vla-delete x)
                (FlatArc x)
              )
            )
            ((eq "AcDbEllipse" objname)
              (if
                (and
                  (< (vlax-get x 'MajorRadius) 1e-6)
                  (< (vlax-get x 'MinorRadius) 1e-6)
                )
                (vla-delete x)
                (FlatEllipse x)
              )
            )
            ((or
                (eq "AcDbPolyline" objname)
                (eq "AcDb2dPolyline" objname)
              )
              (if (< (vlax-curve-getDistAtParam x (vlax-curve-getEndParam x)) 1e-6)
                (vla-delete x)
                (FlatPLine x)
              )
            )
            ((eq "AcDbSpline" objname)
              (if (< (vlax-curve-getEndParam x) 1e-6)
                (vla-delete x)
                (FlatSpline x)
              )
            )
            ((or
                (eq "AcDb3dPolyline" objname)
                (eq "AcDbFace" objname)
                (eq "AcDbLeader" objname)
                (eq "AcDbPolygonMesh" objname)
              )
              (FlatCoordinates x)
            )
            ((eq "AcDbPolyFaceMesh" objname)
              (FlatPolyFaceMesh x)
            )
            ((eq "AcDbPoint" objname)
              (FlatPointObj x)
            )
            ((and 
                (eq "AcDbBlockReference" objname)
                (vlax-property-available-p x 'Path)
              )
              (FlatXref x)
            )
            ;; Revised 7/16/2007 - explode a block which only
            ;; contains other blocks. It won't work if the block
            ;; reference is NUS beyond what ModBlockScale does.
            ((eq "AcDbBlockReference" objname)
              (vlax-for y (vla-item blocks (vlax-get x 'Name)) 
                (setq templst (cons y templst))
              )
              (if 
                (and 
                  (vl-every '(lambda (z)
                    (eq "AcDbBlockReference" (vlax-get z 'ObjectName))) 
                      templst)
                  (ModBlockScale x)
                )
                (progn
                  (ProcessList (vlax-invoke x 'Explode))
                  (vla-delete x)
                  (setq templst nil)
                )
                (ExpBlockMethod x)
              )
              (setq templst nil)
            )
            ((or
               (eq "AcDbText" objname)
               (eq "AcDbAttribute" objname)
               (eq "AcDbAttributeDefinition" objname)
              )
              (FlatText x)
            )
            ((eq "AcDbMText" objname)
              (FlatMText x)
            )          
            ((eq "AcDbTable" objname)
              (FlatTable x)
            )
            ((eq "AcDbHatch" objname)
              (FlatHatch x)
            )
            ((wcmatch objname "*Dimension")
              (FlatDimension x)
            )
            ((eq "AcDbRegion" objname)
              (FlatRegion x)
            )
            ;; Keep in mind the 2008 "flatshot" command 
            ;; for flattening 3D solids.
            ((or
               (eq "AcDb3dSolid" objname) 
               (eq "AcDbBody" objname)
               (eq "AcDbSurface" objname)
              )
              (CommandExplode x)
            )
            ((eq "AcDbShape" objname)
              (FlatShape x)
            )
            ((or
               (eq "AcDbSolid" objname)
               (eq "AcDbTrace" objname)
              )
              (FlatSolidOrTrace x)
            )
            ((or
               (eq "AcDbRay" objname)
               (eq "AcDbXline" objname)
              )
              (FlatRayOrXline x)
            )
            ;; Cannot be exploded.
            ((eq "AcDbMInsertBlock" objname)
              (FlatMInsert x)
            )
            ((eq "AcDbMline" objname)
              (FlatMline x)
            )
            ((or
               (eq "AcDbWipeout" objname)   
               (eq "AcDbRasterImage" objname)
              )
              (FlatWipeoutOrRaster x)
            )
            ((eq "AcDbFcf" objname)
              (FlatTolerance x)
            )
            ((or
               (wcmatch objname "AecDb*,AecsDb*,AeccDb*,Aecb*")
               (eq "AcDbZombieEntity" objname)
              )
              (CommandExplode x)
            )
            ;; Ignore viewports.
            ((eq "AcDbViewport" objname))
            ;; Any object not included above.
            (T 
              (setq cnt (1+ cnt))
              (if (not (vl-position objname notflatlst))
                (setq notflatlst (cons objname notflatlst))
              )
            )
          ) ;cond
        ) ;progn
      ) ;if not erased
    ) ;foreach
  ) ;end

  ;;;;;;; END SuperFlatten SUB-FUNCTIONS ;;;;;;;

  ;;;;;;; START PRIMARY ROUTINE ;;;;;;;

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        blocks (vla-get-Blocks doc)
        layouts (vla-get-Layouts doc)
        views (vla-get-Views doc) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        mspace (vla-get-ModelSpace doc)
        mspacecnt (vlax-get mspace 'Count)
  )
  
  ;; Delete a temporary saved view if it exists.
  (vl-catch-all-apply '(lambda () (vla-delete (vla-item views "zztemp")))) 

  (vla-StartUndoMark doc)

  (setq locklst (UnlockLayers doc)
        hpa (getvar "hpassoc")
        expm (getvar "explmode")
        ucsfol (getvar "ucsfollow")
        cnt 0
        expblkcnt 0
  )

  (setvar "ucsfollow" 0)


  ;; Get the selection set.
  (if (setq ss (ssget (list (cons 410 (getvar "ctab")))))
    (progn

      ;;; ----- START PROGRAM OPTIONS ----- ;;;

      ;; Added program options 7/10/2007.
      ;; Thanks to Steve Doman for his help with the interface.

      ;; Added 8/6/2007 - recall previous options for Overkill
      ;; and explodable blocks in 2006 or later.
      (or *expans* (setq *expans* "No"))
      (or *overkillans* (setq *overkillans* "No"))
      
      (setq optans T)
      
      (cond
        ;; pre 2006 and ET is not loaded
        ((and (< (atof (getvar "AcadVer")) 16.2) (not acet-ss-remove-dups))
          (while optans

            (if presufstr 
              (princ (strcat "\nCurrent options: Rename=" renameans "> " presufstr))
              (princ (strcat "\nCurrent options: Rename=Unspecified"))
            )

            (initget "Rename")
            (setq optans
              (getkword 
                "\nSuperFlatten options [Rename blocks] < >: ")
            )
            (cond 
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans 
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
            )
          )
        )

        ;; pre 2006 and ET is loaded
        ((and (< (atof (getvar "AcadVer")) 16.2) acet-ss-remove-dups)
          (while optans

            (if presufstr 
              (princ (strcat "\nCurrent options: Rename=" renameans "> " presufstr))
              (princ (strcat "\nCurrent options: Rename=Unspecified"))
            )
            (princ (strcat "  Overkill=" *overkillans*))

            (initget "Rename Overkill")
            (setq optans
              (getkword 
                "\nSuperFlatten options [Rename blocks/Overkill] < >: ")
            )
            (cond 
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans 
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
              ((eq optans "Overkill")
                (initget "Yes No")
                (setq *overkillans* 
                  (getkword "\nRun Overkill after flattening? [Yes/No] <N>: ")
                )
                (if (not *overkillans*) (setq *overkillans* "No"))
              )
            )
          )
        )

        ;; 2006 or later and ET is not loaded
        ((and (>= (atof (getvar "AcadVer")) 16.2) (not acet-ss-remove-dups))
          (while optans
            (if presufstr 
              (princ (strcat "\nCurrent options: Rename=" renameans "> " presufstr))
              (princ (strcat "\nCurrent options: Rename=Unspecified"))
            )            
            (princ (strcat "  Explodable=" *expans*))

            (initget "Rename Explodable")
            (setq optans
              (getkword 
                "\nSuperFlatten options [Rename blocks/Explodable blocks] < >: ")
            )
            (cond 
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans 
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
              ((eq optans "Explodable")
                (initget "Yes No")
                (setq *expans* 
                  (getkword 
                    "\nTemporarily set all blocks explodable? [Yes/No] <N>: ")
                )
                (if (not *expans*) (setq *expans* "No"))
              )
            )
          )
        )

        ;; 2006 or later and ET is loaded
        ((and (>= (atof (getvar "AcadVer")) 16.2) acet-ss-remove-dups)
          (while optans            
            (if presufstr 
              (princ (strcat "\nCurrent options: Rename=" renameans "> " presufstr))
              (princ (strcat "\nCurrent options: Rename=Unspecified"))
            )
            
            (princ (strcat "  Explodable=" *expans*
                           "  Overkill=" *overkillans*
                   )
            )
            
            (initget "Rename Explodable Overkill")
            (setq optans
              (getkword 
                "\nSuperFlatten options [Rename blocks/Explodable blocks/Overkill] < >: ")
            )
            (cond 
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans 
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
              ((eq optans "Explodable")
                (initget "Yes No")
                (setq *expans* 
                  (getkword 
                    "\nTemporarily set all blocks explodable? [Yes/No] <N>: ")
                )
                (if (not *expans*) (setq *expans* "No"))
              )
              ((eq optans "Overkill")
                (initget "Yes No")
                (setq *overkillans* 
                  (getkword "\nRun Overkill after flattening? [Yes/No] <N>: ")
                )
                (if (not *overkillans*) (setq *overkillans* "No"))
              )
            )
          )
        )
      ) ; end options cond
      
      ;;; ----- END PROGRAM OPTIONS ----- ;;;

      ;; Added 6/25/2007 to deal with the 2006 Explodable blocks option.
      ;; It might be smarter by looking at selected
      ;; blocks and then decide whether anything needs to be done.
      ;; But that seems a bit of overkill since in most cases the
      ;; user will select all. The following seems sufficient.

      (if (eq "Yes" *expans*)
        (vlax-for x blocks 
          (if 
            (and
              (vlax-property-available-p x 'Explodable)
              (eq acFalse (vlax-get x 'Explodable))
            )
            (progn
              (setq expblklst (cons x expblklst))
              (vlax-put x 'Explodable acTrue)
            )
          )
        )
      )

      ;(starttimer)

      (setvar "cmdecho" 0)
      (setvar "explmode" 1)

      ;; Flattening needs to be done in WCS due to how some
      ;; objects behave while interacting with the code.
      ;; An isometric view like SW can be used without 
      ;; triggering the following.
      ;; There's a small speed penalty if the current view
      ;; has to be saved and restored.
      ;; The UCS/view can be whatever when the code is run 
      ;; and if not WCS, it will be restored as needed.
      (if (zerop (getvar "worlducs"))
        (progn
          (command "._view" "s" "zztemp")
          (command "._ucs" "w")
          (if (zerop (getvar "ucsfollow"))
            (command "._plan" "w")
          )
        )
      )

      ;; Convert the selection set to a list of vla-objects.
      ;; Might use the ActiveSelectionSet here.
      (setq lst (SSVLAList ss))

      ;; Make a list of block names selected including nested names.
      (foreach x lst
        (if 
          (and 
            (eq "AcDbBlockReference" (vlax-get x 'ObjectName))
            (not (vlax-property-available-p x 'Path))
          )
          (progn
            (setq name (vlax-get x 'Name))
            (if (not (vl-position name blknamelst))
              (setq blknamelst (cons name blknamelst))
            )
            (foreach i (GetNestedNames blocks name)
              (if (not (vl-position i blknamelst))
                (setq blknamelst (cons i blknamelst))
              )
            )
          )
        )
      )

      ;; Process the selected objects before flattening block defintions.
      ;; Moved to this location 7/19/2007. It needs to be here rather than
      ;; after flatten definitions so the condition for block references
      ;; in ProcessList does what it should. Explode blocks which only
      ;; contain other blocks.
      
      (ProcessList lst)

      ;; Check for for empty blocks.
      ;; Causes an error with copy objects.
      ;; Empty block named "CIRCULAR STAIRS" found in Ken Luk's 
      ;; customer files example file.
      (if blknamelst
        (progn
          ;; Delete the temporary layout if it already exists.
          (vl-catch-all-apply '(lambda () (vla-delete (vla-item layouts "SuperFlatten layout"))))
          (setq actlayout (vlax-get doc 'ActiveLayout)
                templayout (vlax-invoke layouts 'Add  "SuperFlatten layout")
                layoutblk (vlax-get templayout 'Block)
          )
          (vlax-put doc 'ActiveLayout templayout)
          (foreach x blknamelst
            (setq blkdef (vla-item blocks x)
                  inoutlst nil
                  renameflag nil
            )

            ;; added 7/16/2007 - from Steve's large example file:
            ;; a block definition
            ;; Name = "3D-BASE-STREET-TREES"
            ;; Origin = (66965.5 13010.2 -354.0)
            (setq orig (vlax-get blkdef 'Origin))
            (CheckRename orig (ZZeroPoint orig))
            (vlax-put blkdef 'Origin (ZZeroPoint (vlax-get blkdef 'Origin)))
            
            ;; List objects in source block and filter out viewports.
            (vlax-for i blkdef
              (or
                (eq "AcDbViewport" (vlax-get i 'ObjectName))
                (setq inoutlst (cons i inoutlst))
              )
            )

            (if inoutlst
              (progn

                ;; Copy list to the layout block.
                (setq inoutlst (vlax-invoke doc 'CopyObjects inoutlst layoutblk))

                ;; Empty the source block, except for viewports.
                (vlax-for i blkdef
                  (or
                    (eq "AcDbViewport" (vlax-get i 'ObjectName))
                    (vla-delete i)
                  )
                )

                ;; Flatten objects in layout.
                (ProcessList inoutlst)
                
                (setq inoutlst nil)

                ;; List the flattened objects, filter out viewports.
                (vlax-for i layoutblk
                  (or
                    (eq "AcDbViewport" (vlax-get i 'ObjectName))
                    (setq inoutlst (cons i inoutlst))
                  )
                )

                ;; Copy the flattened objects in the layout back into
                ;; the block definition and delete objects in the layout.
                (if inoutlst
                  (progn
                    (vlax-invoke doc 'CopyObjects inoutlst blkdef)
                    (mapcar 'vla-delete inoutlst)
                  )
                )
              )
            )

            (if 
              (and 
                ;; Cannot rename anonymous blocks.
                (not (vl-string-search "*" x))
                renameflag 
                renameans
                presufstr
              )
              (cond
                ((and
                   (eq renameans "Prefix")
                   (setq newname (strcat presufstr x))
                  )
                  ;; Added existing block name check 8/9/2007.
                  (if (ValidItem blocks newname)
                    (setq notrenamedlst (cons x notrenamedlst))
                    (progn
                      (vlax-put blkdef 'Name newname)
                      (setq newnamelst (cons newname newnamelst))
                    )
                  )
                )
                ((and
                   (eq renameans "Suffix")
                   (setq newname (strcat x presufstr))
                  )
                  ;; Added existing block name check 8/9/2007.
                  (if (ValidItem blocks newname)
                    (setq notrenamedlst (cons x notrenamedlst))
                    (progn
                      (vlax-put blkdef 'Name newname)
                      (setq newnamelst (cons newname newnamelst))
                    )
                  )
                )
              )
            )
          ) ;foreach
          (vlax-put doc 'ActiveLayout actlayout)
          ;templayout is deleted in the error handler.
        ) ;progn
      ) ;if blknamelst

      (if blknamelst
        (vla-regen doc acActiveViewport)
      )

      (if 
        (and 
          (eq "Yes" *overkillans*)
          (setq ss 
            (cadr 
              (acet-ss-remove-dups 
                (ssget "x" '((410 . "Model"))) 1e-6 nil)
            )
          )
        )
        (command "._erase" ss "")
      )


      (if (tblsearch "view" "zztemp")
        (command "._view" "restore" "zztemp" "._view" "delete" "zztemp")
      )

      (if (or newnamelst notrenamedlst)
        (textscr)
      )
      
      (if newnamelst
        (progn
          (princ "\nThe following blocks were renamed: ")
          (foreach x newnamelst
            (print x)
          )
        )
      )

      (if notrenamedlst
        (progn
          (princ "\nThe following blocks were not renamed due to existing block name conflict: ")
          (foreach x notrenamedlst
            (print x)
          )
        )
      )

      (if (> expblkcnt 0)
        (princ (strcat "\nNumber of blocks exploded: " (itoa expblkcnt)))
      )

      (princ 
        (strcat "\nNumber of objects in model space before: " 
          (itoa mspacecnt) " after: " (itoa (vlax-get mspace 'Count)) " \n"
        )
      )

      (if (> cnt 0)
        (progn
          (princ 
            (strcat "\nNumber of objects not processed or flattened: " (itoa cnt) " \n")
          )
          (if notflatlst
            (progn
              (princ "\nObject types not flattened: ")
              (foreach x notflatlst
                (setq pos (+ 3 (vl-string-search "Db" x)))
                (princ (strcat (substr x pos) " "))
              )
              (print)
            )
          )
        )
      )
      
      ;(endtimer)
      
    ) ;progn

    (princ "\nNothing selected. ")

  ) ;if
  
  (*error* nil)

) ;end SuperFlatten

  ;shortcut
  (defun c:SF () (c:SuperFlatten))

;;;;;;; END PRIMARY ROUTINE ;;;;;;;;

(PRINC)