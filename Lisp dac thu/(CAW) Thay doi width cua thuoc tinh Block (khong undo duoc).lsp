(defun c:CAW ( / blk idx obj sel tag wid )

   (setq blk "khung ten" ;; Block Name(s) - allows wildcards
         tag "TENBANVE2"   ;; Attribute Tag(s) to Modify - allows wildcards
         wid 0.5       ;; New Attribute Width
   )
   (if (setq sel (ssget "_X" (list '(0 . "INSERT") '(66 . 1) (cons 2 blk))))
       (repeat (setq idx (sslength sel))
           (if
               (or
                   (and
                       (vlax-property-available-p (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))) 'effectivename)
                       (wcmatch (strcase (vla-get-effectivename obj)) (strcase blk))
                   )
                   (wcmatch (strcase (vla-get-name obj)) (strcase blk))
               )
               (foreach att (vlax-invoke obj 'getattributes)
                   (if
                       (and
                           (wcmatch (strcase (vla-get-tagstring att)) (strcase tag))
                           (vlax-write-enabled-p att)
                       )
                       (vla-put-scalefactor att wid)
                   )
               )
           )
       )
   )
   (vlax-for def (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
       (if (wcmatch (strcase (vla-get-name def)) (strcase blk))
           (vlax-for obj def
               (if 
                   (and
                       (= "AcDbAttributeDefinition" (vla-get-objectname obj))
                       (wcmatch (strcase (vla-get-tagstring obj)) (strcase tag))
                       (vlax-write-enabled-p obj)
                   )
                   (vla-put-scalefactor obj wid)
               )
           )
       )
   )
   (princ)
)
(vl-load-com) (princ)