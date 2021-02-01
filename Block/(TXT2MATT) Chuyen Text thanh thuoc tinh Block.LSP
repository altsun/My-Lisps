(defun c:txt2Matt ( / aDoc processedBlocks blocks2modify block2modify NewMultilineAttribute blockname)
	(setq aDoc (vla-get-ActiveDocument (vlax-get-acad-object)) TextCollection nil)
	    
(if  (setq blocks2modify (ssget "_:L" '((0 . "INSERT"))))      
      (repeat (setq i (sslength blocks2modify))
	(setq n 0 f nil 
              block2modify (vla-item (vla-get-blocks adoc)(setq blockname
                               (vla-get-effectivename (vlax-ename->vla-object (ssname blocks2modify (setq i (1- i))))))))
            (if (not (member blockname processedBlocks))
                	(progn
                              (setq processedBlocks (cons blockname processedBlocks))
                              
                              (while (and (null f)(< n (vla-get-count block2modify)))
                                    (if (member (setq texttype (vla-get-objectname (setq itm (vla-item block2modify n)))) '("AcDbMText" "AcDbText") )
                                        (setq f ( cons (mapcar '(lambda (s) (vlax-get itm s))
                   					'(TextString Height Rotation InsertionPoint StyleName))
                                                              	itm)))
                                        (setq n (1+ n))
                                    )
				(if f
                                    (progn
                                          (setq NewMultilineAttribute (vla-AddAttribute block2modify (cadar f) acAttributeModeMultipleLine
							"Multiples Lines" (vlax-3d-point (cadddr (Car f)))  "Multiples_Lines" (caar f )))
                                          
					  (vla-put-StyleName NewMultilineAttribute (nth 4 (Car f)))

				 	(if (eq texttype "AcDbMText")
				                  	(progn
                                                              (vla-put-MTextBoundaryWidth NewMultilineAttribute (vlax-get (Cdr f) 'width))
                                            		      (vla-put-TextAlignmentPoint NewMultilineAttribute (vlax-3d-point (cadddr (Car f))))
                                            				)
				          		(vla-put-ScaleFactor NewMultilineAttribute (vlax-get (Cdr f) 'ScaleFactor))
                                            )
				         
                                        (vla-delete (cdr f))
                                        (command "_attsync" "_Name" blockname)  
                                          )
                                    )
                              )
                	)
                )
                    )
      (princ)
)