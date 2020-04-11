;;-----------------------------------------------------------------------
;;
;;  Command Name - IsoBlock
;;      Routine For Transforming a Block to Isometric
;;      By WizMan_07Feb10
;;
;;  Version 1.0 - 11May09
;;  Version 1.1 - 06Feb10 - Added Reverse Option and Flatten(Express)
;;  Version 1.2 - 07Feb10 - Fixed DText Rotation inside block(by SEANT)
;;
;;
;;-----------------------------------------------------------------------
;;
;;
(defun c:isoblock (/          blok_ent
                   counter    ent_data
                   ent_pt     i
                   sub_func   *error*
                   blk_name   midtbox
                   midtxt     reverseflag
                   rot        tbox
                  )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun to_southwest (ent_name base_pt / obj)
        (vla-TransformBy
            (setq obj (vlax-ename->vla-object ent_name))
            (vlax-tmatrix
                (list
                    (list (/ (sqrt 2.) 2.) (- (/ (sqrt 2.) 2.)) 0. 0.)
                    (list (/ (sqrt (/ 2. 3.)) 2.)
                          (/ (sqrt (/ 2. 3.)) 2.)
                          (sqrt (/ 2. 3.))
                          0.
                    )
                    (list (- (/ (sqrt 3.) 3.))
                          (- (/ (sqrt 3.) 3.))
                          (/ (sqrt 3.) 3.)
                          0.
                    )
                    (list 0. 0. 0. 1.)
                )
            )
        )
        (vla-move obj
                  (vlax-3d-point
                      (trans (cdr (assoc 10 (entget ent_name))) ent_name 0)
                  )
                  (vlax-3d-point base_pt)
        )
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun to_southeast (ent_name base_pt / obj)
        (vla-TransformBy
            (setq obj (vlax-ename->vla-object ent_name))
            (vlax-tmatrix
                (list
                    (list (/ (sqrt 2.) 2.) (/ (sqrt 2.) 2.) 0. 0.)
                    (list (- (/ (sqrt (/ 2. 3.)) 2.))
                          (/ (sqrt (/ 2. 3.)) 2.)
                          (sqrt (/ 2. 3.))
                          0.
                    )
                    (list (/ (sqrt 3.) 3.)
                          (- (/ (sqrt 3.) 3.))
                          (/ (sqrt 3.) 3.)
                          0.
                    )
                    (list 0. 0. 0. 1.)
                )
            )
        )
        (vla-move obj
                  (vlax-3d-point
                      (trans (cdr (assoc 10 (entget ent_name))) ent_name 0)
                  )
                  (vlax-3d-point base_pt)
        )
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun to_front (ent_name base_pt / obj)
        (vla-TransformBy
            (setq obj (vlax-ename->vla-object ent_name))
            (vlax-tmatrix
                (list
                    (list 1. 0. 0. 0.)
                    (list 0. 0. 1. 0.)
                    (list 0. 1. 0. 0.)  ;mirrored
                    (list 0. 0. 0. 1.)
                )
            )
        )
        (vla-move obj
                  (vlax-3d-point
                      (trans (cdr (assoc 10 (entget ent_name))) ent_name 0)
                  )
                  (vlax-3d-point base_pt)
        )
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun to_front_southwest (ent_name base_pt / obj)
        (to_front ent_name base_pt)
        (to_southwest ent_name base_pt)
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun to_front_southeast (ent_name base_pt / obj)
        (to_front ent_name base_pt)
        (to_southeast ent_name base_pt)
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun dtr (var)
        (* PI (/ var 180.0))
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun fix_txt (blk oblang / ins)
        (vlax-for
                  obj
                     (vla-item (vla-get-Blocks doc) (cdr (assoc 2 (entget blk))))

            (if (eq "AcDbText" (vla-get-Objectname obj))
                (progn
                    (Setq ins (vlax-get obj 'insertionpoint))
                    (vla-put-upsidedown obj 0)
                    (vla-put-ObliqueAngle obj (dtr oblang))
                    (vlax-put obj 'insertionpoint ins)
                    (vla-update (vlax-ename->vla-object (entlast)))
                )
            )
        )
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (defun *error* (msg)
        (if blok_ent
            (progn
                (load "flattensup.lsp")
                (acet-flatn (ssadd blok_ent (ssadd)) nil)
                (cond ((= sub_func (quote to_front_southwest))
                       (fix_txt (entlast) 30)
                      )
                      ((= sub_func (quote to_front_southeast))
                       (fix_txt (entlast) 330)
                      )
                      (t nil)
                )
                (if reverseflag
                    (vlax-for
                              obj
                                 (vla-item (vla-get-Blocks doc) blk_name)

                        (if (eq "AcDbText" (vla-get-Objectname obj))
                            (vla-rotate obj (vlax-3d-point midtxt) pi)
                        )
                    )
                )
                (setq reverseflag nil)
            )
        )
        (and doc (vla-endundomark doc))
        (setvar 'cmdecho 1)
    )
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (setq doc (vla-get-activedocument
                  (vlax-get-acad-object)
              )
    )
    (vla-EndUndoMark doc)
    (vla-StartUndoMark doc)
    (setvar 'cmdecho 0)
    ;;
    ;;--------------------------------------------------------------------
    ;;
    (if (setq blok_ent (car (entsel "\n>>>...Pick a block...>>>: ")))
        (progn
            (setq ent_data (entget blok_ent))
            (setq ent_pt (cdr (assoc 10 ent_data)))
            (setq blk_name (cdr (assoc 2 ent_data)))
            (to_southwest blok_ent ent_pt)

            (setq counter 1)
            (while (or (= (setq i  (strcase
                                       (getstring
                                           "\rPress [SpaceBar] to Toggle View, [R]everse or Press any Letter to exit: "
                                       )
                                   )
                          )
                          ""
                       )
                       (= i "R")

                   )
                (if (/= i "R")
                    (progn
                        (if blok_ent
                            (vla-delete (vlax-ename->vla-object blok_ent))
                        )
                        (setq sub_func
                                 (nth counter
                                      '(to_southwest
                                        to_southeast
                                        to_front_southwest
                                        to_front_southeast
                                       )
                                 )
                        )
                        (entmake ent_data)
                        (setq blok_ent (entlast))

                        (if reverseflag
                            (vlax-for
                                      obj
                                         (vla-item (vla-get-Blocks doc)
                                                   (cdr (assoc 2 (entget blok_ent)))
                                         )

                                (if (eq "AcDbText" (vla-get-Objectname obj))
                                    (progn
                                        (vla-rotate obj (vlax-3d-point midtxt) pi)
                                        (vla-update (vlax-ename->vla-object blok_ent))
                                    )
                                )
                            )
                        )


                        ((eval sub_func) blok_ent ent_pt)
                        (if (< counter 3)
                            (setq counter (1+ counter))
                            (setq counter 0)
                        )
                        (setq reverseflag nil)
                    )
                    (if (not reverseflag)
                    (progn
                        (setq reverseflag t)
                        (setq rot (vla-get-rotation (vlax-ename->vla-object blok_ent)))
                        (vla-put-rotation
                            (vlax-ename->vla-object blok_ent)
                            (+ rot pi)
                        )
                        (vlax-for
                                  obj
                                  (vla-item (vla-get-Blocks doc) (cdr (assoc 2 (entget blok_ent))))

                            (if (eq "AcDbText" (vla-get-Objectname obj))
                                (progn
                                    (setq ins (vlax-get obj 'insertionpoint))
                                    (setq tbox (textbox (entget (vlax-vla-object->ename obj))))
                                    (setq midtbox (mapcar '/
                                                          (mapcar '+ (car tbox) (cadr tbox))
                                                          '(2. 2. 2.)
                                                  )
                                    )
                                    (setq midtxt (mapcar '+ ins midtbox))
                                    (vla-rotate obj (vlax-3d-point midtxt) pi)
                                    (vla-update (vlax-ename->vla-object blok_ent))
                                )
                            )
                        )
                    )
                        )
                )
            )

        )
    )
    (*error* "")
    (princ)
)
(vl-load-com)
;;
;;--------------------------------------------------------------------
;;
(prompt
    "\n>>>...IsoBlock.lsp is now loaded. Type 'IsoBlock' to start ...<<<"
) ;_ prompt
(princ)
;;--------------------------------------------------------------------
;;
;;WIZ_07FEB10