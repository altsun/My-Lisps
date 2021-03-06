(defun C:BD (/ ang1 ang2 ent1 ent2 ent3 ep1 ep3 ipt1 ipt2 ipt21
    mp1 mp3 obj1 obj2 obj3 pt1 pt2 pt3 sp1 sp3)
(setq ent1 (entsel "\nSelect first line >>")
      ent2 (entsel "\nSelect second line >>")
      ent3 (entsel "\nSelect third line >>")
      obj1 (vlax-ename->vla-object (car ent1))
      obj2 (vlax-ename->vla-object (car ent2))
      obj3 (vlax-ename->vla-object (car ent3))
      )
(setq sp1  (vlax-curve-getstartpoint obj1)
      ep1  (vlax-curve-getendpoint obj1)
      mp1  (mapcar (function (lambda (a b) (/ (+ a b) 2))) sp1 ep1)
      sp3  (vlax-curve-getstartpoint obj3)
      ep3  (vlax-curve-getendpoint obj3)
      mp3  (mapcar (function (lambda (a b) (/ (+ a b) 2))) sp3 ep3)
      ipt1 (vlax-invoke obj1 'intersectwith obj3 0)
      ipt2 (vlax-invoke obj2 'intersectwith obj3 0)
      ang1 (angle ipt1 mp1)
      ang2 (angle ipt2 ipt1)
      pt1  (polar ipt1 ang1 100)
      pt2  (polar ipt2 ang1 100)
      pt3  (polar ipt1 ang2 100)
      )
     (command "_.break" ent1 "f" "_non" pt1 "_non" ipt1)
     (command "line" "_non" pt1 "_non" pt2 "")
     (command "line" "_non" pt1 "_non" pt3 "")
(princ)
)