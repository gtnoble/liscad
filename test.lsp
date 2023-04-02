(import "macro")
(load "libwdb.o")

(open-wdb-database "derpo.g")

(defdynamic *arb8-vertices* 
            (let ((vertices (list))) 
              (dotimes (x 2)
                (dotimes (y 2)
                  (dotimes (z 2)
                    (setf vertices (cons (list x y z) vertices)))))
              vertices))


(make-combination 
  "comb.c" 
  (list (make-sph "sph.s" '(1.0 2.0 3.0) 3.0) 
        (make-rpp "block.s" '(-1.0 -1.0 -1.0) '(1.0 1.0 1.0))
        (make-rcc "cyl.s" '(0.0 0.0 0.0) '(0.0 0.0 1.0) 0.5)
        (make-arb8 "arb.s" (dynamic *arb8-vertices*))
        ) 
  'union 
  nil)
