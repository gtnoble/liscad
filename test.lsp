(load "libwdb.o")
(load "geometry.lsp")
(import "matrix")

(open-wdb-database "derpo.g")


(make-combination 
  "comb.c" 
  (list (make-sph "sph.s" '(1.0 2.0 3.0) 3.0) 
        (make-rpp "block.s" '(-1.0 -1.0 -1.0) '(1.0 1.0 1.0))
        (make-rcc "cyl.s" '(0.0 0.0 0.0) '(0.0 0.0 1.0) 0.5)
        (let* ((bottom-quad (matrix::cartesian-product #(0 1) #(0 1) #(0)))
               (top-quad (translate bottom-quad #(0 0 1)))) 
          (make-arb8 "arb.s" bottom-quad top-quad))) 
  'union 
  nil)
