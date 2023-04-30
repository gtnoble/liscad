(load "libwdb.o")
(load "geometry.lsp")
(import "matrix")

(defun inches->millimeters (millimeters)
  (* 25.4 millimeters))

(defconstant +mounting-hole-spacing-width+
             (inches->millimeters 1.625)
             )

(defconstant +old-school-hole-spacing-length+
             (inches->millimeters 2.5))

(defconstant +new-school-hole-spacing-length+
             (inches->millimeters 2.125))

(defconstant +hole-diameter+
             (inches->millimeters 0.215))

(defconstant +hole-to-edge-distance+
             (* +hole-diameter+ 1.5))

(open-wdb-database "riser.g")

(defun make-riser (base-height wedge-angle name)
  (flet ((base-points->top-points (points)
           (translate 
             (rotate-about (basis-vector 'x) 
                           wedge-angle 
                           points) 
             (basis-vector 'z base-height)))) 
    (let ((body 
            (let* ((base-width (+ +mounting-hole-spacing-width+ 
                                  (double +hole-to-edge-distance+)))
                   (base-x (half base-width))
                   (base-length (+ +old-school-hole-spacing-length+ 
                                   (double +hole-to-edge-distance+)))
                   (base-vertices (vector (vector base-x 0 0)
                                          (vector base-x base-length 0)
                                          (vector (- base-x) base-length 0)
                                          (vector (- base-x) 0 0)))
                   (top-vertices (base-points->top-points base-vertices)))
              (make-arb8 "body.s" base-vertices top-vertices)))
          (holes
            (let* ((base-hole-centers (translate 
                                        (matrix::cartesian-product  
                                          (half (vector (- +mounting-hole-spacing-width+) 
                                                        +mounting-hole-spacing-width+))
                                          (vector 0 
                                                  +old-school-hole-spacing-length+ 
                                                  +new-school-hole-spacing-length+)
                                          #(0))
                                        (basis-vector 'y +hole-to-edge-distance+)))
                   (top-hole-centers (base-points->top-points base-hole-centers))
                   (hole-directions (map '<general-vector> 
                                         (lambda (base-center top-center)
                                           (matrix::sub top-center base-center))
                                         base-hole-centers
                                         top-hole-centers)))
              (make-combination "holes.c"
                                (map '<general-vector> 
                                     (lambda (base-hole-center direction-vector)
                                       (let* ((hole-radius (half +hole-diameter+))
                                              (clearance-vector 
                                                (matrix::mult (matrix::normalize direction-vector) 
                                                              hole-radius)))
                                         (make-rcc "hole.s"
                                                   (matrix::sub base-hole-center clearance-vector) 
                                                   (matrix::add (double clearance-vector) 
                                                                direction-vector)
                                                   hole-radius)))
                                     base-hole-centers
                                     hole-directions)
                                'union
                                nil))))
      (make-combination name (list body holes) 'difference nil))))
