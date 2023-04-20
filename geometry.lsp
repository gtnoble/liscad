(import "matrix")
(import "seq")

(defun translate (vertices direction-vector)
  (map '<general-vector> 
       (lambda (vertex) (matrix::add vertex direction-vector)) 
       vertices))

(defun unit-vector (axis)
  (cond ((equal axis 'x) #(1.0 0.0 0.0))
        ((equal axis 'y) #(0.0 1.0 0.0))
        ((equal axis 'z) #(0.0 0.0 1.0))))

(defun reflect-axis (axis vertices)
  (map '<general-vector> 
       (lambda (vertex) 
         (matrix::element-wise-product vertex 
                               (matrix::negate (unit-vector axis)))) 
       vertices))
