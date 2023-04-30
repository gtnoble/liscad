(import "matrix")
(import "seq")

;;; TRANSLATE translates vertices in the direction of DIRECTION-VECTOR
(defun translate (vertices direction-vector)
  (map '<general-vector> 
       (lambda (vertex) (matrix::add vertex direction-vector)) 
       vertices))

(defun scale (vertices scaling-factor)
  (map '<general-vector>
       (lambda (vertex) (matrix::mult vertex scaling-factor))
       vertices))

;;; BASIS-VECTOR the basis vector corresponding to axis AXIS
;;; If SCALE is passed, the returned vector is scaled by factor SCALE
(defun basis-vector (axis &rest scale)
  (matrix::mult (cond ((equal axis 'x) #(1.0 0.0 0.0))
                      ((equal axis 'y) #(0.0 1.0 0.0))
                      ((equal axis 'z) #(0.0 0.0 1.0)))
                (if scale (car scale) 1)))

;;; FLIP-AXIS reflects vertices VERTICES across the coordinate plane that is perpendicular to axis AXIS
(defun flip-axis (axis vertices)
  (map '<general-vector> 
       (lambda (vertex) 
         (matrix::element-wise-product vertex 
                                       (matrix::negate (basis-vector axis)))) 
       vertices))

;;; DEGREES->RADIANS converts angle DEGREES in degrees to angle in radians
(defun degrees->radians (degrees)
  (* degrees (quotient *pi* 180.0)))

;;; ROTATE-ABOUT rotates vertices VERTICES about the axis defined by AXIS-VECTOR
;;; The angle of the rotation in degrees is specified by ANGLE-DEGREES
(defun rotate-about (axis-vector angle-degrees vertices)
  (let ((rotate-vertex (lambda (vertex) 
                         (let ((normalized-axis-vector (matrix::normalize axis-vector))
                               (angle (degrees->radians angle-degrees))) 
                           (matrix::add (matrix::mult vertex 
                                                      (cos angle))
                                        (matrix::mult (matrix::cross normalized-axis-vector vertex) 
                                                      (sin angle))
                                        (matrix::mult normalized-axis-vector 
                                                      (matrix::dot normalized-axis-vector vertex)
                                                      (- 1 (cos angle))))))))
    (map '<general-vector> rotate-vertex vertices)))

(defun half (x)
  (matrix::mult x 0.5))

(defun double (x)
  (matrix::mult x 2))
