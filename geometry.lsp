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

(defun distance (vertex1 vertex2)
  (matrix::norm (matrix::sub vertex1 vertex2)))

(defun centered-pair (x)
  (let ((displacement-from-origin (half x))) 
    (vector displacement-from-origin 
            (matrix::negate displacement-from-origin))))

(defun all-combinations-with-one-vertex-removed (vertices) 
  (labels ((one-vertex-removed (previous-vertices next-vertices)
             (cons (append previous-vertices
                           (cdr next-vertices))
                   (if (> (length next-vertices) 1) 
                       (one-vertex-removed (cons (car next-vertices) 
                                                 previous-vertices) 
                                           (cdr next-vertices))
                       nil))))
    (one-vertex-removed nil vertices)))

(defun polygonalize (vertices) 
  (labels ((make-path (current-length waypoints remaining-vertices)
             (list current-length waypoints remaining-vertices))
           (path-length (path) 
             (car path))
           (path-waypoints (path) 
             (elt path 1))
           (path-remaining-vertices (path)
             (elt path 2))
           (path-vertices-remainingp (path)
             (> (length (path-remaining-vertices path)) 0))
           (polygonalize-current-path (current-path)
             (if (path-vertices-remainingp current-path)
                 (labels ((make-step (vertex remaining) 
                            (cons vertex remaining))
                          (get-vertex (step) 
                            (car step))
                          (get-remaining (step) 
                            (cdr step))
                          (get-shortest-path (next-step)
                            (let ((previous-vertex (car (path-waypoints current-path)))) 
                              (polygonalize-current-path 
                                (make-path  (+ (path-length current-path) 
                                               (distance previous-vertex 
                                                         (get-vertex next-step)))
                                            (cons (get-vertex next-step) (path-waypoints current-path))
                                            (get-remaining next-step)))))) 
                   (let ((potential-steps (mapcar 
                                            #'make-step
                                            (path-remaining-vertices current-path) 
                                            (all-combinations-with-one-vertex-removed 
                                              (path-remaining-vertices current-path)))))
                     (reduce (lambda (shortest-path-yet candidate-step)
                               (let ((potential-next-path (get-shortest-path candidate-step)))
                                 (if (< (path-length potential-next-path) 
                                        (path-length shortest-path-yet))
                                     potential-next-path
                                     shortest-path-yet)))
                             (get-shortest-path (car potential-steps))
                             (cdr potential-steps))))
                 current-path
                 )))
    (let ((vertices-list (convert vertices <list>))) 
      (convert 
        (path-waypoints 
          (polygonalize-current-path 
            (make-path 0 
                       (list (car vertices-list)) 
                       (cdr vertices-list)))) 
        <general-vector>))))

(defun polygon-normal (polygon)
  (let* ((triangle (subseq polygon 0 3))
        (reference-point (elt triangle 0)))
    (matrix::normalize 
      (matrix::cross (matrix::sub (elt triangle 1) reference-point)
                     (matrix::sub (elt triangle 2) reference-point)))))

(defun hole-clearance (hole-direction plane-normal radius)
  (let* ((unit-normal (matrix::normalize plane-normal))
        (unit-direction (matrix::normalize hole-direction))
        (normal-dot-direction (matrix::dot unit-normal unit-direction)))
    (matrix::mult (abs (quotient (sqrt (- 1 (expt normal-dot-direction 2)))
                                 normal-dot-direction)) 
                  radius
                  unit-direction)))
