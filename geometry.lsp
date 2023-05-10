(import "matrix")
(import "seq")

(defconstant +origin+ #(0 0 0))

;;; TRANSLATE translates vertices in the direction of DISPLACEMENT
(defun translate (vertices displacement)
  (map '<general-vector> 
       (lambda (vertex) (matrix::add vertex displacement)) 
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

;;; ROTATE-VECTOR rotates VECTOR about AXIS-VECTOR by angle ANGLE in radians
(defun rotate-vector (axis-vector angle vector)
  (let ((normalized-axis-vector (matrix::normalize axis-vector))) 
    (matrix::add (matrix::mult vector 
                               (cos angle))
                 (matrix::mult (matrix::cross normalized-axis-vector vector) 
                               (sin angle))
                 (matrix::mult normalized-axis-vector 
                               (matrix::dot normalized-axis-vector vector)
                               (- 1 (cos angle))))))

;;; ROTATE-ABOUT rotates vertices VERTICES about the axis defined by AXIS-VECTOR
;;; The angle of the rotation in degrees is specified by ANGLE-DEGREES
(defun rotate-about (axis-vector angle-degrees vertices)
  (map '<general-vector> (lambda (vertex)
                           (let ((angle (degrees->radians angle-degrees)))
                             (rotate-vector axis-vector angle vertex))) 
       vertices))

(defun half (x)
  (matrix::mult x 0.5))

(defun double (x)
  (matrix::mult x 2))

;;; DISTANCE calculates pythagorean distance
(defun distance (vertex1 vertex2)
  (matrix::norm (matrix::sub vertex1 vertex2)))

;;; CENTERED-PAIR creates a pair of values that are PAIR-DISTANCE apart and reflected across the origin
(defun centered-pair (pair-distance)
  (let ((displacement-from-origin (half pair-distance))) 
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

;;; POLYGOLANLZE converts any sequence of vertices into a sequence of vertices represnting a polygon.
;;; The vertices are ordered such that if line segements were drawn to the next vertex in the sequence
;;; the resulting segments would form a not-intersecting polygon
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

;;; POLYGON-NORMAL takes a set of coplanar points that represent a polygon and calculates the unit normal vector
(defun polygon-normal (polygon)
  (let* ((triangle (subseq polygon 0 3))
         (reference-point (elt triangle 0)))
    (matrix::normalize 
      (matrix::cross (matrix::sub (elt triangle 1) reference-point)
                     (matrix::sub (elt triangle 2) reference-point)))))

;;; HOLE-CLEARANCE calculates the addidional length a cylinder needs for it to completely penetrate a plane,
;;; assuming that the cylinder base vertex is currently intersecting the plane
(defun hole-clearance (hole-direction plane-normal radius)
  (let* ((unit-normal (matrix::normalize plane-normal))
         (unit-direction (matrix::normalize hole-direction))
         (normal-dot-direction (matrix::dot unit-normal unit-direction)))
    (matrix::mult (abs (quotient (sqrt (- 1 (expt normal-dot-direction 2)))
                                 normal-dot-direction)) 
                  radius
                  unit-direction)))


(defun helix (vertex radius direction distance pitch)
  (let* ((unit-direction (matrix::normalize direction))
         (displacement (matrix::mult unit-direction distance))
         (perpendicular-radius (matrix::sub radius 
                                            (matrix::mult (matrix::dot unit-direction radius) 
                                                          unit-direction)))
         (angle (quotient distance 
                   (quotient pitch  (* 2 *pi*))))
         (rotated-radius (rotate-vector unit-direction angle perpendicular-radius)))
    (matrix::add vertex displacement rotated-radius)))

(defun point-interval (start increment-length end)
  (let* ((interval-length (distance start end))
         (unit-interval (matrix::normalize (matrix::sub end start)))
         (increment (matrix::mult unit-interval increment-length)))
    (for ((interval-elements (list start)))
         ((> (distance (car interval-elements) 
                       start)
             interval-length)
          (reverse (cdr interval-elements)))
         (setf interval-elements 
               (cons (matrix::add (car interval-elements) 
                                  increment) 
                     interval-elements)))))


