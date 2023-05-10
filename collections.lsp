(load "libwdb.o")
(import "matrix")
(load "geometry.lsp")

(defun threading-tool (name vertex major-radius direction pitch step-length tool-type)
  (let* ((unit-direction (matrix::normalize direction))
         (directional-pitch (matrix::mult unit-direction pitch))
         (perpendicular-radius (matrix::sub major-radius 
                                            (matrix::mult (matrix::dot unit-direction major-radius) 
                                                          unit-direction)))
         (unit-radius (matrix::normalize perpendicular-radius))
         (major-diameter (matrix::norm (double perpendicular-radius)))
         (thread-height (* pitch 
                           (quotient (sqrt 3) 
                                     2)))
         (minor-diameter (- major-diameter 
                            (quotient (* 5 
                                         (sqrt 3) 
                                         pitch)
                                      8)))
         (effective-pitch-diameter (- major-diameter 
                                      (quotient (* 3 
                                                   (sqrt 3) 
                                                   pitch)
                                                8)))
         (internal-peak (- (half effective-pitch-diameter) 
                           (half thread-height)))
         (external-peak (+ (half effective-pitch-diameter)
                           (half thread-height))))
    (labels ((external-peak-point (distance)
               (helix vertex 
                      (matrix::mult unit-radius external-peak) 
                      unit-direction 
                      distance
                      pitch))
             (internal-peak-point (distance)
               (helix (matrix::sub vertex 
                                   (half directional-pitch))
                      (matrix::mult unit-radius internal-peak)
                      unit-direction
                      distance
                      pitch))
             (external-thread-profile (distance)
               (vector (internal-peak-point distance) 
                       (external-peak-point distance) 
                       (internal-peak-point (matrix::add distance 
                                                         pitch))))
             (internal-thread-profile (distance)
               (vector (external-peak-point distance)
                       (internal-peak-point distance)
                       (external-peak-point (matrix::add distance 
                                                         pitch)))))
        (flet ((make-threads (thread-profile)
                 (let ((thread-faces (map '<list>
                                          ;;TODO replace with linspace
                                          (lambda (x) (funcall thread-profile (matrix::norm x)))
                                          (point-interval +origin+ step-length direction)))) 
                   (make-combination 
                     "threads.c" 
                     (map '<general-vector> (lambda (current-face previous-face) 
                                              (make-arb6 "thread_segment.s" 
                                                         previous-face 
                                                         current-face))
                          (cdr thread-faces)
                          thread-faces)
                     'union
                     nil)))) 
          (cond ((equal tool-type 'tap)
                  (let ((trim-peaks (make-rcc "trim-peaks.s" 
                                              vertex 
                                              direction 
                                              (half minor-diameter))))
                    (make-combination
                      name
                      (vector 
                        (make-threads #'external-thread-profile)
                        trim-peaks)
                      'difference
                      nil)))
                ((equal tool-type 'die)
                  (let ((trim-peaks (make-rcc "trim-peaks.s" 
                                              vertex 
                                              direction 
                                              (half major-diameter))))
                    (make-combination
                      name
                      (vector
                        (make-threads #'internal-thread-profile)
                        trim-peaks)
                      'intersection
                      nil)))
                (t (error "tool type must be either 'tap or 'die")))))))
