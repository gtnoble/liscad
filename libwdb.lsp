(c-option "-L/home/gtnoble/Software/BRL-CAD/brlcad/build/lib -I/home/gtnoble/Software/BRL-CAD/brlcad/include -lwdb -lbu -lbn -lrt")
(c-include "<stdlib.h>")
(c-include "<stdio.h>")
(c-include "<math.h>")
(c-include "\"vmath.h\"")
(c-include "\"bu/app.h\"")
(c-include "\"bn.h\"")
(c-include "\"raytrace.h\"")
(c-include "\"rt/geom.h\"")
(c-include "\"wdb.h\"")

(import "seq")
(import "macro")
(import "string")
(load "geometry.lsp")

;;; Counter for creating unique C variable names
(defdynamic *c-sym-count* 0)

;;; Creates a unique C variable name
(defun gensym-c ()
  (setf (dynamic *c-sym-count*) (+ (dynamic *c-sym-count*) 1))
  (convert (string-append "C_GEN_SYM" 
                 (convert (dynamic *c-sym-count*) <string>))
           <symbol>))

;;; Class for managing libwdb geometry database access
(defclass <wdb-database> ()
  ((ptr :accessor pointer :initform 0)
   (object-names :accessor wdb-object-names :initform (list))))

(defdynamic *current-wdb-db* (create (class <wdb-database>)))

;;; Is there an object in the currently open database with this name?
(defun wdb-object-namep (object-name)
  (some (lambda (name) (string= name object-name))
        (current-wdb-names)))

;;; Creates a unique database object name with prefix PREFIX
(defun available-object-name (prefix)
  (flet ((make-name (prefix replicate) (string-append prefix "-" (convert replicate <string>)))) 
    (let* ((replicate-count 0)
          (candidate-name (make-name prefix replicate-count))) 
      (while (wdb-object-namep candidate-name)
        (setf replicate-count (+ replicate-count 1))
        (setf candidate-name (make-name prefix replicate-count)))
      candidate-name)))

;;; Database file pointer for open database
(defun current-wdb-pointer ()
  (pointer (dynamic *current-wdb-db*)))

;;; List of names of all objects in the open database
(defun current-wdb-names ()
  (wdb-object-names (dynamic *current-wdb-db*)))

(defun add-object-name (name)
  (let ((db (dynamic *current-wdb-db*))) 
    (setf (wdb-object-names db)
        (cons name (wdb-object-names db)))))

;;; Open wdb database with filename FILENAME
(defun open-wdb-database (filename) 
  (c-lang "struct rt_wdb *db_fp;")
  (let ((res (create (class <wdb-database>))))
    (c-lang "db_fp = wdb_fopen(Fgetname(FILENAME));")
    (c-lang "char *fp_str;")
    (c-lang "fp_str = fast_sprint_hex_long(db_fp);")
    (setf (pointer res) (c-lang "Fmakefaststrlong(fp_str);"))
    (setf (dynamic *current-wdb-db*) res)
    res))

;;; Sets the INDEX element of C-vector with C name VECTOR-C-NAME to value of variable VALUE-VARIABLE
(defmacro set-c-vector-coordinate (vector-c-name index value-variable)
  (let* ((value-name-string (convert value-variable <string>))
        (c-expression (string-append vector-c-name "[" index "] = Fgetflt(" value-name-string ");")))
    `(c-lang ,c-expression)))

(defmacro let-c-double (c-name type value &rest body)
  (let* ((temp-lisp-name (gensym-c))
        (c-expression (string-append type " " c-name " = " 
                                     "Fgetflt(" (convert temp-lisp-name <string>) ");")))
    `(let ((,temp-lisp-name (convert ,value <float>)))
       (c-lang ,c-expression)
       ,@body)))

;;; Creates multiple C-doubles with a syntax similar to LET
(defmacro let-c-doubles (float-mapping &rest body)
  (reduce (lambda (body, float-params)
               `(let-c-double ,@float-params ,body))
             `(progn ,@body)
             float-mapping))

(defmacro let-c-double-array(array-c-name type vector number-of-elements &rest body)
  (let ((c-names (list))
        (c-double-initializer (list))
        (c-array-initializer (list)))
    (dotimes (i number-of-elements)
      (let* ((name (convert (gensym-c) <string>))
            (c-array-init-expression (string-append array-c-name "[" (convert i <string>) "] = " name ";")))
        (push name c-names)
        (push `(,name ,type (elt ,vector ,i)) c-double-initializer)
        (push `(c-lang ,c-array-init-expression) c-array-initializer)))
    (let ((c-array-declaration 
             (string-append type " " array-c-name "[" (convert number-of-elements <string>) "]" ";" )))
      `(let-c-doubles (,@c-double-initializer)
                      (c-lang ,c-array-declaration)
                      ,@c-array-initializer
                      ,@body))))


;;; Creates a 3D C-vector with C name VECTOR-C-NAME with Ctype TYPE. 
;;; The value of the elements are those in sequence SEQ
(defmacro let-c-vector (vector-c-name type seq &rest body)
  (let ((x-vector-name (gensym-c))
        (y-vector-name (gensym-c))
        (z-vector-name (gensym-c)))
    `(if (= (length ,seq) 3) 
         (let ((,x-vector-name (convert (elt ,seq 0) <float>))
               (,y-vector-name (convert (elt ,seq 1) <float>))
               (,z-vector-name (convert (elt ,seq 2) <float>)))
           (c-lang ,type)
           (c-lang " ")
           (c-lang ,vector-c-name)
           (c-lang ";")
           (set-c-vector-coordinate ,vector-c-name "0" ,x-vector-name)
           (set-c-vector-coordinate ,vector-c-name "1" ,y-vector-name)
           (set-c-vector-coordinate ,vector-c-name "2" ,z-vector-name)  
           ,@body)
         (error "Sequence must be of length 3!"))))

;;; Creates multiple C-vectors with a syntax similar to LET
(defmacro let-c-vectors (vector-mapping &rest body)
  (reduce (lambda (body, vector-pair)
               `(let-c-vector ,@vector-pair ,body))
             `(progn ,@body)
             vector-mapping))

;;; Creates a wdb object based on the C creation function name COMMAND
;;; PREFIX is the base name used to generate the created object name
;;; OPTIONS is the third through the last arguments to the c creation function
(defmacro make-wdb-object (command prefix &rest options)
  (let* ((generated-name (gensym-c))
        (generated-name-str (convert generated-name <string>))
        (generated-dbptr-name (gensym-c))
        (generated-dbptr-name-str (convert generated-dbptr-name <string>))
        (options-str (string::join options ", "))
        (c-expression (string-append "res = " command "(" 
                                     "Fgetlong(" generated-dbptr-name-str "), " 
                                     "Fgetname(" generated-name-str "), " options-str 
                                     ");")))
    `(let ((,generated-name (available-object-name ,prefix))
           (,generated-dbptr-name (current-wdb-pointer)))
       (c-lang ,c-expression)
       (add-object-name ,generated-name)
       ,generated-name)))

(defun unnest (vertices)
  (reduce 
    (lambda (accumulated vertex) (concatenate '<list> accumulated vertex)) 
    (list)
    vertices))

(defun make-arb8 (name quad1 quad2)
  (let* ((vertices (concatenate '<list> 
                                (polygonalize quad1) 
                                (polygonalize quad2)))
        (flat-vertices (unnest vertices))) 
    (if (= (length vertices) 8) 
      (let-c-double-array "flat_vertices" "fastf_t" flat-vertices 24
                          (make-wdb-object "mk_arb8" name "flat_vertices"))
      (error "Must have exactly 8 vertices"))))

(defun make-arb6 (name triangle1 triangle2)
  (let ((quad1 (concatenate '<general-vector> 
                            (vector (elt triangle1 0)) 
                            triangle1))
        (quad2 (concatenate '<general-vector> 
                            (vector (elt triangle2 0)) 
                            triangle2)))
    (make-arb8 name quad1 quad2)))

;;; Makes a sphere in the current database
;;; NAME is base name
;;; VERTEX is sphere vertex
;;; RADIUS is sphere radius
(defun make-sph (name vertex radius)
  (let-c-vector "vertex_point" "point_t" vertex
                (make-wdb-object "mk_sph" name "vertex_point" "Fgetflt(RADIUS)")))

;;; Makes a rectangular parallelpipid AKA rectangular prism in the current database
;;; NAME is base name
(defun make-rpp (name min-vertex max-vertex)
  (let-c-vectors (("min_vertex" "point_t" min-vertex) ("max_vertex" "point_t" max-vertex))
                  (make-wdb-object "mk_rpp" name "min_vertex" "max_vertex")))

;;; Makes a right circular cylinder in the current database
(defun make-rcc (name base-vertex height-vector radius)
  (let-c-vectors (("base" "point_t" base-vertex) ("height" "vect_t" height-vector))
                  (make-wdb-object "mk_rcc" name "base" "height" "Fgetflt(RADIUS)")))

(defun union-operation ()
  (c-lang "res = WMOP_UNION | INT_FLAG;"))

(defun subtract-operation ()
  (c-lang "res = WMOP_SUBTRACT | INT_FLAG;"))

(defun intersect-operation ()
  (c-lang "res = WMOP_INTERSECT | INT_FLAG;"))

;;; Appends an object to a combination list
(defun append-combination-list (element-name list-head operation)
  (let ((operation-code (cond ((eq operation 'union) (union-operation))
                               ((eq operation 'difference) (subtract-operation))
                               ((eq operation 'intersection) (intersect-operation))
                               (t (error "invalid operation selected")))))
    (c-lang "mk_addmember(Fgetname(ELEMENT_NAME), Fgetlong(LIST_HEAD), NULL, OPERATION_CODE & INT_MASK);")))

;;; Makes a combination
(defun make-combination (combination-name element-names operation is_region &rest region-parameters)
  (c-lang "struct wmember wm_hd;")
  (c-lang "BU_LIST_INIT(&wm_hd.l);")
  (c-lang "char *list_head_str;")
  (c-lang "list_head_str = fast_sprint_hex_long(&wm_hd.l);")
  (c-lang "unsigned char rgb[3] = {0, 0, 0};")
  (let ((list-head (c-lang "res = Fmakefaststrlong(list_head_str);"))
        (wdbptr (current-wdb-pointer)))
    (dolist (element-name (convert element-names <list>))
      (append-combination-list element-name list-head operation))
    (make-wdb-object "mk_lcomb" combination-name "&wm_hd" "0" "NULL" "NULL" "NULL" "0")
    ))
