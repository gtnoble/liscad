(import "string")
(import "macro")

(defmacro c-pointer->number (c-expression)
  (let ((c-line (join 
                  "res = Fmakefaststrlong(fast_sprint_hex_long("
                  c-expression
                  "));")))
    `(c-lang ,c-line)))

(defun fill-c-double-array (array_address, seq)
  ;(assure <integer> array_address)
  (c-lang "double *c_array = Fgetlong(ARRAY_ADDRESS);")
    (for ((idx 0 (+ idx 1)))
         ((= idx (length seq)))
      (let ((element (elt seq idx)))
        (c-lang "c_array[IDXsubst & INT_MASK] = Fgetflt(ELEMENT);"))))

(defmacro let-c-double-array (vector-c-name type seq length &rest body)
  (assure <integer> length)
  (assure <string> vector-c-name)
  (assure <string> type)
  (let ((declaration (join type  " " vector-c-name "[" (to-string length) "]" ";" "")))
    `(progn (c-lang ,declaration)
            (fill-c-double-array (c-pointer->number ,vector-c-name) (subseq ,seq 0 length))
           ,@body)))

(defmacro let-c-vector (vector-c-name type seq &rest body)
  `(let-c-double-array type seq 3 ,@body))
