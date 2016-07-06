(in-package :kt-opengl)

;;  GL utils
;;

;;  get parameters
;;
(defun gl-get-integers (size pname)
  (let ((params (cffi-uffi-compat:allocate-foreign-object :int size))
	results)
    (gl-get-integerv pname params)
    (setf results (loop for i from 0 to (1- size)
                      collecting (cffi-uffi-compat:deref-array params '(:array :int) i)))
    (cffi-uffi-compat:free-foreign-object params)
    results))

(defun gl-get-floats (size pname)
  (let ((params (cffi-uffi-compat:allocate-foreign-object :float size))
	results)
    (gl-get-floatv pname params)
    (setf results
      (loop for i from 0 to (1- size) 
          collecting (cffi-uffi-compat:deref-array params '(:array :float) i)))
    (cffi-uffi-compat:free-foreign-object params)
    results))

(defun gl-get-doubles (size pname)
  (let ((params (cffi-uffi-compat:allocate-foreign-object :double size))
	results)
    (gl-get-doublev pname params)
    (setf results (loop for i from 0 to (1- size)
                      collecting (cffi-uffi-compat:deref-array params '(:array :double) i)))
    (cffi-uffi-compat:free-foreign-object params)
    results))


(defmacro with-gl-parami ((vars pname) &body body)
  `(destructuring-bind ,vars (gl-get-integers ,(length vars) ,pname)
     ,@body)
  )
(defmacro with-gl-paramf ((vars pname) &body body)
  `(destructuring-bind ,vars (gl-get-float ,(length vars) ,pname)
     ,@body)
  )
(defmacro with-gl-paramd ((vars pname) &body body)
  `(destructuring-bind ,vars (gl-get-double ,(length vars) ,pname)
     ,@body)
  )

;; pass an array of numbers
;;
(defmacro with-gl-integers ((obj form) &body body)
  (let ((value (gensym)))
    `(let* ((,value ,form)
	    (,obj (cffi-uffi-compat:allocate-foreign-object :int (length ,value))))
       (dotimes (i (length ,value))
	 (setf (cffi-uffi-compat:deref-array ,obj '(:array :int) i) (elt ,value i)))
       ,@body
       (cffi-uffi-compat:free-foreign-object ,obj))
  ))

(defmacro with-gl-floats ((obj form) &body body)
  (let ((value (gensym)))
    `(let* ((,value ,form)
	    (,obj (cffi-uffi-compat:allocate-foreign-object :float (length ,value))))
       (dotimes (i (length ,value))
	 (setf (cffi-uffi-compat:deref-array ,obj '(:array :float) i) (elt ,value i)))
       ,@body
       (cffi-uffi-compat:free-foreign-object ,obj))
  ))

(defmacro with-gl-doubles ((obj form) &body body)
  (let ((value (gensym)))
    `(let* ((,value ,form)
	    (,obj (cffi-uffi-compat:allocate-foreign-object :double (length ,value))))
       (dotimes (i (length ,value))
	 (setf (cffi-uffi-compat:deref-array ,obj '(:array :double) i) (elt ,value i)))
       ,@body
       (cffi-uffi-compat:free-foreign-object ,obj))
  ))

;; create a display list
;;
(defmacro with-display-list (size &body body)
  (let ((var (gensym)))
    `(let ((,var (gl-gen-lists ,(car size))))
       (gl-new-list ,var GL_COMPILE)
       ,@body
       (gl-end-list)
       ,var)
    ))
