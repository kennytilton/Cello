;;;
;;; Copyright (c) 1995,2003 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.


(in-package :ffx)
  
(defparameter *gl-rsrc* nil)

(defparameter *fgn-mem* nil)

(defun fgn-dump ()
  (print (length *fgn-mem*))
  (loop for fgn in *fgn-mem*
        do (print fgn)
        summing (fgn-amt fgn)))

#+check
(fgn-dump)

(defun ffx-reset (&optional force)
  (hic-reset force))

(defun hic-reset (&optional force)
  (if force
      (progn
        (loop for fgn in *fgn-mem*
            do (print fgn)
              (foreign-free (fgn-ptr fgn))
            finally (setf *fgn-mem* nil))
        (loop for fgn in *gl-rsrc*
            do (print fgn)
              (glfree (fgn-type fgn)(fgn-ptr fgn))
            finally (setf *gl-rsrc* nil))
    (progn
      (when *fgn-mem*
        (loop for fgn in *fgn-mem*
            do (print fgn)
            finally (break "above fgn-mem not freed")))
      (when *gl-rsrc*
        (loop for fgn in *gl-rsrc*
            do (print fgn)
            finally (break "above *gl-rsrc* not freed")))))))

(defstruct fgn ptr id type amt)

(defmethod print-object ((fgn fgn) s)
  (format s "fgnmem ~a :amt ~a :type ~a"
    (fgn-id fgn)(fgn-amt fgn)(fgn-type fgn)))

(defmacro fgn-alloc (type amt-form &rest keys)
  (let ((amt (gensym))
        (ptr (gensym)))
    `(let* ((,amt ,amt-form)
            (,ptr (falloc ,type ,amt)))
       (call-fgn-alloc ,type ,amt ,ptr (list ,@keys)))))

(defun call-fgn-alloc (type amt ptr keys)
  ;;(print `(call-fgn-alloc ,type ,amt ,keys))
  (fgn-ptr (car (push (make-fgn :id keys
                        :type type
                        :amt amt
                        :ptr ptr)
                  *fgn-mem*))))

(defun fgn-free-all ()
  (loop for f in *fgn-mem* do
        (foreign-free (fgn-ptr f))
        finally (setf *fgn-mem* nil)))

#+go
(fgn-free-all)

(defun fgn-free (&rest fgn-ptrs)
  ;; (print `(fgn-free freeing ,@fgn-ptrs))
  (let ((start (copy-list fgn-ptrs)))
    (loop for fgn-ptr in start do
          (let ((fgn (find fgn-ptr *fgn-mem* :key 'fgn-ptr)))
            (if fgn
                (setf *fgn-mem* (delete fgn *fgn-mem*))
              (format t "~&Freeing unknown FGN ~a" fgn-ptr))
            (foreign-free fgn-ptr)))))

(defun gllog (type resource amt &rest keys)
  (push (make-fgn :id keys
          :type type
          :amt amt
          :ptr resource)
    *gl-rsrc*))

(defun glfree (type resource)
  (let ((fgn (find (cons type resource) *gl-rsrc*
               :test 'equal
               :key (lambda (g)
                      (cons (fgn-type g)(fgn-ptr g))))))
    (if fgn
        (setf *gl-rsrc* (delete fgn *gl-rsrc*))
      (progn
        ;(format t "~&ignoring unknown GL resource ~a" (cons type resource))
        #+not (ecase type
          (:texture (ogl:ogl-texture-delete resource)))))))

(defmacro make-ff-array (type &rest values)
  (let ((fv (gensym))(n (gensym))(vs (gensym)))
    `(let ((,fv (fgn-alloc ',type ,(length values) :make-ff-array))
           (,vs (list ,@values)))
       (dotimes (,n ,(length values) ,fv)
         (setf (ff-elt ,fv ,type ,n)
           (coerce (nth ,n ,vs) ',(if (keywordp type)
                                     (intern (symbol-name type))
                                   (get type 'ffi-cast))))))))

(defmacro ff-list (array type count)
  (let ((a (gensym))(n (gensym)))
  `(loop with ,a = ,array
       for ,n below ,count
       collecting (ff-elt ,a ,type ,n))))

(defun make-floatv (&rest floats)
  (let* ((co (fgn-alloc :float (length floats) :make-floatv))
         )
    (apply 'ff-floatv-setf co floats)))

(defmacro ff-floatv-ensure (place &rest values)
  `(if ,place
       (ff-floatv-setf ,place ,@values)
     (setf ,place (make-floatv ,@values))))

(defun ff-floatv-setf (array &rest floats)
  (loop for f in floats
        and n upfrom 0
        do (setf (mem-aref array :float n) (* 1.0 f)))
  array)

;--------- with-ff-array-elements ------------------------------------------


(defmacro with-ff-array-elements ((fa type &rest refs) &body body)
  `(let ,(let ((refn -1))
           (mapcar (lambda (ref)
                     `(,ref (mem-aref ,fa ,type) ,(incf refn)))
             refs))
     ,@body))

;-------- ff-elt ---------------------------------------

(defmacro ff-elt-p (v n)
  `(mem-aref ,v :pointer ,n))

(defmacro ff-elt (v type n)
  `(mem-aref ,v ',type ,n))

(defun elti (v n)
  (ff-elt v :int n))

(defun eltc (v n)
  (ff-elt v :char n))

(defun (setf elti) (value v n)
  (setf (ff-elt v :int n) (coerce value 'integer)))

(defun (setf eltuc) (value v n)
  (setf (ff-elt v :unsigned-char n) value))

(defun eltuc (v n)
  (declare (fixnum n))
  (ff-elt v :unsigned-char n))

(defun eltf (v n)
  (ff-elt v :float n))

(defun (setf eltf) (value v n)
  (setf (ff-elt v :float n) (coerce value 'float)))

(defun elt$ (v n)
  (ff-elt v :string n))

(defun (setf elt$) (value v n)
  (setf (ff-elt v :string n) value))

(defun eltd (v n)
  (ff-elt v :double n))

(defun (setf eltd) (value v n)
  (setf (ff-elt v :double n) (coerce value 'double-float)))

(defmacro fgn-pa (pa n)
  `(mem-aref ,pa :pointer ,n))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(ffx-reset eltc eltuc
           ff-elt ff-list
           eltf eltd elti fgn-pa
           with-ff-array-elements
           make-ff-array
           make-floatv ff-floatv-ensure
           hic-reset fgn-alloc fgn-free gllog glfree)))