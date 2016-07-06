(in-package :ffx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(falloc)))

;;;(defun deref-array (array type position)
;;;  (mem-aref array type position))
;;;
;;;(defun (setf deref-array) (value array type position)
;;;  (setf (mem-aref array type position) value))

(defun falloc (type &optional (size 1))
  (cffi-uffi-compat:allocate-foreign-object type size))