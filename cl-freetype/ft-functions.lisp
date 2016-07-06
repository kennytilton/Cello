(in-package :cl-freetype)

(defvar *ft-library* nil)


;;  Utility functions
;;
(defmacro with-ft-library ((library) &body body)
  `(progn
     (unless *ft-library*
       (error "ft-library is not initialied."))
     (let ((,library *ft-library*))
       ,@body
       ))
  )

(defmacro ft-call (&body body)
  `(let ((result (progn ,@body)))
     ; the result code FT_Error should be 0.
     (unless (or (null result) (eql 0 result))
       (error "ft-error: ~a in ~a" result (quote ,body)))))

#+xx
(initialize-ft)



(defun initialize-ft ()
  ;(print (list :initialize-ft-entry *ft-library*))
  (when (not *ft-library*)
    #+stillissue?
    (let ((path (freetype-dynamic-lib-path)))
      (assert path)
      (print `(:loading ,path))
      (trace load-foreign-library)
      ;;
      ;; next is ACL-specific
      ;; bigger fix is to use CFFI define-/use-foreign-library
      ;;

      (load (make-pathname
             :directory '(:absolute "StuckOnAlgebra XP" "application")
             :name "freetype6"
             :type "dll") :foreign t)
      (with-foreign-object (ptr 'ft-library)
        (ft-call (ft-init-freetype ptr))
        (setf *ft-library* (deref-pointer ptr 'ft-library)))
      (format t "Freetype initialized: version=~a~%" (get-ft-library-version))
      )
    
    (let ((path (freetype-dynamic-lib-path)))
      (assert path)
      (print `(:loading ,path))
      (trace load-foreign-library)
      (unless (load-foreign-library path)
        (print :wow)
        (format t "~&initialize-ft: cannot load library: ~a" path)
        (error "initialize-ft: cannot load library: ~a" path))
      (with-foreign-object (ptr 'ft-library)
               (ft-call (ft-init-freetype ptr))
               (setf *ft-library* (deref-pointer ptr 'ft-library)))
      (format t "Freetype initialized: version=~a~%" (get-ft-library-version))
      )))

#+test
(initialize-ft)

#+xx
(load "tcl85.dll")

#+test
(probe-file (freetype-dynamic-lib-path))

(defun done-ft ()
  (when *ft-library*
    (ft-call (ft-done-freetype *ft-library*))
    (setf *ft-library* nil)))


;;  APIs
;;
(defun get-ft-library-version ()
  (with-ft-library (library)
    (with-foreign-object (amajor 'ft-int)
      (with-foreign-object (aminor 'ft-int)
	(with-foreign-object (apatch 'ft-int)
	  (ft-call (ft-library-version library amajor aminor apatch))
	  (loop for pointer in (list amajor aminor apatch)
	     collecting (deref-pointer pointer 'ft-int)))))))

(defun get-new-face (font-file &optional (face-index 0))
  (with-ft-library (library)
    (with-foreign-object (ptr 'ft-face)
      (with-cstring (pathstr (namestring font-file))
	(ft-call (ft-new-face library pathstr face-index ptr))
        (deref-pointer ptr 'ft-face)))))


(defun get-new-memory-face (font-face &optional (face-index 0))
  (with-ft-library (library)
    (let ((buf (convert-to-foreign-string font-face)))
      (with-foreign-object (ptr 'ft-face)
	(ft-call (ft-new-memory-face library buf (length font-face) face-index ptr))
        (values (deref-pointer ptr 'ft-face) buf)))))

(defun done-face (face)
  (with-ft-library (library)
    (declare (ignore library))
    (ft-call (ft-done-face face))))

(defun set-char-size (face size64 res)
  (with-ft-library (library)
    (declare (ignore library))
    (ft-call (ft-set-char-size face 0 size64 res res))))

(defun set-pixel-sizes (face width height) 
  (with-ft-library (library)
    (declare (ignore library))
    (ft-call (ft-set-pixel-sizes face width height))))

(defun get-char-index (face charcode)
  (with-ft-library (library)
    (declare (ignore library))
    (ft-get-char-index face charcode)))

(defun get-kerning (face left-glyph right-glyph kern-mode)
  (with-ft-library (library)
    (declare (ignore library))
    (with-foreign-object (ptr 'ft-vector)
      (ft-call (ft-get-kerning face left-glyph right-glyph kern-mode ptr))
      (vector (ft-vector/x ptr)
	      (ft-vector/y ptr)))))

(defun load-glyph (face glyph-index load-flags)
  (with-ft-library (library)
    (declare (ignore library))
    (ft-call (ft-load-glyph face glyph-index load-flags))
    (ft-facerec/glyph face)))


(defun load-char (face char-code load-flags)
  (with-ft-library (library)
    (declare (ignore library))
    (ft-call (ft-load-char face char-code load-flags))
    (ft-facerec/glyph face)))

; FT_Outline_Get_CBox cannot be used with the current UFFI implementation,
; as it does not support getting the slot address of a structure.
;(defun get-outline-cbox (glyphslot)
;  (with-ft-library (library)
;    (ft-call (ft-outline-get-cbox (get-slot-address glyphslot 'outline) bbox)
;    ))

(defun render-glyph (glyphslot render-mode)
  (with-ft-library (library)
    (declare (ignore library))
    (ft-call (ft-render-glyph glyphslot render-mode))))

(defun get-first-char (face)
  (with-ft-library (library)
    (declare (ignore library))
    (with-foreign-object (agindex 'ft-uint)
      (values (ft-get-first-char face agindex)
	      (deref-pointer agindex 'ft-uint)))))

(defun get-next-char (face char-code)
  (with-ft-library (library)
    (declare (ignore library))
    (with-foreign-object (agindex 'ft-uint)
      (values (ft-get-next-char face char-code agindex)
	      (deref-pointer agindex 'ft-uint)))))

(defun get-postscript-name (face)
  (with-ft-library (library)
    (declare (ignore library))
    (convert-from-cstring (ft-get-postscript-name face))))
