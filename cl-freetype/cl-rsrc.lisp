;;;  cl-rsrc
;;;  MacOS resource fork handling
;;;

(defpackage #:cl-rsrc
  (:nicknames #:rsrc)
  (:use #:common-lisp)
  (:export #:open-rsrc-fork
	   #:close-rsrc-fork
	   #:with-rsrc-fork
	   #:list-resource-types
	   #:list-resource-by-type
	   #:find-resource-by-id
	   #:find-resource-by-name
	   #:get-resource-by-id
	   #:get-resource-by-name
	   ))

(in-package :cl-rsrc)


;;  resource-fork class
;;

(defclass resource-fork ()
  ((resource-map :accessor resource-map)
   (resource-attr :accessor resource-attr)
   (fstream :accessor fstream :initarg :fstream)
   ))

(defun readui8 (resfork)
  (read-byte (fstream resfork)))

(defun readui16 (resfork)
  (let ((a (make-array 2 :element-type 'unsigned-byte)))
    (read-sequence a (fstream resfork))
    (dpb (aref a 0) (byte 8 8) (aref a 1))))

(defun readui24 (resfork)
  (let ((a (make-array 3 :element-type 'unsigned-byte)))
    (read-sequence a (fstream resfork))
    (dpb (aref a 0) (byte 8 16) (dpb (aref a 1) (byte 8 8) (aref a 2)))))

(defun readui32 (resfork)
  (let ((a (make-array 4 :element-type 'unsigned-byte)))
    (read-sequence a (fstream resfork))
    (dpb (aref a 0) (byte 8 24) (dpb (aref a 1) (byte 8 16) (dpb (aref a 2) (byte 8 8) (aref a 3))))))

(defun readstr32 (resfork)
  (let ((a (make-array 4 :element-type 'unsigned-byte)))
    (read-sequence a (fstream resfork))
    (map 'string #'code-char a)))

(defun readstrn (resfork n)
  (let ((a (make-array n :element-type 'unsigned-byte)))
    (read-sequence a (fstream resfork))
    (map 'string #'code-char a)))

;; read-rsrc-map returns:
;;  (("type" (id1 "name1" attr1 offset1 size1)
;;           (id2 "name2" attr2 offset2 size2)
;;           ...)
;;   ("type" (id1 "name1" attr1 offset1 size1)
;;           (id2 "name2" attr2 offset2 size2)
;;           ...)
;;  )
(defun read-rsrc-map (resfork)
  (let* (offset-data offset-map offset-types offset-names num-types restypes)
    (file-position (fstream resfork) 0)
    (setf offset-data (readui32 resfork))
    (setf offset-map (readui32 resfork))
    (readui32 resfork) ; length-data
    (readui32 resfork) ; length-map

    (file-position (fstream resfork) offset-map)
    (readui32 resfork) (readui32 resfork) (readui32 resfork) (readui32 resfork) ; reserved
    (readui32 resfork) (readui16 resfork) ; reserved
    (setf (resource-attr resfork) (readui16 resfork))
    (setf offset-types (+ offset-map (readui16 resfork)))
    (setf offset-names (+ offset-map (readui16 resfork)))

    (file-position (fstream resfork) offset-types)
    (setf num-types (1+ (readui16 resfork)))

    (loop for i from 0 below num-types 
       do
       (let* ((name (readstr32 resfork))
	      (num (1+ (readui16 resfork)))
	      (offset (+ offset-types (readui16 resfork))))
					;(format t "type: ~a ~a ~a~%" name num offset)
	 (push (list name num offset) restypes)))
    
    (setf (resource-map resfork)
	  (loop for (name num offset) in restypes
	     collecting
	     (cons name 
		   (loop for i from 0 below num
		      collecting
		      (progn
			(file-position (fstream resfork) (+ offset (* i (+ 2 2 1 3 4))))
			(let* ((id (readui16 resfork))
			       (of-name1 (readui16 resfork))
			       (attr (readui8 resfork))
			       (ofst (+ offset-data (readui24 resfork)))
			       name1 size)
			  (when (/= #xffff of-name1)
			    (file-position (fstream resfork) (+ offset-names of-name1))
			    (let ((len (readui8 resfork)))
			      (setf name1 (readstrn resfork len))))
			  (file-position (fstream resfork) ofst)
			  (setf size (readui32 resfork))
			  (list id name1 attr (+ 4 ofst) size)
			  ))
		      ))))
    ))


;;  high level API
;;
(defun list-resource-types (resfork)
  (mapcar #'car (resource-map resfork)))

(defun list-resource-by-type (resfork type)
  (assoc type (resource-map resfork) :test #'equalp))

(defun find-resource-by-id (resfork type id)
  ; entry = (id name attr offset size)
  (let* ((types (list-resource-by-type resfork type))
	 (entry (and types (assoc id (cdr types)))))
    (and entry
	 (let ((name (elt entry 1))
	       (attr (elt entry 2))
	       (pos (elt entry 3))
	       (size (elt entry 4)))
	   (file-position (fstream resfork) pos)
	   (values size name attr)))))

(defun find-resource-by-name (resfork type name)
  ; entry = (id name attr offset size)
  (let* ((types (list-resource-by-type resfork type))
	 (entry (and types (find-if (lambda (e) (equal name (elt e 1))) (cdr types)))))
    (and entry
	 (let ((id (elt entry 0))
	       (attr (elt entry 2))
	       (pos (elt entry 3))
	       (size (elt entry 4)))
	   (file-position (fstream resfork) pos)
	   (values size id attr)))))

(defun get-resource-by-id (resfork type id)
  (let ((size (find-resource-by-id resfork type id)))
    (and size (readstrn resfork size))))

(defun get-resource-by-name (resfork type name)
  (let ((size (find-resource-by-name resfork type name)))
    (and size (readstrn resfork size))))

(defun open-rsrc-fork (file-name)
  (let ((resfork (make-instance 'resource-fork :fstream 
				(open file-name :direction :input :element-type 'unsigned-byte))))
    (read-rsrc-map resfork)
    resfork))

(defun close-rsrc-fork (resfork)
  (close (fstream resfork)))

(defmacro with-rsrc-fork ((resfork file-name) &body body)
  `(let ((,resfork (open-rsrc-fork ,file-name)))
     (prog1 (progn ,@body)
       (close-rsrc-fork ,resfork)))
  )


; test
(defun test ()
  (with-rsrc-fork (resfork #p"/Applications (Mac OS 9)/SimpleText/rsrc")
    (print (list-resource-types resfork))
    (print (list-resource-by-type resfork "MENU"))
    (print (multiple-value-list (find-resource-by-id resfork "MENU" 1)))
    (print (multiple-value-list (find-resource-by-id resfork "MENU" 128)))
    (print (multiple-value-list (find-resource-by-name resfork "MENU" "Copple")))
    (print (multiple-value-list (find-resource-by-name resfork "MENU" "Apple")))
    ))
#+test-rsrc
(cl-rsrc::test)
