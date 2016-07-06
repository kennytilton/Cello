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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(   
            defun-ffx defun-ffx-multi
               dffr
             dfc
             dft
             dfenum
             make-ff-pointer
             ff-pointer-address
             )))

(defun ff-pointer-address (ff-ptr)
  #-lispworks ff-ptr
  #+lispworks (fli:pointer-address ff-ptr))

;;;(defun make-ff-pointer (n)
;;;  #-lispworks
;;;  n
;;;  #+lispworks
;;;  (fli:make-pointer :address n :pointer-type '(:pointer :void)))

(defun make-ff-pointer (n)
  #+lispworks (fli:make-pointer :address n :pointer-type  '(:pointer :void))
  #+clisp (ffi:unsigned-foreign-address n)
  #-(or clisp lispworks) n
  )

(defmacro defun-ffx (rtn module$ name$ (&rest type-args) &body post-processing)
  (declare (ignore module$))
  (let* ((lisp-fn (lisp-fn name$))
         (lispfn (intern (string-upcase name$)))
         (var-types (let (args)
                      (assert (evenp (length type-args)) () "uneven arg-list for ~a" name$)
                      (dotimes (n (floor (length type-args) 2) (nreverse args))
                        (let ((type (elt type-args (* 2 n)))
                              (var (elt type-args (1+ (* 2 n)))))
                          (when (eql #\* (elt (symbol-name var) 0))
                            ;; no, good with *: (setf var (intern (subseq (symbol-name var) 1)))
                            (setf type :pointer))
                          (push (list var type) args)))))
         (cast-vars (mapcar (lambda (var-type)
                             (copy-symbol (car var-type))) var-types)))
    `(progn
       (cffi:defcfun (,name$ ,lispfn) ,(if (and (consp rtn) (eq '* (car rtn)))
                                           :pointer rtn)
         ,@var-types)
                     
       (defun ,lisp-fn ,(mapcar #'car var-types)
         (let ,(mapcar (lambda (cast-var var-type)
                         `(,cast-var ,(if (listp (cadr var-type))
                                         (car var-type)
                                       (case (cadr var-type)
                                         (:int `(coerce ,(car var-type) 'integer))
                                         (:long `(coerce ,(car var-type) 'integer))
                                         (:unsigned-long `(coerce ,(car var-type) 'integer))
                                         (:unsigned-int `(coerce ,(car var-type) 'integer))
                                         (:float `(coerce ,(car var-type) 'float))
                                         (:double `(coerce ,(car var-type) 'double-float))
                                         (:string (car var-type))
                                         (:pointer (car var-type))
                                         (otherwise
                                          (let ((ffc (get (cadr var-type) 'ffi-cast)))
                                            (assert ffc () "Don't know how to cast ~a" (cadr var-type))
                                            `(coerce ,(car var-type) ',ffc)))))))
                 cast-vars var-types)
           (prog1
               (,lispfn ,@cast-vars)
             ,@post-processing)))
       (eval-when (compile eval load)
         (export '(,lispfn ,lisp-fn))))))

#+precffi
(defmacro defun-ffx (rtn module$ name$ (&rest type-args) &body post-processing)
  (let* ((lisp-fn (lisp-fn name$))
         (lispfn (intern (string-upcase name$)))
         (var-types (let (args)
                      (assert (evenp (length type-args)) () "uneven arg-list for ~a" name$)
                      (dotimes (n (floor (length type-args) 2) (nreverse args))
                        (let ((type (elt type-args (* 2 n)))
                              (var (elt type-args (1+ (* 2 n)))))
                          (when (eql #\* (elt (symbol-name var) 0))
                            ;; no, good with *: (setf var (intern (subseq (symbol-name var) 1)))
                            (setf type `(* ,type)))
                          (push (list var type) args)))))
         (cast-vars (mapcar (lambda (var-type)
                             (copy-symbol (car var-type))) var-types)))
    `(progn
       (def-function (,name$ ,lispfn) ,var-types
         :returning ,rtn
         :module ,module$)
                     
       (defun ,lisp-fn ,(mapcar #'car var-types)
         (let ,(mapcar (lambda (cast-var var-type)
                         `(,cast-var ,(if (listp (cadr var-type))
                                         (car var-type)
                                       (case (cadr var-type)
                                         (:int `(coerce ,(car var-type) 'integer))
                                         (:long `(coerce ,(car var-type) 'integer))
                                         (:unsigned-long `(coerce ,(car var-type) 'integer))
                                         (:unsigned-int `(coerce ,(car var-type) 'integer))
                                         (:float `(coerce ,(car var-type) 'float))
                                         (:double `(coerce ,(car var-type) 'double-float))
                                         (:string (car var-type))
                                         (otherwise
                                          (let ((ffc (get (cadr var-type) 'ffi-cast)))
                                            (assert ffc () "Don't know how to cast ~a" (cadr var-type))
                                            `(coerce ,(car var-type) ',ffc)))))))
                 cast-vars var-types)
           (prog1
               (,lispfn ,@cast-vars)
             ,@post-processing)))
       (eval-when (compile eval load)
         (export '(,lispfn ,lisp-fn))))))

(defmacro defun-ffx-multi (rtn module$ &rest name-sig-pairs)
  (assert (evenp (length name-sig-pairs)))
  `(progn
     ,@(loop for name in name-sig-pairs by #'cddr
          and sig in (cdr name-sig-pairs) by #'cddr
          collecting `(defun-ffx ,rtn ,module$ ,name ,sig))))

(defmacro dffr (rtn name$ (&rest type-args))
  `(dff ,rtn ,name$ ,type-args) nil)

(defmacro dfc (sym value)
  `(progn
     (defconstant ,sym ,value)
     (eval-when (compile eval load)
       (export ',sym))))

(defmacro dfenum (remark &rest enum-defs)
  (declare (ignore remark))
  (let ((num -1))
    `(progn
       ,@(loop for edef in enum-defs
            collecting (if (consp edef)
                           `(dfc ,(car edef) ,(setf num (cadr edef)))
                         `(dfc ,edef ,(incf num)))))))

(defmacro dft (ctype ffi-type ffi-cast)
  `(progn
     (setf (get ',ctype 'ffi-cast) ',ffi-cast)
     (defctype ,ctype ,ffi-type)
     (eval-when (compile eval load)
       (export ',ctype))))

;------ lisp-fn ------------------

(defun lisp-fn (n$)
  (intern
   (with-output-to-string (ln)
    (loop with n$len = (length n$)
        for n upfrom 0
        for c across n$
        when (and (plusp n)
               (upper-case-p c)
               (or (lower-case-p (elt n$ (1- n)))
                 (unless (>= (1+ n) n$len)
                   (lower-case-p (elt n$ (1+ n))))))
        do (princ #\- ln)
        do (princ (char-upcase c) ln)))))
