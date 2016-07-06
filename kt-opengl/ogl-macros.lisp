;; -*- mode: Lisp; Syntax: Common-Lisp; Package: kt-opengl; -*-
;;________________________________________________________
;;
;;;
;;; Copyright (c) 2004 by Kenneth William Tilton.
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

(in-package :kt-opengl)

(defvar *stack-depth*
  (fgn-alloc :int 1 :ignore))

(defmacro with-matrix ((&optional load-identity-p) &body body)
  `(call-with-matrix ,load-identity-p (lambda () ,@body) ',body))

(defun call-with-matrix (load-identity-p matrix-fn matrix-code)
  (declare (ignorable matrix-code))
  (gl-push-matrix)  
  (unwind-protect
      (progn
        (when load-identity-p
          (gl-load-identity))
        (funcall matrix-fn))
    (gl-pop-matrix)))

#+debugversion
(defun call-with-matrix (load-identity-p matrix-fn matrix-code)
  (let* ((mm-pushed (get-matrix-mode))
        (sd-pushed (get-stack-depth mm-pushed)))
    (progn ;; cells:wtrc (0 100 "with-matrix starts with mode" (matrix-mode-symbol mm-pushed) :depth sd-pushed)
      (gl-push-matrix)
      (unwind-protect
          (progn
            (when load-identity-p
              (gl-load-identity))
            (prog1
                (funcall matrix-fn)
              (glec :with-matrix-body)))
        (assert (eql mm-pushed (get-matrix-mode))()
          "matrix-mode left as ~a  instead of ~a by form ~a"
          (ogl::get-matrix-mode) mm-pushed  matrix-code)
        (cells:trc nil "poppping matrix!!!!!" (matrix-mode-symbol (get-matrix-mode)) :from-depth (get-stack-depth (get-matrix-mode)))
        (gl-pop-matrix)
        (assert (eql sd-pushed (get-stack-depth mm-pushed))()
          "matrix depth deviated ~d during ~a"
          (- sd-pushed (get-stack-depth mm-pushed))
          matrix-code)
        (glec :exit-with-stack)))))

(defmacro with-attrib ((do-it? &rest attribs) &body body)
  `(call-with-attrib ,do-it?
    ,(apply '+ (mapcar 'symbol-value attribs))
    (lambda () ,@body)))

(defparameter *attrib-depth* 0) ;; only got 16 and ran out with complex interface automatically pushing

(defun call-with-attrib (do-it? attrib-mask attrib-fn)
  (if do-it?
      (progn
        (gl-push-attrib attrib-mask)
        (incf *attrib-depth*)
        (let ((e (glGetError)))
          (case e
            (0)
            (1283 (print `(pushattriboflowat ,*attrib-depth*))(break "ogl gl-push-attrib ~a" e))
            (otherwise (print `(pushattriberror ,e))(break "ogl gl-push-attrib ~a" e))))
  
        (unwind-protect
            (prog1
                (funcall attrib-fn)
              (glec :with-attrib))
          (gl-pop-attrib)
          (decf *attrib-depth*)
          (glec :with-attrib-pop)))
    (funcall attrib-fn)))

(defmacro with-client-attrib ((&rest attribs) &body body)
  `(call-with-client-attrib
    ,(apply '+ (mapcar 'symbol-value attribs))
    (lambda () ,@body)))

(defun call-with-client-attrib (attrib-mask attrib-fn)
  (gl-push-client-attrib attrib-mask)
  (glec :with-client-attrib-push)
  (unwind-protect
      (prog1
          (funcall attrib-fn)
        (glec :with-client-attrib))
    (gl-pop-client-attrib)
    (glec :with-client-attrib-pop)))

(defmacro with-ogl-isolation (do-it? &body body)
  `(with-attrib (,do-it? gl_lighting_bit gl_texture_bit gl_enable_bit gl_hint_bit gl_line_bit gl_color_buffer_bit)
     ,@body))

(defvar *gl-begun*)
(defvar *gl-stop*)

(defmacro with-gl-begun ((what) &body body)
  `(call-with-gl-begun ,what (lambda () ,@body)))

(defun call-with-gl-begun (what begun-fn)
  (when (boundp '*gl-begun*)
    (setf *gl-stop* t)
    (break ":nestedbegin"))
  (progn
    (glec :with-gl-begun-BEFORE)
    (let ((*gl-begun* t))
      (gl-begin what)
      (funcall begun-fn)
      (gl-end))
    (glec :with-gl-begun-exit)))

(export! ogl-echk)
(defun ogl-echk (dbg)
  (let ((e (glgeterror)))
    (unless (zerop e)
      (format t "~&Oglerr ~a at ~a" e dbg)
      #-its-alive!
      (break "Oglerr ~a at ~a" e dbg))))

(defmacro with-gl-translation ((dxf dyf &optional (dzf 0)) &body body)
  (let ((dx (gensym))(dy (gensym))(dz (gensym)))
    `(let ((,dx ,dxf)(,dy ,dyf)(,dz ,dzf))
       (gl-translatef ,dx ,dy ,dz)
       (prog1
           ,@body
         (gl-translatef (- ,dx)(- ,dy)(- ,dz))))))

(defun glec (&optional (id :anon) announce-success)
  (declare (ignorable id announce-success))
  #-its-alive! 
  (if (and (boundp '*gl-begun*) *gl-begun*)
      (progn (cells:trc nil "not checking error inside gl.begin" id))
    (let ((e (glgeterror)))
      (if (zerop e)
          (when announce-success
            (print `(OpenGL cool ,id)))
        (if t ;; (null (find id '(glutInitDisplayMode glutInitWindowSize)))
            (if (boundp '*gl-stop*)
                (cells:trc "error but *gl-stop* already bound" e id)
              (progn
                (setf *gl-stop* t)
                (format t "~&~%OGL error ~a at ID ~a" e id)
                ;(break "OGL error ~a at ID ~a" e id)
                ))
          #+sigh (print `("OGL error ~a at ID ~a" ,e ,id)))))))

