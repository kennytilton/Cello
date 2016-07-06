;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
#|

Copyright (C) 2004 by Kenneth William Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cello)



(dft uint* :pointer :int)
(dft int* :pointer :int)

;;;#-lispworks
;;;(declaim (type int* *ix-select-r*))
(defparameter *ix-select-r* (fgn-alloc 'glint 4))

(defun view-b (n)
  (cffi:mem-aref *ix-select-r* 'glint n))

;;;#-lispworks
;;;(declaim (type uint* *ix-select-buffer*))

(defparameter *ix-select-buffer* (fgn-alloc 'gluint 512))
(defun buffy (y)
  (cffi:mem-aref *ix-select-buffer* 'gluint) y)

(defun ix-select (pos tolerance &key (select :nearest) (target ctk::*tkw*))           
  (declare (ignorable select pos tolerance))
  (gl-get-integerv gl_viewport *ix-select-r*)

  (print `(ix-select viewport ,(loop for n below 4 collecting (view-b n))))

  (gl-select-buffer 512 *ix-select-buffer*)
  (gl-render-mode gl_select)
  (gl-init-names)

  (gl-push-name 0)
  (gl-matrix-mode gl_projection)
  (gl-push-matrix)
  (gl-load-identity)

  #+(or) (when pos ;; pass nil to select all visible
    (glu-pick-matrix
     (v2-h pos) (ups (view-b 3) (v2-v pos)) ;;OQ: are GLUT mouse y's up or down?
     (v2-h tolerance) (v2-v tolerance) *ix-select-r*))

  (glu-pick-matrix
     500 500
     1000 1000 *ix-select-r*)

  #+(or) (let ((aspect (/ (- (view-b 2)(view-b 0))
                            (- (view-b 3)(view-b 1)))))
    ;;(format t "~&perspective sees aspect: ~a" aspect)
    (glu-perspective 45 aspect 0.1 100.0)) ;;OQ: appropriate for ortho?
  
  (gl-matrix-mode gl_modelview)
  #+(or) (let ((*ogl-listing-p* target)
        *selecting* *render-clip-l* *render-clip-r* *render-clip-t* *render-clip-b*)
    (with-metrics (nil nil "ix-paint" self)
      (ix-paint target)))
  (gl-call-list (dsp-list target))
    

  (gl-matrix-mode gl_projection) 
  (gl-pop-matrix)
  
  (gl-matrix-mode gl_modelview)

  (let ((hits (gl-render-mode gl_render)))
    (print `(:hits ,hits))
    (when (plusp hits)
      (print `(:got-hits ,hits))
      #+(or) (flet ((dist (n)
               (buffy (1+ (* n 4))))
             (self (n)
               (buffy (+ 3 (* n 4)))))
        (ecase select
          (:all (let ((names (maptimes (x (1- hits))  #'self )))
                  (fm-collect-if target (lambda (node)
                                          (member (gl-name node) names)))))
          (:nearest (let ((closest 0))
                      (dotimes (n (1- hits))
                        (let ((next (1+ n)))
                          (if (< (dist next) (dist closest))
                              (setf closest next))))
                      (values (fm-find-if target (lambda (node)
                                            (eql closest (gl-name node))))
                        (dist closest)))))))))

