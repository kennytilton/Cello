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

;------------------------------------------------------------

(export! ix-polygon)
(defmodel ix-polygon (ix-view)
   ((fore-color :initarg :fore-color :initform +black+ :accessor fore-color)
    (poly-style :initarg :poly-style :initform nil :accessor poly-style)
    (poly-thickness :initarg :poly-thickness :initform (u96ths 1) :accessor poly-thickness)
    (poly-symmetry :initarg :poly-symmetry :initform nil :accessor poly-symmetry)
    (vertices :initarg :vertices :initform nil :accessor vertices)))

(defmethod ix-paint ((self ix-polygon))
  (let ((gh0 (ll self)) (gv0 (lt self)))
    (flet ((g2d (vertex) (mkv2 (+ (car vertex) gh0) (+ (cdr vertex) gv0)))
           (sym2d (vertex) (mkv2 (+ (cdr vertex) gh0) (+ (car vertex) gv0))))
      (let ((vs (if (poly-symmetry self)
                    (append (mapcar #'g2d (vertices self))
                            (nreverse (mapcar #'sym2d (vertices self))))
                  (mapcar #'g2d (vertices self)))))
        
        (with-matrix (nil)
          (gl-line-width  (poly-thickness self))
          (gl-polygon-mode gl_front_and_back gl_fill)
          (with-gl-begun (gl_triangles)
            (dolist (v vs)
              (gl-vertex3f (v2-h v) (v2-v v) 0)))
          (ogl::glec :f3d))))))

