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

(defun ix-render-light (self)
  (trc nil "lightinst" (id self) (^enabled) (ff-list (^pos) :float 3)
    (^ambient)(^diffuse)(^specular))
  (if (^enabled)
      (progn
        (gl-enable (id self))
        (gl-lightfv (id self) gl_position (^pos))
        (gl-lightfv (id self) gl_ambient (^ambient))
        (gl-lightfv (id self) gl_diffuse (^diffuse))
        (gl-lightfv (id self) gl_specular (^specular)))
    (gl-disable (id self)))

;;;
;;;  #+(or) (destructuring-bind ( x . y)
;;;      (^SPOT-DIR)
;;;    (let ((d (make-floatv x y 1)))
;;;      (gl-Lightfv (id self) GL_SPOT_DIRECTION d)
;;;      (fgnfree d)))
;;;
  (gl-lighti (id self) gl_spot_cutoff (^cutoff))
  (gl-lighti (id self) gl_spot_exponent (^spot-exp))
  )

;;----------------------------------------------

(defmodel ogl-lit-scene () ;; mix in with ix-family
  (
   (clear-rgba :cell nil :initarg :clear-rgba :initform nil :accessor clear-rgba)
   (light-model :initarg :light-model :initform (list (cons gl_light_model_ambient *dim*))
     :accessor light-model)
   (lights :initarg :lights :accessor lights
     :initform nil #+refactor (c? (without-c-dependency
                    (let (lights)
                      (fm-traverse self (lambda (self)
                                          (when (typep self 'ix-light)
                                            (push self lights))))
                      (nreverse lights)))))
   (fixed-lighting :initform nil :initarg :fixed-lighting :accessor fixed-lighting)
   (emergency-lighting :initarg :emergency-lighting :accessor emergency-lighting
     :owning t
     :initform (list (make-instance 'light
                       :id gl_light6
                       :enabled t
                       :pos (make-ff-array :float  0 0 (nearer 1200) 1) ;; 200 (downs 300)
                       :ambient *dim*
                       :diffuse *bright*
                       :specular *bright*)
                 (make-instance 'light
                         :id gl_light1
                         :enabled t
                         :pos (make-ff-array :float 0 0 (nearer 100) 1) ;;  700 (downs 600) 
                         :ambient *dim*
                         :diffuse *dim* ;; *average*
                         :specular *bright*)))))

(defmethod ix-paint :before ((self ogl-lit-scene))
  (gl-enable gl_color_material)
  (when (eql :on (lighting self))
    (trc nil "lighting on!" self)
    (gl-enable gl_lighting))

  (dolist (lm (light-model self))
    ;(trc "lighting model!" self lm)
    (gl-light-modelfv (car lm)(cdr lm)))
  
  (gl-enable gl_auto_normal)
  (gl-enable gl_normalize)
           
  (let (lights)
    ;; /// next bit should not descend into a nested lit scene
    #+refactorifneeded
    (fm-traverse self (lambda (self)
                        (when (typep self 'ix-light)
                          (setf lights (or lights (^enabled)))
                          (ix-render-light self))))
    (loop for light in (fixed-lighting self)
          do (ix-render-light light))
    (when (not lights)
      (dolist (e-light (emergency-lighting self))
        (ix-render-light e-light)))))

(defun pct-xlate (pct v1 v2 expansion)
  (let* ((dv (round (- v2 v1) 2))
         (xv1 (- v1 (* expansion dv)))
         (xv2 (+ v2 (* expansion dv))))
    (+ xv1 (* pct (- xv2 xv1)))))

