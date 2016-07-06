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

; ------------- Frame classes and mk-macros

(defclass frame-3d (frame-style ogl-node)
  ((edges :initarg :edges :accessor edges)
   (3dstyle :initarg :3dstyle :reader 3dstyle :initform :edge-raised)
   (thickness :initarg :thickness :initform (u8ths 1) :reader thickness)
   (texturing :initarg :texturing :initform nil :reader texturing)))

;;;(defclass Cylinder (ogl-quadric-based)
;;;  ((height :initarg :height :initform 1 :reader height)
;;;   (base-r :initarg :base-r :initform 1 :reader base-r)
;;;   (top-r :initarg :top-r :initform 1 :reader top-r)
;;;   (slices :initarg :slices :initform 10 :reader slices)
;;;   (stacks :initarg :stacks :initform 1 :reader stacks)
;;;   (rotation :initarg :rotation :initform nil :reader rotation)
;;;   )
;;;  (:default-initargs
;;;      :quadric (c? (glu-new-quadric))
;;;    ))

(defmacro mk-frame (&optional (edges :bf-rect))
   `(make-instance 'frame-3d :edges (logior :bf-mono ,edges) :3dstyle :edge-sunken))

(defmacro mk-frame-3d (3dstyle &optional (edges :bf-rect))
   `(make-instance 'frame-3d :edges ,edges :3dstyle ,3dstyle))

(let ((normalv (make-v3f)))
  (declare (ignorable normalv))
  (defun cgl-normal (key x1 y1 z1 x2 y2 z2 x3 y3 z3)
    (declare (ignorable key))
    (trc nil "send ncalc-normalf" x1 y1 z1 x2 y2 z2 x3 y3 z3)
    (multiple-value-bind (x y z)
        (ncalc-normalf x1 y1 z1 x2 y2 z2 x3 y3 z3)
      (trc nil "normal 3f" key x y z)
      (gl-normal3f x y z))))

(defmethod ix-layer-expand ((key (eql :frame-3d)) &rest args)
  ;(print (list key :args args))
  (destructuring-bind (&key thickness edges texturing) (cdr args)
    `(render-frame-3d l-box ,thickness ,edges ,(car args) ,texturing)))

(defmethod ix-layer-expand ((key (eql :sunken-frame)) &rest args)
  ;(print (list key :args args))
  `(render-frame-3d l-box ,(car args) nil :edge-sunken nil))

(defmethod ix-layer-expand ((key (eql :raised-frame)) &rest args)
  ;(print (list key :args args))
  `(render-frame-3d l-box ,(car args) nil :edge-raised nil))

(defun render-frame-3d (lbox thickness edges 3dstyle texturing)
  (declare (ignorable edges))
  (with-r-bounds (outl outt outr outb) lbox
    (let* (
           (thick thickness)
           (in (min 4 thick))
           (inl (+ outl thick))
           (int (+ outt (downs thick)))
           (inr (- outr thick))
           (inb (+ outb (ups thick)))
           (inz (ecase 3dstyle
                  (:edge-sunken (farther thick))
                  (:edge-raised (nearer thick)))))
      (destructuring-bind (&optional uface uback)
          texturing
        (declare (ignorable uback))
        (with-attrib (t gl_texture_bit gl_enable_bit gl_hint_bit gl_line_bit gl_color_buffer_bit)
          (if uface
              (progn ;;quick hack
                (trc nil "bingo frame3d texturing!!!!" uface (texture-name uface) (r-width lbox) (image-size uface))
                (ogl-tex-activate (texture-name uface))
                (ogl-tex-gen-setup gl_object_linear gl_modulate gl_repeat
                  .003
                  ;; (eko ("f3dscale") (/ 1 (/ (r-width lbox) (car ))))
                  :s :tee)
                )
            (progn
              (gl-disable gl_texture_2d)
              (gl-enable gl_lighting)))
          (flet
              ((vrto ()
                 (when uface ;; just treating it as a flag for "texture on"
                   (gl-tex-coord2f 1 1))
                 (gl-vertex3f outr outt 0))
               (vlto ()
                 (when uface
                   (gl-tex-coord2f 0 1))
                 (gl-vertex3f outl outt 0))
               (vlbo ()
                 (when uface
                   (gl-tex-coord2f 0 0))
                 (gl-vertex3f outl outb 0))
               (vrbo ()
                 (when uface
                   (gl-tex-coord2f 1 0))
                 (gl-vertex3f outr outb 0))
               (vlti ()
                 (when uface
                   (gl-tex-coord2f 0 1))
                 (gl-vertex3f inl int inz))
               (vlbi ()
                 (when uface
                   (gl-tex-coord2f 0 0))
                 (gl-vertex3f inl inb inz))
               (vrti ()
                 (when uface
                   (gl-tex-coord2f 1 1))
                 (gl-vertex3f inr int inz))
               (vrbi ()
                 (when uface
                   (gl-tex-coord2f 1 0))                   
                 (gl-vertex3f inr inb inz)))
            (flet ((render ()
                     (gl-translatef 0 0 (xlout thick))
                     (gl-enable gl_lighting)
                     (with-gl-begun (gl_quads)
                       ;; top
                       (cgl-normal :top
                         outr outt 0
                         outl outt 0
                         (+ outl in) (+ outt (downs in)) inz
                         )
                       (vlto)(vlti)(vrti)(vrto)
                       
                       ;; left
                       (cgl-normal :left outl outt 0
                         outl outb 0
                         (+ outl in) (+ outb (ups in)) inz)
                       (vlbo)(vlbi)(vlti)(vlto)
                       
                       ;; bottom
                       (cgl-normal :bottom outl outb 0
                         outr outb 0
                         (- outr in) (+ outb (ups in)) inz)
                       
                       (vlbi)(vlbo)(vrbo)(vrbi)
                       
                       ;; right
                       (cgl-normal :right
                         (- outr in) (+ outt (downs in)) inz
                         outr outb 0
                         outr outt 0)
                       (vrbi)(vrbo)(vrto)(vrti)
                       
                       ;; front
                       
                       #+nahhh ;; we're just doing the frame!
                       (progn
                         (cgl-normal :front
                           (- outr in) (+ outb (ups in)) inz
                           (- outr in) (+ outt (downs in)) inz
                           (+ outl in) (+ outt (downs in)) inz
                           )
                       
                         (vrti)(vlti)(vlbi)(vrbi))
                       
                       )
                     (gl-translatef 0 0 (xlout thick))))
              
              (render)
              (ogl::glec :f3d))))))))
  
#|
(defclass cone3d (frame-3d)())
  
(defmethod ix-render-layer ((self cone3d) lbox)
  ;
  ; we expect to have been gl-translated to the center of
  ; the lbox parameter
  ;
  (let* ((r (floor (r-width lbox) 2)))

    (gl-translatef 0 0 -100)
    (gl-rotatef 45 0 0 0)
    (gl-scalef 1.1 1.1 1.1)
    (glut-solid-cone r (* 4 r)
      9 1)
    
    (ogl::glec :f3d)))

(defclass sphere-3d (frame-3d)())

(defmethod ix-render-layer ((self sphere-3d) lbox)
  (let* ((r (floor (r-width lbox) 2)))
    (trc nil "drawing sphere radius" self lbox r)
    (gl-translatef 0 0 1000)
    (gl-scalef 1.1 1.1 1.1)
    (glut-solid-sphere (* 100 r) 9 1)
    (ogl::glec :f3d)))

|#