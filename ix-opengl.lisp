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

(export! .og.
	 .ogc.
	 ogl-shared-resource-tender
	 ogl-node
	 ogl-family
	 dsp-list)

(defmethod ogl-dsp-list-prep progn (self)
  (declare (ignore self))
  (assert (not *ogl-listing-p*)))

(defvar *ogl-shared-resource-tender*)

(defclass ogl-shared-resource-tender () ;; mixin
  ((display-lists :initform nil :accessor display-lists)
   (quadrics :initform nil :accessor quadrics)
   (textures :initform nil :accessor textures)))

(defmethod not-to-be :before ((self ogl-shared-resource-tender))
  (loop for (nil . dl) in (display-lists self)
        do (gl-delete-lists dl 1)
        finally (setf (display-lists self) nil))
  (loop for (nil . q) in (quadrics self)
        do (glu-delete-quadric q)))

(defmethod ogl-shared-resource-tender ((self ogl-shared-resource-tender))
  self)

(defmethod ogl-shared-resource-tender (other)
  (c-break "ogl-shared-resource-tender undefined for ~a" other))

(defmethod ogl-node-window (other)
  (c-break "ogl-node-window undefined for ~a" other))

(export! ogl-context ogl-isolate? visible ^visible dsp-list-formula)

(defmd ogl-node ()
  (ogl-context nil :cell nil)
  (ogl-isolate? nil :cell nil)
  (visible t)
  (dsp-list nil #+not (disp-lisp-formula))
  gl-name)

(defun dsp-list-formula ()
  #+bzzt
  (c-formula (:lazy :until-asked)
    (assert (not *ogl-listing-p*))
    (let ((dl-id (or .cache (gl-gen-lists 1))))
      (if (not (^visible))
          (progn
            #+nahh (when .cache
                     ;(trc "zapping invisible cache")
                     (gl-new-list dl-id gl_compile)
                     (gl-end-list)))
        (progn 
          (ogl-dsp-list-prep self)
          (glec :post-prep)
          (if t #+programmerbeware? (without-c-dependency
                  (every 'dsp-list (kids self)))
              (let ((*ogl-shared-resource-tender*
                     (ogl-shared-resource-tender self)))
                ;(trc "starting dl" dl-id self)
                (gl-new-list dl-id gl_compile)
                (let ((*ogl-listing-p* self)
                      *selecting* *render-clip-l* *render-clip-r* *render-clip-t* *render-clip-b*)
                  (with-metrics (nil nil "ix-paint" self)
                    (ix-paint self)))
                (glec :post-prep)
                (trc nil "---------------finished display list" dl-id self)
                (gl-end-list)
                (glec :post-prep)
                (setf (redisplayp .og.) t))
            (break "all or nothing on dsp-lists"))))
      dl-id)))

(defun render (self)
  (let (*selecting* *render-clip-l* *render-clip-r* *render-clip-t* *render-clip-b*)
    (ogl-echk :render)
    (with-metrics (nil nil "ix-paint" self)
      (trc nil "render" self (^height))
      (ix-paint self))))

(export! fully-visible)
(defgeneric fully-visible (self)
  (:method ((self ogl-node))
    (and (visible self)
      (or (null .pa)
        (fully-visible .pa))))
  (:method (other)
    (declare (ignore other))
    t))

(defmodel ogl-family () ;; mixin
  ()
  (:default-initargs
      :gl-name (c? (incf (gl-name-highest .w.)))
      :clipped nil))

(defobserver dsp-list ()
  (when old-value
    (gl-delete-lists old-value 1)))

(defmethod not-to-be :after ((self ogl-node))
  (bwhen (dl (slot-value self 'dsp-list)) ;; don't trigger lazy cell
    (gl-delete-lists dl 1)))



