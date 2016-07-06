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

(defmodel ct-jumper (control ix-view)())

(defun ix-slider-jumper-action (self e)
  (slider-set .parent
    (v2-in-rect-ratio
     (v2-in-rect (v2-subtract
                  (v2-xlate .w. self (evt-where e))
                  (mkv2 (round (l-width (nsib)) 2)
                    (downs (round (l-height (nsib)) 2))))
       (drag-range (nsib)))
     (drag-range (nsib)))))

(defmodel ix-slider (ix-family)
  ((initial-pcts :initarg :initial-pcts :accessor initial-pcts
     :initform (list (mkv2 .50 .50)))
   (thumb-size :initarg :thumb-size :accessor thumb-size
     :unchanged-if 'v2=
     :initform (mkv2 (u8ths 1) (u8ths 1)))
   (thumb-layers :initarg :thumb-layers :accessor thumb-layers
     :initform (with-layers (:out 24)
                 :on
                 +light-gray+
                 (:frame-3d :edge-raised
                   :thickness (u96ths 3))))
   (tracked-pct :initarg :tracked-pct :initform nil :accessor tracked-pct)
   (value-fn :initarg :value-fn :initform nil :accessor value-fn)
   (jumper-action :initarg :jumper-action :reader jumper-action
     :initform 'ix-slider-jumper-action)
   (jumper-layers :initarg :jumper-layers :reader jumper-layers
     :initform (with-layers +light-gray+ :on 
                 (:frame-3d :edge-raised
                   :thickness (u96ths 3)))))
  (:default-initargs
      :ll 0 :lt 0
    :value (c? (let ((vs (loop for k in (rest (^kids))
                                collecting (funcall (or (^value-fn) 'identity)
                                             (drag-pct k)))))
                    (if (cdr vs) vs (car vs))))
    :kids (c? (the-kids
               
                 (mk-part :bar (ct-jumper)
                   :px 0 :py 0
                   :ll 0 :lr (c? (l-width .parent))
                   :lt 0 :lb (c? (downs (l-height .parent)))
                   :skin (c? (skin .parent))
                   :pre-layer (c? (jumper-layers .parent))
                   :enabled (c? (null (cddr (kids .parent)))) ;; ie, only one dragger
                   :click-repeat-p t
                   :ct-action (jumper-action self))
                 
                 (loop for ipct in (^initial-pcts)
                     collecting (make-kid 'ct-drag
                                  :md-name (intern (concatenate 'string
                                                     (string (md-name self)) "-dragger"))
                                  :drag-pct (c-in ipct)
                                  :click-evt (c-in nil)
                                  :lighting :on
                                  :ll 0 :lr (c? (v2-h (thumb-size .parent)))
                                  :lt 0 :lb (c? (downs (v2-v (thumb-size .parent))))
                                  :pre-layer (c? (thumb-layers .parent))))))))


(defmethod ix-render-layer ((self (eql :slider-thumb)) lbox)
  (declare (ignore lbox))
  (make-instance 'frame-3d
                   :3dstyle :edge-raised
                   :thickness (u96ths 3)))

(defobserver tracked-pct ()
  (when new-value
    (slider-set self new-value)))
 
(defun make-slider (md-name &key (value-fn 'identity)
                     (initial-pcts (list (mkv2 .50 .50)))
                     (width (uin 1)) (height (u8ths 1)))
  (make-part md-name 'ix-slider
    :lr width :lb (downs height)
    :value-fn value-fn
    :initial-pcts initial-pcts))

(defun slider-set (self value)
  (assert (typep self 'ix-slider))
  (with-cc :slider-setf-drag
    (setf (drag-pct (second (kids self))) value)))