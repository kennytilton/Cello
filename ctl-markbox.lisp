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

(defparameter *mark-box-size* (u96ths 9))

(eval-now!
  (defmethod ix-layer-expand ((self (eql :x-mark)) &rest args)
    `(ix-render-x-mark ,(car args) l-box ,(second args) ,(third args))))

(defmd ct-mark-box (ct-toggle ix-view)
  :ll  (- *mark-box-size*)
  :lt  (ups  *mark-box-size*)
  :lr  *mark-box-size*
  :lb  (downs *mark-box-size*)
  :skin nil ;;(c? (skin .w.))
  :pre-layer (with-layers
                 (:in 4)
               :off
               +white+
               :fill
               (:frame-3d :edge-sunken :thickness 3)
               :off
               (:rgba (or (^fg-color) +black+))
               (:out 4)
               (:x-mark (^value))))

(defun ix-render-x-mark (do-p lbox &optional thickness inset
                          &aux (thick (or thickness (/ (r-width lbox) 4)))
                          (ins (or inset thick)))
  (when do-p
    (let* ((br (- (r-right lbox) ins)) ;; /// bogus use of thick to inset "x"
           (bl (+ (r-left lbox) ins))
           (bt (+ (r-top lbox) (downs ins)))
           (bb (+ (r-bottom lbox) (ups ins))))
      (with-matrix ()
        (gl-line-width (log2scr thick))
        (gl-disable gl_texture_2d)
        (with-gl-begun (gl_lines)
          (gl-vertex3f bl bt 0)(gl-vertex3f br bb 0)
          (gl-vertex3f bl bb 0)(gl-vertex3f br bt 0))
        (ogl::glec :f3d)))))

; -----   radios -------------------------------

(defmd ct-radio-item (ct-toggle)
  (kb-selector nil :cell nil)
  (already-on-do nil :cell nil)
  (radio (c? (upper self ct-radio)))
  :enabled t
  :value (c? (find (associated-value self) (value (^radio))))
  :ct-action (ct-action-lambda
              (with-cc :ct-radio-item
                (radio-item-to-value self event (^radio)))))

(defun radio-item-to-value (self event radio)
  (declare (ignorable event))
  (trc nil "radio item acts" self (value self) (already-on-do self) .w.)
  (if (value self)
      (ecase (already-on-do self)
        ((nil))
        (:off (setf (value radio) nil)))
    (progn
      (trc nil "here come rb" (associated-value self) radio)
      (setf (value radio)
        (list (associated-value self))))))

(defmodel ct-radio-button (ct-mark-box ct-radio-item) ())
(defmodel ct-text-radio-item ( ct-radio-item ct-text)())

(defmd ct-text-selectable (ct-selectable ct-text))

(defmd ct-radio (control ix-inline)
  on-change
  :value (c-in nil))

(defobserver .value ((self ct-radio)) ;; /// should every control have this?
  (when (^on-change)
    ;(trcx radio-value-observer self new-value old-value old-value-boundp)
    (funcall (^on-change) self new-value old-value old-value-boundp)))

(defmodel ct-radio-row (ct-radio)
  ()
  (:default-initargs
      :orientation :horizontal
      :value (c-in nil)))

(defmodel ct-radio-stack (ct-radio)
  ()
  (:default-initargs
      :value (c-in nil)
    :orientation :vertical))

(defun radio-on-name (radio-values)
  (some (lambda (rb-value)
            (unless (empty$ (cdr rb-value))
              (car rb-value)))
        radio-values))

;--------------- CTCheckBox --------------------------------------------
(export! ct-check-box ct-check-text ct-radio-labeled ct-radio-push-button ct-text-selectable)

(defmodel ct-check-box (ct-mark-box) 
  ()
  (:default-initargs
      :lighting :on
   :value (c-in nil))
  )

(defmd ct-check-text (control ix-row)
  :value (c-in nil)
  :justify :center
  :spacing (u96ths 8)
  :outset (u96ths 2)
  :kids (c? (the-kids
             (make-kid 'ct-check-box
               :md-name  :check-box
               :fg-color (c? (fg-color .parent))
               :value (c? (value .parent))
               :enabled nil) ;; let parent handle clicks since text is clickable by the rules
             (make-kid 'ix-text
               :md-name  :label
               :text$ (c? (title$ .parent))
               :style-id :button)))

    :ct-action (ct-action-lambda
                (trc nil "checktext bingo" (not (value self)))
                (with-cc :check-text-action
                  (setf (value self) (not (value self))))))

(defmodel ct-radio-labeled (ix-row ct-radio-item)
  ()
  (:default-initargs
    :justify :center
    :spacing (u96ths 8)
    :outset (u96ths 2)
    :kids (c? (the-kids
               (mk-part :rbutton (ct-check-box)
                 :value (c? (value .parent))
                 :enabled nil) ;; let parent handle clicks since text is clickable by the rules
               
               (mk-part :label (ix-text)
                 :lighting :off
                 :text$ (c? (title$ .parent))
                 :style-id :button
                 :text-color (c? (if (enabled .parent)
                                                     +white+ +gray+))
                 :pre-layer (with-layers (:rgba (^text-color))))))))

(defmodel ct-radio-push-button (ct-radio-item ct-button)
  ()
  (:default-initargs
      :inset (mkv2 (upts 4) (upts 4))
    :depressed (c? (or (^hilited)(^value)))
    ))

(defmethod ix-paint ((self ct-radio-push-button))
  (when (eql self (kid1 .parent))
    (trc nil "rendering radio-push" self :raster (ogl-raster-pos-get))
    #+(or) (if (ogl-is-enabled gl_scissor_test)
              (trc "rendering radio-push" self :scissored (ogl-bounds (ogl-scissor-box)))
            (trc "rendering radio-push" :unscissored)))
  (call-next-method))

(defmodel ct-push-toggle (ct-toggle ct-button)
  ()
  (:default-initargs
      :value (c-in nil)))

(export! ct-dot-grid)
(defmd ct-dot-grid (control ix-dot-grid))