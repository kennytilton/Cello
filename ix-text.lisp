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

;===========================================================

(eval-when (compile load eval)
  (export '(ix-paint inset ix-text ix-styled ix-view)))

(defmodel ix-text (ix-styled ix-view)
  (
   (text$ :initform nil :initarg :text$ :accessor text$)

   (display-text$ :initform (c? (remove-if-not #'graphic-char-p (^text$)))
     :initarg :display-text$ :accessor display-text$)
   
   (text-width :initarg :text-width :accessor text-width
     :initform (c? (bwhen (t$ (^display-text$))
                     (ix-string-width self t$))))

   (char-mask :cell nil :initarg :char-mask
     :initform nil
     :reader char-mask)
   
   (maxcharwidth :initarg :maxcharwidth
     :initform (c? (ix-string-width self "M"))
     :accessor maxcharwidth)
   
   (wrapp :initform nil :accessor wrapp :cell nil :initarg :wrapp)
   
   (justify-hz :initform :left
     :accessor justify-hz
     :initarg :justify-hz)
   
   (justify-vt :initform :top
     :accessor justify-vt :initarg :justify-vt)
   
   (inset :cell nil :initarg :inset
     :unchanged-if 'v2=
     :initform (mkv2 0 0)
     :accessor inset)
   (ll :initform (c? (- (inset-h self))))
   (lt :initform (c? (eko (nil "ixtext lt")
                       (ups 0 (font-ascent (text-font self)) (inset-v self)))))
   (lr :initform (c? (eko (nil "ix-text lr")
                       (^lr-width (+ (cond
                                      ((char-mask self) (ix-string-width self (char-mask self)))
                                      ((^text-width))
                                      ((^maxcharwidth))
                                      (t (error "Please specify a font or :lr <n>.")))
                                    (* 2 (inset-h self)))))))
   (lb :initform (c? (eko (nil "ixtext LB")
                       (downs (font-descent (text-font self)) (inset-v self)))))
   )
  (:default-initargs
      :lighting :off))

(export! a-label text$ ^text$ a-sub-label)

(defmacro a-label (text$ &rest key-arg-pairs)
  `(make-kid 'ix-text
     ,@key-arg-pairs
     :style-id :label
     :text$ ,text$))

(defmacro a-sub-label (text$ &rest key-arg-pairs)
  `(make-kid 'ix-text
     ,@key-arg-pairs
     :style-id :sub-label
     :text$ ,text$))

(defmethod display-text$ :around ((self ix-text))
  (or (call-next-method)
    (text$ self)))

(defmethod ix-paint ((self ix-text))
  (when (display-text$ self)
    (ix-render-in-font (text-font self) self)))

(defmethod ix-align-text (self font)
  (declare (ignorable font))
  (flet ((hxs ()
           (- (l-width self) (* 2 (v2-h (inset self))) (^text-width)))
         (vxs ()
           (- (l-height self) (* 2 (v2-h (inset self))) (font-height font))))
    (gl-translatef
     (ecase (justify-hz self)
       (:left 0)
       (:center (/ (hxs) 2.0))
       (:right (hxs)))
     (ecase (justify-vt self)
       (:top 0)
       (:center (downs (/ (vxs) 2.0)))
       (:bottom (downs (vxs))))
     0)))



#+(or)
(format nil "~3,1f" pi)

(defmodel ix-text-tall (ix-text)
  ((text-height :reader text-height
     :initarg :text-height)
   (formatted$ :reader formatted$
     :initarg :formatted$)
   (wrapp :initform t))
  (:default-initargs
      :lb (c? (downs (text-height self)
                (inset-v (inset self))))
    :formatted$ (c? (bif (text$ (^text$))
                      (wrap$ text$
                        (- (^lr) (inset-h (inset self)))
                        (target-res (s-canvas)) (^text-font))
                      ""))
    :text-height (c? (bif (text$ (^formatted$))
                       (* (font-height (text-font self))
                         (1+ (count #\newline text$)))
                       0))))

(defun wrap$ (s w res font)
  (declare (ignorable w res font))
  s)

(export! *menus* find-menu tool-tip-mixin)

(defparameter *menus* nil) ;; set this at make-instance time in the kids rule of the window

(defun find-menu (id)
  (fm-find-one *menus* id :must-find t :skip-tree nil :global-search nil :test #'cells::true-that))

(defun make-string-tool-tip (self s)
  (make-kid 'tool-tip$
    :text$ s))

(defclass tool-tip-mixin ()())

(defmd tool-tip$ (tool-tip-mixin ix-text)
  :inset 3
  :style-id :label
  :pre-layer (with-layers
                 +yellow+
               :fill
               (:frame-3d :edge-raised
                 :thickness 2)
               +black+)
  :text$ "")

(export! tool-tip ^tool-tip)

(defmd tool-tip (tool-tip-mixin ix-stack)
  :visible (c? (^kids))
  :kids (c? (the-kids
             (bwhen (mv (mouse-view .og.))
               (bwhen (v (fm-ascendant-if mv 'tool-tip))
                 (when (and (fully-visible v) ;; gotta lose tooltip of button clicked that takes us off screen
                         (tool-tip-show? v)) ; quick fix
                   (typecase (tool-tip v)
                     (null)
                     (string
                      (make-string-tool-tip self (tool-tip v)))
                     (t (funcall (tool-tip v) self v))))))))
  
  ;
  ; tedious geometry stuff to keep tool tip
  ; visible yet not eclipsed by mouse pointer
  ;
  :px (let (fixed)
        (c? (bwhen (mp (mouse-pos .og.))
              (cond
               ((^visible)
                .retog.
                (or fixed (setf fixed
                            (let ((pref (+ 6 (v2-h mp))))
                              (if (> (+ pref (l-width self)) (lr .og.)) ;; don't sail off to right of togl
                                  (px-maintain-pr (lr .og.) #+hunh? (- (v2-h mp) 16))
                                pref)))))
               (t (setf fixed nil))))))
  :py (let (fixed)
        (c? (bwhen (mp (mouse-pos .og.))
              (cond
               ((^visible)
                .retog.
                (or fixed (setf fixed
                            (min (- (lt .og.)(l-height self))
                              (+ 6 (py-maintain-pb (v2-v mp)))))))
               (t (setf fixed nil)))))))
