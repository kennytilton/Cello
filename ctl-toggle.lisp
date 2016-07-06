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

(export! ct-text ct-button ct-button-ex ct-selectable-button mk-twisted mk-twisted-part)

(defmodel ct-text (control ix-text)
  ()
  (:default-initargs
      :style-id :label
    
    :text$ (c? (string (^value)))
    :inset (mkv2 (u96ths 2)(u96ths 2))
    :lighting :off
    :text-color (c? (if (^enabled) 
                        (if (^mouse-over?)
                            +green+ +black+)
                      +light-gray+))
    :pre-layer (with-layers :off (:rgba (^text-color)))))

(export! ix-control ct-action kb-selector)
(defmd ix-control (ix-view control))

(defmd ct-button (control ix-text) ;; same inheritance, but otherwise unrelated to CTText
  (value (c-in nil) :cell :ephemeral)
  (inset (mkv2 (upts 4) (upts 12)) :unchanged-if 'v2=)
  (depressed (c? (^hilited)))
  :ct-action (ct-action-lambda
               (with-cc :button-press
                 .retog.
                 (setf (^value) t)))
  :title$ (c? (string-capitalize (md-name self)))
  :text$ (c? (^title$))
  :clipped nil #+not t
  :justify-hz :center
  :justify-vt :center
  :style-id :button
  :skin (c? (skin .w.))
  :text-color (c? (cond
                   ((not (^enabled)) +dark-gray+)
                   ((^depressed) +dark-gray+)
                   (t +black+)))
  :pre-layer  (c? (let* ((thick (min (u96ths 4) (* 0.08 (l-width self))))
                         (defl (if (clo::^depressed) (downs (/ thick 3)) 0))
                         (push-in (if (clo::^depressed) (xlout (* .5 thick)) 0)))
                    (declare (ignorable thick defl))
                    (trc nil "ctbutton" thick defl)
                    
                    (with-layers
                        (:v3f (/ defl 2) defl push-in)
                      
                      +white+
                      :on
                      (:frame-3d :edge-raised
                        :thickness thick
                        :texturing (list (clo::^skin)))
                      (:rgba (^text-color))
                      :off
                      ))))

(defmacro ct-button-ex ((text command) &rest initargs)
  `(make-instance 'ct-button
     :fm-parent *parent*
     :title$ ,text
     :ct-action (ct-action-lambda
                 (with-cc :ct-button-ex-ct-action
                   ,command))
     ,@initargs))

(defmodel ct-selectable-button (ct-selectable ct-button)())

; ---------------- CT FSM ---------------------
(defmodel ctfsm (control)
  (
   (transition-fn :cell nil :initarg :transition-fn :reader transition-fn)
   (states :cell nil :initarg :states :reader states)
   )
  (:default-initargs
      :value (c-in nil)
    :transition-fn 'ctfsm-transition-fn

   :ct-action (ct-action-lambda
               ;(trc "twister ct-action" self event)
               (with-integrity (:change :ctfsm-action)
                 (let ((newv (funcall (transition-fn self) self (value self) (states self))))
                   (ct-fsm-assume-value self newv))))))

(export! ctfsm-transition-fn)

(defun ctfsm-transition-fn (self current-state state-table)
  (declare (ignorable self))
  #+xxx (trc "CTFSM :transitionFN  curr,table" current-state state-table)
  (or (cadr (member current-state state-table :test (if (stringp current-state)
                                                        #'string-equal
                                                      #'eql)))
    (car state-table)))

(defmethod ct-fsm-assume-value (self new-value)
  (setf (value self) new-value))

; --------------- CT Toggle -----------------------

(defmodel ct-toggle (ctfsm)
  ((associated-value :initform t :cell nil :initarg :associated-value :reader associated-value)
   (associated-shortc :initform t :cell nil :initarg :associated-shortc :reader associated-shortc))
  (:default-initargs
   :states '(nil t)))

;------------------------------------------------------

(defmodel ct-twister (ct-toggle ix-polygon) ;; convert to IMBitmapMulti??
  ;
  ; For twist-down control to open/close details
  ;
  ()
  (:default-initargs
   :value (c-in nil) ;;; closed by default
   :poly-style :fill
   :pre-layer (c? (with-layers 
                      (:poly-mode gl_front_and_back gl_fill)
                    (:rgba (if (^hilited)
                               +green+ +black+))))
   :vertices (c? (if (value self)
                     '((2 . -4) (7 . -9) (12 . -4))
                     '((4 . -2) (9 . -7) (4 . -12))))
   :ll 0 :lt 0 :lr (u96ths 15) :lb (downs (u96ths 15))))

(export! a-twister a-twister-lb30 ix-twister ct-radio-tree expanded ^initial-open initial-open ^selectedp selectedp)

(defmacro a-twister ((label component-args initial-open &rest twister-args) twisted-widget)
  `(a-stack (,@component-args)
        (a-row ()
          (or (car .cache)
            (make-kid 'ct-twister
              :md-name :show-contents
              :value (c-in ,initial-open)
              :visible (c? (^enabled))
              ,@twister-args))
          ,(if (stringp label)
               `(make-kid 'ix-text
                 :text$ ,label
                  :text-color +black+
                 :style-id :button)
             label)) ;; actually should be a form to build a widget
        (a-stack (:collapsed (c? (eko (nil "collapsed!!!!!!!!!!!!" .cause)
                                   (let ((tw (fm^ :show-contents)))
                                     (assert (eq .parent (fm-parent (fm-parent tw))))
                                     (not (value tw))))))
          ,twisted-widget)))

(defmacro a-twister2 ((label component-args initial-open &rest twister-args) twisted-widget)
  `(a-stack (,@component-args)
     (a-row ()
       (or (car .cache)
         (make-kid 'ct-twister
           :md-name :show-contents
           :value (c-in ,initial-open)
           :visible (c? (^enabled))
           ,@twister-args))
       ,(if (stringp label)
            `(make-kid 'ix-text
               :text$ ,label
               :text-color +black+
               :style-id :button)
          label)) ;; actually should be a form to build a widget
     (a-stack (:collapsed (c? (eko (nil "collapsed!!!!!!!!!!!!" .cause)
                                (let ((tw (fm^ :show-contents)))
                                  (assert (eq .parent (fm-parent (fm-parent tw))))
                                  (not (value tw))))))
       (unless (^collapsed) ,twisted-widget))))

(defmacro a-twister-lb30 ((label component-args initial-open &rest twister-args) twisted-widget)
  `(a-stack (,@component-args)
     (a-row (:lb (downs 24) :justify :center)
       (or (car .cache)
         (make-kid 'ct-twister
           :md-name :show-contents
           :value (c-in ,initial-open)
           :visible (c? (^enabled))
           ,@twister-args))
       ,(if (stringp label)
            `(make-kid 'ix-text
               :text$ ,label
               :style-id :button)
          label)) ;; actually should be a form to build a widget
     (a-stack (:collapsed (c? (eko (nil "collapsed!!!!!!!!!!!!" .cause)
                                (let ((tw (fm^ :show-contents)))
                                  (assert (eq .parent (fm-parent (fm-parent tw))))
                                  (not (value tw))))))
       ,twisted-widget)))

(defmd ix-twister (ix-stack)
  label
  initial-open
  twisted-widget
  :kids (c? (let ((label (^label)))
              (the-kids
               (a-stack ()
                 (a-row ()
                   (or (car .cache)
                     (make-kid 'ct-twister
                       :md-name :show-contents
                       :value (c?n (initial-open (u^ ix-twister)))
                       :visible (c? (^enabled))))
                   (if (stringp label)
                       (make-kid 'ix-text
                         :text$ label
                         :style-id :button)
                     label))
                 (a-stack (:px 8 :collapsed (c? (let ((tw (fm^ :show-contents)))
                                            (not (value tw)))))
                   (let ((spec (twisted-widget (u^ ix-twister))))
                     (apply 'make-instance (car spec)
                       :fm-parent self (cdr spec)))))))))

(export! selectorp selection label ^selectorp ^selection ^label tree-label ^tree-label
  ^kids-factory kids-factory ct-stack)

(defmd ct-stack (ix-stack control)
  :tool-tip-show? (c? (eko (nil "show?" (^mouse-over?))
                        (and 
                         (show-tool-tips? .tkw)
                         (or (^mouse-over?)
                           (fm-descendant-if self 'mouse-over?))
                         (and (mouse-still .og.)
                           (> 5 (mouse-still .og.) 1))))))

(defmd ct-radio-tree (ct-stack)
  (tree-label (c? (princ (^value))))
  (selectedp (c? (eko (nil "selectedp")
                   (eq self (selection (selector self))))))
  label
  initial-open
  (expanded (c? (^selectedp)))
  kids-factory
  (indent 12)
  :kids (c? (let ((label (^tree-label))
                  (tree self))
              (the-kids
               (if (stringp label)
                   (make-kid 'ct-text
                     :text$ label
                     :style-id :button
                     :text-color (c? (if (selectedp .parent) +blue+ +black+))
                     :ct-action (ct-action-lambda
                                 ;;(trc "tree triggered" (selector self) tree)
                                 (with-cc :ct-radio-item
                                   (trc "making selection" (selector self) tree)
                                   (setf (selection (selector self)) tree)
                                   #+not
                                   (if (bwhen (n (nsib)) (kids n))
                                       (with-cc :button-press
                                         .retog.
                                         ;;(trc "expand chg" (nsib) (type-of (nsib)))
                                         (setf (expanded .parent) (not (expanded .parent))))
                                     (progn
                                       ;;(trc "setting selector!!!!!" (selector self) tree)
                                       (setf (selection (selector self)) tree))))))
                 label)
               (bwhen (f (^kids-factory))
                 (a-stack (:px (^indent)
                            :collapsed (c? (not (expanded tree))))
                   (funcall f self)))))))

(defgeneric selectedp (self)
  (:method (self) (declare (ignore self)) nil))

(defgeneric selectorp (self)
  (:method (self) (declare (ignore self)) nil))

(defmethod selector (self)
  (u^ ct-selector))

#| vestigial?

(defmacro mk-twisted (twisted-name (label-class &rest label-args)
                                 (twisted-class &rest twisted-args))
  `(make-kid :twisted-group (ix-zero-tl)
      :showkids (c-in nil)
      :ll (c? (geo-kid-wrap self 'pl))
      :lr (c? (geo-kid-wrap self 'pr))
      :kids (c? (let ((thetree self))
                   ;; (trc "making all parts of tree for" (value self) rethinker)
                   (the-kids
                    (mk-part 'ix-kid-sized
                       :ll (u96ths -20) :px 0
                       :kids (c? (packed-flat!
                                   (mk-part :opener (ct-twister)
                                      :py (u96ths 2)
                                      :px (c? (px-maintain-pr (u96ths -3)))
                                      :ct-action (lambda (self event &aux (twist-group (fm-parent .parent)))
                                                       (declare (ignorable self event))
                                                       (with-transaction ()
                                                         (setf (showkids twist-group) (not (showkids twist-group))))))
                                   (mk-part ,twisted-name (,label-class)
                                      ,@label-args))))
                    (mk-part ,twisted-name (,twisted-class)
                       ,@twisted-args
                       :px 0
                       :py (c? (^prior-sib-pb self)))
                    )))))

(defmacro mk-twisted-part (twisted-name (label$ &rest label-args)
                            twisted-part)
  `(make-kid 'ix-zero-tl
     :showkids (c-in nil) ;; /// parameterize
     :ll (c? (geo-kid-wrap self 'pl))
     :lr (c? (geo-kid-wrap self 'pr))
     :kids (c? (the-kids
                (make-kid 'ix-kid-sized
                  :ll (u96ths -20) :px 0
                  :kids (c? (packed-flat!
                             (make-kid 'ct-twister
                               :py (u96ths 2)
                               :px (c? (px-maintain-pr (u96ths -3))))
                             (make-kid 'ix-text
                               :md-name ',twisted-name
                               ,@label-args
                               :text$ ,label$))))
                ,twisted-part
                ))))

|#