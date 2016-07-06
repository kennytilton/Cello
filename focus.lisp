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

#| To do

- IXEditSelection needs a home

;;; also got FFComposite rule deciding it was active if any kid was

arrange for Focuser to process clicks and keys first, then mebbe dump into dvk,
bottom up from focus/imageunder

arrange for Controller to process clicks first, then mebbe dump into 
bottom up from focus/imageunder

add finalization for radio button (look at others, see if ICR can ne de-celled

when focused and editactive, push caret onto kids of cttext? or work out how to show
it without it being a kid there

|#


(defmd focuser (ix-canvas)
  (focus (c-in nil))
  textual-focus
  (edit-active (c-in nil))
  (insertion-pt (c-in 0))
  (sel-end (c-in nil))
  (sel-range nil :documentation "selEnd identified during drag operation")
  (undo-data nil :cell nil
    :documentation "Data structure holding undo information")
  :kids (c? (the-kids (^content))))


(export! ^focus focus .focus focus-find-first .focuser)

;;;(defobserver focus ((self focuser))
;;;  (when (and (null new-value) old-value)
;;;            (break "focus nilling"))
;;;  (TRC "focus-observe" self new-value old-value))

(defun focuser (self)
  (swdw))

(define-symbol-macro .focuser (focuser self))
  
(defmethod (setf focus) :around (new-focus self) ;; better be Focuser
  (let ((curr-focus (slot-value self 'focus)))
    (trc nil "setf focus sees new" new-focus :old curr-focus :focuser self)
    (unless (eql new-focus curr-focus)
      (focus-lose curr-focus new-focus)
      (focus-gain new-focus))
    (call-next-method)))

(defun focus-on-dbg (self new-focus dbg)
  (declare (ignorable dbg))
  (trc nil "dbg focus by" dbg :on new-focus :self self)
  (setf .focus new-focus))

(export! focused-on ^focused-on focus-on-dbg)

(defmodel focus ()
  ((focus-thickness :cell nil :initarg :focus-thickness
                   :initform (u96ths 3)
                   :accessor focus-thickness)

   (tab-mode :documentation ":ceiling :stop or nil"
            :cell nil :initarg :tab-mode
            :initform :stop
            :accessor tab-mode)

   (focused-on :initarg :focused-on
               :initform (c-in nil)
               :accessor focused-on)))

(defun tabstopp (self)
  (eql :stop (tab-mode self)))

(defmethod tab-mode (other)
   (declare (ignore other))
   nil)

(defmethod edit-requires-activation (self)
  (declare (ignore self)))

(defmodel focus-minder ()
  ;
  ; an entity which remembers which descendant was focused when the
  ; window focus moves outside the FocusMinder. This 'minded' focus
  ; is restored as the window's focus if the FocusMinder itself
  ; becomes the window's focus (if no minded focus, we focus-first)
  ;
  ((focus-minded :accessor focus-minded :initarg :focus-minded
                :initform (c? (let ((focus (focus .w.)))
                                 (if (fm-includes self focus)
                                     (if (eql self focus)
                                         .cache
                                       focus)
                                   .cache))))))

(export! focus-handle-keysym)

(defgeneric focus-handle-keysym (self keysym)
  (:method :around (self keysym)
    (let ((r (call-next-method)))
      (when (and (not (eq r :focus-handle-keysym-break))
              .parent)
        (focus-handle-keysym .parent keysym))))
  (:method (self keysym)
    (declare (ignore self keysym))
    nil))
