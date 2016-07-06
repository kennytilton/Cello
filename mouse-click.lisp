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

(export! os-event)

(defmodel mouse ()
  ((leftb :initarg :leftb :initform (c-in :up) :accessor leftb)
   (middleb :initarg :middleb :initform (c-in :up) :accessor middleb)
   (rightb :initarg :rightb :initform (c-in :up) :accessor rightb)))

(defmodel mouse-click (model perishable)
  ((click-window :cell nil :initarg :click-window :initform nil :reader click-window)
   (os-event :cell nil :initarg :os-event :reader os-event)
   (clickee :cell nil :initarg :clickee :reader clickee)
   (clickee-pxy :reader clickee-pxy :cell nil :initarg :clickee-pxy)
   (click-age :initform (c? (- .time (evt-when (os-event self))))
             :documentation "Unreliable unless click-repeat-p forcing events")

   (double-clicked :reader double-clicked
     :initform (c? (double-click-evt (click-window self)))) ;; hoping Tk guarantees constraints on position

   (click-completed :reader click-completed
     :initform (c? (or (^double-clicked)
                     (mouse-up-evt (click-window self)))))
   
   (click-over :reader click-over
     :initform (c?  (when (typep (click-window self) 'model)
                      (unless (^click-completed)
                        (when (mouse-over? (clickee self))
                          (mouse-pos (click-window self)))))))
   
   (in-drag :reader in-drag
     :initform (c? (when (typep (click-window self) 'model)
                     (unless (^click-completed)
                       (when (mouse-over? (clickee self))
                         (mouse-pos (click-window self)))))))
   
   (clicked :reader clicked
     :initform (c? ;(trc "clicked?> typeof clickw" (click-window self) (type-of (click-window self)))
                (bwhen (up (^click-completed))
                  (trc nil "clicked?> asking point-in-box"
                    (evt-where up)(clickee self)(without-c-dependency(screen-box (clickee self))))
                  (when (point-in-box (evt-where up) ;; more precise than mPos
                          (without-c-dependency ;; moving GUI elements? chya
                              (screen-box (clickee self))))
                    (cons (clickee self) up)))))
   )
  (:default-initargs
      :expiration (c? (eko (nil "expiry?" (click-window self))
                        (mouse-up-evt (click-window self))))))

(defmethod initialize-instance :after ((self mouse-click) &key)
  (with-integrity (:change :ii-mouseclick)
    (when (typep (clickee self) 'focus)
      (unless (control-key-down (evt-buttons (os-event self))) ;; lame debugging enabler; make better
        (focus-navigate (focus (click-window self)) (clickee self))))

    ;;;20060601 (to-be self) ;; unnecessary? 2301kt just moved this from after next line 
    (trc nil "echo click set self clickee" self (clickee self))

    (when (clickee self) 
      (setf (click-evt (clickee self)) self))))

(defmethod (setf click-evt) :around (new-click self)
  (when (or (null new-click)
          (if (typep self 'window)
              (ctl-notify-mouse-click self self new-click)
            (ctl-notify-mouse-click (fm-parent self) self new-click)))
    (call-next-method)))

(defmethod ctl-notify-mouse-click (self clickee click)
  (when (fm-parent self)
    (ctl-notify-mouse-click (fm-parent self) clickee click)))

; --------------------------------------------------------

(defmethod not-to-be :around ((self mouse-click))
  (when (typep (click-window self) 'model) ;; ACL can do weird things closing a window
    (with-integrity (:change :not-to-be-click)
      (trc nil "echo click clearing self from clickee" (clickee self))
      (setf (click-evt (clickee self)) nil) ;; do this first?
      ;; (trc "echo click not-to-be-ing self from clickee" self)
      (call-next-method))))

(defobserver clicked ()
  (trc nil "echo clicked " self new-value)
  (when (and new-value (click-window self))
    (progn ;; with-metrics (t nil "echo clicked calling control.do.action" self new-value)
      (control-do-action (car new-value) (cdr new-value)))))

(defobserver double-clicked ()
  (when new-value
    (trc nil " double-clicked " self new-value (clickee self))
    (do-double-click (clickee self))))

;----------------------------------------

(defobserver click-over ()
   (ctl-handle-over (clickee self) self new-value))

(defmethod ctl-handle-over (self click-start over-info)
  (declare (ignore self click-start over-info)))

;;; (defmethod ctl-handle-over :before ((self control) clickStart overInfo)
;;;   (declare (ignore clickStart))
;;;   (setf (hilited self) overInfo)) ;; treat as flag: only issue is nil or not

;-----------------------------------------

