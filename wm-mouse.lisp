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

(defstruct (os-event 
           (:conc-name nil))
  modifiers
  where
  realtime
  c-event)

(defun mk-os-event (modifiers where)
  (make-os-event :modifiers modifiers
                :where where
                :realtime (now)))

(defun evt-when (os-event)
  (realtime os-event))

(defun evt-buttons (os-event)
  (modifiers os-event))

(defun evt-shift-key-down (os-event)
  (shift-key-down (evt-buttons os-event)))

(defun evt-control-key-down (os-event)
  (control-key-down (evt-buttons os-event)))

(defun evt-where (os-event)
  (where os-event))

(export! evt-c-event evt-shift-key-down evt-control-key-down)
(defun evt-c-event (os-event)
  (c-event os-event))

(defun evt-wherex (os-event)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; (logand (the fixnum (evtLParam os-event)) (1- 65536))
  (v2-h (evt-where os-event)))

(defun evt-where-y (os-event)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (v2-v (evt-where os-event)))




