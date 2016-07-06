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

(define-symbol-macro .w.
    (or (.window-cache self)
      (setf (.window-cache self) (upper self window))))

;;;(defmacro wd ()
;;;  `(or (windowCache self)
;;;     (setf (windowCache self) (upper self Window))))

(defmacro swdw () '.w.)

(defmacro with-clipping ((self-form) &body body)
  (let ((fn (gensym)))
    `(let ((,fn (lambda () ,@body)))
       (declare (dynamic-extent ,fn))
       (call-with-clipping ,self-form ,fn))))



(defparameter *time-me* nil)

(defparameter *hlt* nil)
(defparameter *hs* nil)

(defparameter *with-invalidation* nil)
(defparameter *with-one-invalidation* nil)
(defparameter *wm-paint-stack* nil)
(defparameter *wm-paint-reentrant* nil)



