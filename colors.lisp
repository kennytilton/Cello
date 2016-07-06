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

;;; $Header: /project/cello/cvsroot/cello/colors.lisp,v 1.7 2006/09/19 11:25:51 fgoenninger Exp $

(in-package :cello)

;;; -> ALL COLOR DEFINITIONS AND RELATED FUNCTIONS HAVE BEEN MOVED INTO
;;;    FILE KT-OPENGL/COLORS.LISP

;;; --- Lights ------------

(defparameter *brilliant* (make-ff-array :float 1 1 1 1))
(defparameter *b-red* (make-ff-array :float 1 0 0 1))
(defparameter *bright* (make-ff-array :float 0.5f0 0.5f0 0.5f0 1.0f0))
(defparameter *average* (make-ff-array :float 0.5f0 0.5f0 0.5f0 1.0f0))
(defparameter *dim* (make-ff-array :float 0.25f0 0.25f0 0.25f0 1.0f0))
(defparameter *dusk* (make-ff-array :float 0.10f0 0.10f0 0.10f0 1.0f0))
(defparameter *blackout* (make-ff-array :float 0f0 0f0 0f0 1.0f0))
(defparameter *light-pos-tr* (make-ff-array :float 640 0 (nearer 100) 1))
(defparameter *light-pos-tl* (make-ff-array :float 0 0 (nearer 200) 1))
(defparameter *lightposl* (make-ff-array :float 0 -400 (nearer 50) 1))

(defmodel light ()
  ((id        :cell nil :initarg :id :initform nil :accessor id)
   (enabled   :initarg :enabled :initform nil :accessor enabled)
   (pos       :initarg :pos :initform nil :accessor pos)
   (ambient   :initarg :ambient :initform nil :accessor ambient)
   (diffuse   :initarg :diffuse :initform nil :accessor diffuse)
   (specular  :initarg :specular :initform nil :accessor specular)
   (cutoff    :initarg :cutoff :initform 180 :accessor cutoff)
   (spot-dir  :initarg :spot-dir :initform (cons 0 0) :accessor spot-dir)
   (spot-exp  :initarg :spot-exp :initform 0 :accessor spot-exp)
   ))

(export! light)