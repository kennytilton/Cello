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


;;; $Id: cello.lisp,v 1.16 2008/04/11 09:22:46 ktilton Exp $


;;; ============================================================================
;;; PACKAGE DEFINITION
;;; ============================================================================

(defpackage :cello
  (:nicknames :clo)
  (:use
     #:common-lisp
     #-(or ccl cormanlisp sbcl openmcl) #:clos
     #:utils-kt
     #:cells
     #:gui-geometry
     #:cffi
     #:cffi-extender
     #:celtk
     #:kt-opengl
     #:cl-openal
     #:cl-ftgl
     #:cl-magick)
  
  (:export
   
     #:cello-window
     #:cello-window-event-handler
     #:with-layers
     #:visible
     
     #:ct-button
     #:ct-drag
     #:ct-poly-drag
     #:ct-mark-box
     #:ct-radio-item
     #:ct-radio-button
     #:ct-text-radio-item
     #:ct-radio
     #:ct-radio-row
     #:ct-radio-stack
     #:ct-radio-push-button
     #:ct-push-toggle
     #:ct-selector
     #:ct-selector-inline
     #:ct-selectable
     #:ct-text
     #:ct-toggle
     #:ct-twister
     #:ct-jumper
     
     #:ix-togl))

(in-package :cello)



;;; --- macros -----------------------------------------
(export! .togl .og. .ogc. .retog.)

(define-symbol-macro .togl (nearest self ix-togl))

(define-symbol-macro .og.
    (or (ogl-context self)
      (setf (ogl-context self) (nearest self ctk::togl))))

(define-symbol-macro .ogc. (togl-ptr .og.))
(define-symbol-macro .retog. (if (and .og. .ogc.) (togl-post-redisplay .ogc.)
                               #+shhh(print :not-retogging)))

;;; ============================================================================
;;; MISC
;;; ============================================================================



(defmodel c-button (geometer ctk:button)
  ()
  (:default-initargs
      :ll 0 :lt 0 :lr (u96ths 48)
    :lb (u96ths 24)))
