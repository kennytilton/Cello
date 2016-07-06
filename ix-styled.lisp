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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-styles text-color ^text-color)))

;;; (defclass Helper ()
;;;   ((aa-host :initform nil :initarg :host :accessor host)))

(defclass gui-style ()
  ((id :initarg :id :accessor id :initform nil)
   (face :initarg :face :accessor face :initform nil)
   (text-color :initarg :text-color :accessor text-color)
   )
  (:default-initargs
  :text-color +black+))

(defclass gui-style-sizable ()
  ((size :initarg :size :accessor size :initform nil)
   (size-relative :initarg :size-relative :accessor size-relative :initform 0)
   (sizes :initarg :sizes :accessor sizes :initform nil)))

(defun min-max (min max value)
  (max min (min max value)))

(defun gui-style-size (style)
  (or (size style)
    (setf (size style)
      (elt (sizes style)
        (min-max 0 (1- (length (sizes style)))
          (+ (ceiling (length (sizes style)) 2)
            (size-relative style)))))))

(defparameter *styles* nil)

(defmacro with-styles ((&rest custom-styles) &body body)
  `(call-with-styles (list ,@custom-styles) (lambda () ,@body)))

(defun call-with-styles (styles styled-fn)
  (setf *styles* styles) ;; need when showing off from repl
  (let ((*styles* styles))
    (funcall styled-fn)))

(defun styles-default () *styles*)

(defun gui-style (self style)
  (when style
    ;;(print `(gui-style ,style ,(styles-default)))
    (or (ix-find-style self style)
      (find style (styles-default) :key 'id)
      (find :default (styles-default) :key 'id)
      (break "gui-style cannot find requested style ~a midst known styles ~a"
        style (MAPCAR 'id (styles-default))))))

(defmodel ix-styled ()
  ((style-id :initarg :style-id
     :initform nil
     :reader style-id)
   
   (style :initform (c? (gui-style self (^style-id)))
     :initarg :style
     :reader style)
   
   (text-font :reader text-font :initarg :text-font
             :initform (c? (bwhen (style (^style))
                             (make-style-font style))))

   (text-color :reader text-color :initarg :text-color
     :initform (c? (bif (style (^style))
                     (text-color style)
                     +black+))))
  (:default-initargs
      :pre-layer (c? (if (^bkg-color)
                         (with-layers
                           (:rgba (^bkg-color))
                           :fill
                           (:rgba (^text-color)))
                       (with-layers
                           (:rgba (^text-color)))))))

(defmethod ix-find-style ((self ix-view) style-id)
  (or (find style-id (^gui-styles) :key 'id)
      (ix-find-style .parent style-id)))

(defmethod ix-find-style (self style-id)
  (declare (ignore self style-id)))


(defmethod ogl-dsp-list-prep progn ((self ix-styled) &aux (font (text-font self)))
  (assert (not *ogl-listing-p*))
  (trc nil "ogl-dsp-list-prep sub-prepping font" font)
  (typecase font
    (ftgl-extruded
     (unless (ftgl::ftgl-disp-ready-p font)
       (setf (ftgl::ftgl-disp-ready-p font) t)
       (ftgl::fgc-set-face-size (ftgl::ftgl-get-metrics-font font) 
         (round (ftgl::ftgl-size font)) (ftgl::ftgl-target-res font))))
    (otherwise (ftgl::ftgl-get-display-font font)))
  ;; until 2008-03-30 this next was only done for extruded case above
  (ix-string-width self (display-text$ self))) ;; ugh. make better. subclass must have display-text$

(export! ix-string-width)

(defun ix-string-width (self string)
  (c-assert (s-canvas) () "~a not contained by any canvas" self)
  (font-string-width (target-res (s-canvas)) (text-font self) string))

#|

;--------------------------------------------------------

(defun styles-or (styles superStyles)
  (cond
   ((null styles) superstyles)
   ((null superstyles) styles)
   (t
    (let (effectives)
      (dolist (style styles)
        (c-assert style)
        (push (bIf (superStyle (find (styleName style) superStyles :key #'styleName))
                 (make-style-customizing superstyle
                                         :textface (textface style)
                                         :textStyle (textStyle style)
                                         :textSize (textSize style)
                                         :textSizes (textSizes style)
                                         :fgColor (fgColor style)
                                         :bkColor (bkColor style))
                 
                 style)
              effectives))
      
      (dolist (superStyle superStyles)
        (pushnew superStyle effectives :key #'styleName))
      
      effectives))))

(defun make-style-customizing (superstyle &key textFace textStyle textsizes textsize fgcolor bkcolor)
  (c-assert superstyle)
  (make-Style (stylename superstyle)
              :textface (or textface (textface superStyle))
              :textStyle (or textStyle (textStyle superStyle))
              :textSize (or textSize (textSize superStyle))
              :textSizes (or textSizes (textSizes superStyle))
              :fgColor (or fgColor (fgColor superStyle))
              :bkColor (or bkColor (bkColor superStyle))))




|#
