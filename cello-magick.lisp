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

(eval-when (compile load eval)
  (defmethod ix-layer-expand ((key (eql :wand)) &rest args)
    `(let ((wand ,(car args)))
       (cells::trc nil "ix-layer-expand draw wand for" self wand)
       (ix-render-wand wand l-box))))

(defobserver recording ()
  (when old-value
    (recording-write old-value)))

(defun ix-snapshot (self)
  (let ((recording (make-recording
                      :wand (magick-wand-template)
                      :pathname (funcall (snapshot-pathnamer self) self))))
    (ix-record-frame self recording)
    (recording-write recording)
    (recording-destroy recording)))

(let (pixels pix-cols pix-rows)
  (defun ix-record-frame (self recording)
    "Only works for window so far"
    (let ((columns (l-width self))
          (rows (l-height self)))
      (when (and pixels
              (not (and (eql pix-cols columns)(eql pix-rows rows))))
        (fgn-free pixels)
        (setf pixels nil))
      (unless pixels
        (setf pixels (fgn-alloc :unsigned-char (* 3 columns rows) :snapshot)
          pix-cols columns pix-rows rows))

      ;;(trc "snapshot read" offset columns rows)
      (gl-read-pixels 0 0 ;; (v2-h offset) (abs (v2-v offset))
        columns rows gl_rgb gl_unsigned_byte pixels)
      (ogl::glec :snapshot)
      (record-frame recording pixels columns rows))))

(export! ix-image-file)

(defmd ix-image-file (ix-view)
  (:documentation "Quick way to drop a view of a binary JPG, PNG, GIF, etc into a Cello window")
  image-path
  (mode :texture :documentation ":texture or :pixel, as in OpenGL")
  tilep
  transparency
  :value (c? (if (^image-path)
                 (let ((wand (wand-ensure-typed
                              (ecase (^mode) (:texture 'wand-texture)(:pixel 'wand-pixel))
                              (^image-path)
                              :tilep (^tilep)
                              :storage (if (^transparency) gl_rgba gl_rgb))))
                   (assert wand () "Unable to load image file ~a" (^value))
                   wand)
               (break "ix-image-file has no path to image file!!!!!" self)))
  :pre-layer (c? (bwhen (w (^value))
                   (with-layers  +white+ (:wand w))))
  :ll 0 :lt 0 :lb (c? (bif (w (^value))
                        (downs (cdr (image-size w)))
                        0))
  :lr (c? (bif (w (^value))
            (car (image-size (^value)))
            0)))

(defparameter *mapping-textures* nil)

(defun ix-render-wand (wand l-box)
  (if wand
    (apply 'wand-render wand (r-bounds l-box))
    (trc nil "ix-render-wand sees no wand" l-box)))




