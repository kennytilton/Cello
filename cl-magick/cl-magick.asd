;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;(declaim (optimize (debug 2) (speed 1) (safety 1) (compilation-speed 1)))
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))


(in-package :asdf)

#+(or allegro lispworks cmu mcl cormanlisp sbcl scl)

(defsystem cl-magick
  :name "cl-magick"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "1.0.0"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT"
  :description "Bindings for ImageMagick"
  :long-description "Poorly implemented bindings to half of ImageMagick"
  :depends-on (:cffi :cffi-extender :utils-kt :gui-geometry :kt-opengl)
  :components ((:file "cl-magick")
               (:file "magick-wand"  :depends-on ("cl-magick"))
               (:file "drawing-wand" :depends-on ("magick-wand"))
               (:file "pixel-wand"   :depends-on ("drawing-wand"))
               (:file "mgk-utils"    :depends-on ("pixel-wand"))
               (:file "wand-image"   :depends-on ("mgk-utils"))
               (:file "wand-texture" :depends-on ("wand-image"))
               (:file "wand-pixels"  :depends-on ("wand-texture"))))
