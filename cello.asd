;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;(declaim (optimize (debug 2) (speed 1) (safety 1) (compilation-speed 1)))
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

#+(or allegro lispworks cmu mcl cormanlisp sbcl scl)

(defsystem cello
  :name "cello"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "1.0.0"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT"
  :description "A Portable Common Lisp GUI"
  :long-description "The final pieces of a portable Common Lisp GUI"
  
  :depends-on (:cells :gui-geometry :kt-opengl :cffi-extender :cl-magick
               :celtk :cl-openal)
  :serial t
  :components
  ((:file "cello")
   (:file "window-macros")
   (:file "clipping")
   (:file "colors")
   (:file "ix-layer-expand")
   (:file "frame")
   (:file "application")
   (:file "image")
   (:file "ix-opengl")
   (:file "ix-canvas")
   (:file "font")
   (:file "ix-grid")
   (:file "mouse-click")
   (:file "control") 
   (:file "focus")
   (:file "focus-navigation")
   (:file "focus-utilities")
   (:file "ix-styled")
   (:file "ix-text")
   (:file "ix-togl")
   (:file "lighting")
   (:file "ctl-toggle")
   (:file "ctl-markbox")
   (:file "ctl-drag")
   (:file "ctl-selectable")
   (:file "slider")
   (:file "cello-window")
   (:file "window-utilities")
   (:file "wm-mouse")
   (:file "pick")
   (:file "ix-paint")
   (:file "ix-polygon")
   (:file "cello-ftgl")
   (:file "cello-magick")
   (:file "cello-openal")
   (:file "nehe-06")))




