;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;(declaim (optimize (debug 2) (speed 1) (safety 1) (compilation-speed 1)))
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

(defsystem kt-opengl
  :name "kt-opengl"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "1.0.0"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT"
  :description "Partial OpenGL Bindings"
  :long-description "Poorly implemented bindings to half of OpenGL"
  :depends-on (:cffi-extender :cells)
  :serial t
  :components ((:file "defpackage")
               (:file "kt-opengl-config")
               (:file "kt-opengl")
               (:file "gl-def")
               (:file "gl-constants")
               (:file "gl-functions")
               (:file "glu-functions")
               (:file "ogl-macros")
               (:file "ogl-utils")
               (:file "move-to-gl")
	       (:file "colors")))
