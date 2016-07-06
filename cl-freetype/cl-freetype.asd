;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; cl-freetype.asd

(in-package :asdf)

(defsystem cl-freetype
    :name "cl-freetype"
  :author "Yusuke Shinyama <yusuke at cs dot nyu dot edu>"
  :version "0.1"
  :depends-on (:cffi-extender :cl-rsrc)
  :serial t
  :components ((:file "cl-freetype")
               (:file "ft-defs")
               (:file "ft-functions")))
    
