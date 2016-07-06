;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;(declaim (optimize (debug 2) (speed 1) (safety 1) (compilation-speed 1)))
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))


(in-package :asdf)

(defsystem cellodemo
  :name "cellodemo"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "1.0.0"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT"
  :description "Cello Demos"
  :long-description "Cello Demonstrations"
  :components (#+notyet (:module :cloucell
                 :components ((:file "inspection")
                              (:file "ct-item-stack" :depends-on ("inspection"))
                              (:file "ct-slot-stack" :depends-on ("ct-item-stack"))
                              (:file "inspector-toolbar" :depends-on ("ct-slot-stack"))
                              (:file "inspector-window" :depends-on ("inspector-toolbar"))
                              (:file "outline" :depends-on ("inspector-window"))
                              (:file "slot-inspector" :depends-on ("outline"))
                              (:file "structure-view" :depends-on ("slot-inspector"))))
                
                (:file "cellodemo")
                (:file "demo-window")
                (:file "tutor-geometry")                
                (:file "light-panel")
                (:file "hedron-render")
                (:file "hedron-decoration")))
