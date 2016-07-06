;; -*- lisp-version: "8.0 [Windows] (Mar 7, 2007 14:53)"; cg: "1.81"; -*-

(in-package :cg-user)

(defpackage :cl-magick)

(define-project :name :cl-magick
  :modules (list (make-instance 'module :name "cl-magick.lisp")
                 (make-instance 'module :name "magick-wand.lisp")
                 (make-instance 'module :name "drawing-wand.lisp")
                 (make-instance 'module :name "pixel-wand.lisp")
                 (make-instance 'module :name "mgk-utils.lisp")
                 (make-instance 'module :name "wand-image.lisp")
                 (make-instance 'module :name "wand-texture.lisp")
                 (make-instance 'module :name "wand-pixels.lisp"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cl-magick
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules nil
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:local-name-info)
  :build-flags '(:allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'cl-magick::cl-magick-test
  :on-restart 'do-default-restart)

;; End of Project Definition
