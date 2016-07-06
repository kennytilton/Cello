;; -*- lisp-version: "9.0 [64-bit Windows *SMP*] (Dec 5, 2014 17:12)"; cg: "9.0"; -*-

(in-package :cg-user)

(defpackage :CELLO)

(define-project :name :cello-demo
  :modules (list (make-instance 'module :name "cellodemo.lisp")
                 (make-instance 'module :name "demo-window.lisp")
                 (make-instance 'module :name "tutor-geometry.lisp")
                 (make-instance 'module :name "light-panel.lisp")
                 (make-instance 'module :name "hedron-render.lisp")
                 (make-instance 'module :name "hedron-decoration.lisp"))
  :projects (list (make-instance 'project-module :name "../cello" :show-modules t))
  :libraries nil
  :editable-files nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cello
  :main-form nil
  :compilation-unit t
  :concatenate-project-fasls nil
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :local-name-info)
  :build-flags (list :allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 0
  :run-with-console nil
  :project-file-version-info nil
  :on-initialization 'cello::cello-test
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
