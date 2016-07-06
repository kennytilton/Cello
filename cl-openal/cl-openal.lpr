;; -*- lisp-version: "8.0 [Windows] (Jan 22, 2007 8:01)"; cg: "1.81"; -*-

(in-package :cg-user)

(defpackage :cl-openal)

(define-project :name :cl-openal
  :modules (list (make-instance 'module :name "cl-openal.lisp")
                 (make-instance 'module :name "altypes.lisp")
                 (make-instance 'module :name "al.lisp")
                 (make-instance 'module :name "alctypes.lisp")
                 (make-instance 'module :name "alc.lisp")
                 (make-instance 'module :name "alu.lisp")
                 (make-instance 'module :name "alut.lisp")
                 (make-instance 'module :name "cl-openal-init.lisp")
                 (make-instance 'module :name "wav-handling.lisp")
                 (make-instance 'module :name "cl-openal-demo.lisp"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cl-openal
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
  :on-initialization 'cl-openal::cl-openal-test
  :on-restart 'do-default-restart)

;; End of Project Definition
