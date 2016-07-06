;; -*- lisp-version: "8.0 [Windows] (Sep 14, 2007 21:56)"; cg: "1.81"; -*-

(in-package :cg-user)

(defpackage :cffi-extender)

(define-project :name :cffi-extender
  :modules (list (make-instance 'module :name "cffi-extender.lisp")
                 (make-instance 'module :name "my-uffi-compat.lisp")
                 (make-instance 'module :name "definers.lisp")
                 (make-instance 'module :name "arrays.lisp")
                 (make-instance 'module :name "callbacks.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "..\\..\\1-devtools\\cffi\\cffi"))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cffi-extender
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
  :on-initialization 'default-init-function
  :on-restart 'do-default-restart)

;; End of Project Definition
