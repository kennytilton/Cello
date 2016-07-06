;; -*- lisp-version: "8.0 [Windows] (Jan 29, 2007 18:02)"; cg: "1.81"; -*-

(in-package :cg-user)

(defpackage :kt-opengl)

(define-project :name :kt-opengl
  :modules (list (make-instance 'module :name "defpackage.lisp")
                 (make-instance 'module :name "kt-opengl-config.lisp")
                 (make-instance 'module :name "kt-opengl.lisp")
                 (make-instance 'module :name "gl-def.lisp")
                 (make-instance 'module :name "gl-constants.lisp")
                 (make-instance 'module :name "gl-functions.lisp")
                 (make-instance 'module :name "glu-functions.lisp")
                 (make-instance 'module :name "ogl-macros.lisp")
                 (make-instance 'module :name "ogl-utils.lisp")
                 (make-instance 'module :name "move-to-gl.lisp")
                 (make-instance 'module :name "colors.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "..\\cffi-extender\\cffi-extender"))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :kt-opengl
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules nil
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:compiler :top-level :local-name-info)
  :build-flags '(:allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'kt-opengl::lesson-14
  :on-restart 'do-default-restart)

;; End of Project Definition
