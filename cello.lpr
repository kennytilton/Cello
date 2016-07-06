;; -*- lisp-version: "9.0 [64-bit Windows *SMP*] (Dec 5, 2014 17:12)"; cg: "9.0"; -*-

(in-package :cg-user)

(defpackage :CELLO)

(define-project :name :cello
  :modules (list (make-instance 'module :name "cello.lisp")
                 (make-instance 'module :name "window-macros.lisp")
                 (make-instance 'module :name "clipping.lisp")
                 (make-instance 'module :name "colors.lisp")
                 (make-instance 'module :name "ix-layer-expand.lisp")
                 (make-instance 'module :name "frame.lisp")
                 (make-instance 'module :name "application.lisp")
                 (make-instance 'module :name "image.lisp")
                 (make-instance 'module :name "ix-togl.lisp")
                 (make-instance 'module :name "ix-opengl.lisp")
                 (make-instance 'module :name "ix-canvas.lisp")
                 (make-instance 'module :name "font.lisp")
                 (make-instance 'module :name "ix-grid.lisp")
                 (make-instance 'module :name "mouse-click.lisp")
                 (make-instance 'module :name "control.lisp")
                 (make-instance 'module :name "focus.lisp")
                 (make-instance 'module :name "focus-navigation.lisp")
                 (make-instance 'module :name "focus-utilities.lisp")
                 (make-instance 'module :name "ix-styled.lisp")
                 (make-instance 'module :name "ix-text.lisp")
                 (make-instance 'module :name "lighting.lisp")
                 (make-instance 'module :name "ctl-toggle.lisp")
                 (make-instance 'module :name "ctl-markbox.lisp")
                 (make-instance 'module :name "ctl-drag.lisp")
                 (make-instance 'module :name "ctl-selectable.lisp")
                 (make-instance 'module :name "slider.lisp")
                 (make-instance 'module :name "cello-window.lisp")
                 (make-instance 'module :name "window-utilities.lisp")
                 (make-instance 'module :name "wm-mouse.lisp")
                 (make-instance 'module :name "pick.lisp")
                 (make-instance 'module :name "ix-paint.lisp")
                 (make-instance 'module :name "ix-polygon.lisp")
                 (make-instance 'module :name "cello-ftgl.lisp")
                 (make-instance 'module :name "cello-magick.lisp")
                 (make-instance 'module :name "cello-openal.lisp")
                 (make-instance 'module :name "ct-scroll-pane.lisp")
                 (make-instance 'module :name "ct-scroll-bar.lisp"))
  :projects (list (make-instance 'project-module :name "../Cells/cells" :show-modules
                                 nil)
                  (make-instance 'project-module :name "kt-opengl/kt-opengl"
                                 :show-modules nil)
                  (make-instance 'project-module :name "cl-freetype/cl-freetype"
                                 :show-modules t)
                  (make-instance 'project-module :name "cl-ftgl/cl-ftgl" :show-modules
                                 nil)
                  (make-instance 'project-module :name "cl-openal/cl-openal"
                                 :show-modules nil)
                  (make-instance 'project-module :name
                                 "../Cells/gui-geometry/gui-geometry" :show-modules nil)
                  (make-instance 'project-module :name "cl-magick/cl-magick"
                                 :show-modules nil)
                  (make-instance 'project-module :name "../Celtk/CELLOTK" :show-modules
                                 nil)
                  (make-instance 'project-module :name "cffi-extender/cffi-extender"
                                 :show-modules nil))
  :libraries nil
  :editable-files nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cello
  :main-form nil
  :compilation-unit t
  :concatenate-project-fasls nil
  :verbose nil
  :runtime-modules nil
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
  :on-initialization 'cello::nehe-06
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
