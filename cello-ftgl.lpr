;; -*- lisp-version: "6.2 [Windows] (Jun 26, 2002 11:39)"; common-graphics: "1.389.2.105.2.14"; -*-

(in-package :common-graphics-user)

(defpackage :cello (:export))

(define-project :name :cello-ftgl
  :application-type (intern "Standard EXE" (find-package :keyword))
  :modules (list (make-instance 'module :name "cello-ftgl.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "..\\cl-ftgl\\cl-ftgl")
                  (make-instance 'project-module :name
                                 "..\\cellocore\\cellocore"))
  :libraries nil
  :distributed-files nil
  :project-package-name :cello
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg :drag-and-drop :lisp-widget
                     :multi-picture-button :common-control
                     :edit-in-place :outline :grid :group-box
                     :header-control :progress-indicator-control
                     :common-status-bar :tab-control :trackbar-control
                     :up-down-control :dde :mci :carets :hotspots
                     :menu-selection :choose-list :directory-list
                     :color-dialog :find-dialog :font-dialog
                     :string-dialog :yes-no-list-dialog
                     :list-view-control :rich-edit :drawable :ole :www
                     :aclwin302)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:compiler :top-level :local-name-info)
  :build-flags '(:allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'cello::cello-ftgl-test
  :on-restart 'do-default-restart)

;; End of Project Definition
