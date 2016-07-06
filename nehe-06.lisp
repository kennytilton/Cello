;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; nehe-06.lisp --- Celtk/Togl version of cl-opengl Lisp version of
;;;                  nehe lesson 06 spinning cube with texture
;;;

(in-package :cello)

(defvar *startx*)
(defvar *starty*)
(defvar *xangle0*)
(defvar *yangle0*)
(defvar *xangle*)
(defvar *yangle*)

(defparameter *vTime* 100)

(defparameter *grace* nil)

(defconstant wcx 640)        ;; Window Width
(defconstant wcy 480)        ;; Window Height
(defparameter xrot 0.0f0)
(defparameter yrot 0.0f0)
(defparameter zrot 0.0f0)
(defparameter *skin6* nil)

(defvar *jmc-font* )

(defun nehe-06 () ;; ACL project manager needs a zero-argument function, in project package
  (cl-magick-reset)
  (cl-ftgl-init)
  (test-window 'nehe-06-demo))

(defmodel nehe-06-demo (window)
  ()
  (:default-initargs
      :title$ "Rotating nehe-06 Widget Test"
    :kids (c? (the-kids
               (mk-stack (:packing (c?pack-self))
                 (make-instance 'nehe06
                   :fm-parent *parent*
                   :width 700 :height 500
                   :timer-interval 2 #+later (c? (let ((n$ (value (fm-other :vtime))))
                                         (format nil "~a" (max 1 (or (parse-integer n$ :junk-allowed t) 0)))))
                   :double 1 ;; "yes"
                   ))))))


(defconstant +pif+ (coerce pi 'single-float))

(defmodel nehe06 (togl)
  ((shoot-me :cell nil :initform nil :accessor shoot-me)
   (frame-count :cell nil :initform 0 :accessor frame-count)
   (t0 :cell nil :initform 0 :accessor t0)
   ;
   (width :initarg :wdith :initform 400 :accessor width)
   (height :initarg :wdith :initform 400 :accessor height))
  (:default-initargs
      :cb-destroy (lambda (self)
                    (bwhen (s (shoot-me self))
                      (trc "stopping source" s)
                      (cl-openal::al-source-stop s)))))

(defmethod togl-timer-using-class ((self nehe06))
  (trc nil "enter nehe-06 timer" self (togl-ptr self) (get-internal-real-time))
  (togl-post-redisplay (togl-ptr self))
  (if (shoot-me self)
      (unless (cl-openal::al-source-playing-p (shoot-me self))
        (cl-openal::al-source-play (shoot-me self)))
    (setf (shoot-me self)
      (cl-openal::wav-play-start "/0dev/cello/user/sounds/spinning.wav"))))

(defmethod togl-reshape-using-class ((self nehe06))
  (let ((width (togl-width (togl-ptr self)))
        (height (togl-height (togl-ptr self))))

    (trc "enter nh6 reshape" self width height)
    (unless (or (zerop width) (zerop height))
      (gl-viewport 0 0 width height)
      (gl-matrix-mode gl_projection)
      (gl-load-identity)
      (glu-perspective 45 (/ width height) 0.1 100)
      (gl-matrix-mode gl_modelview)
      (gl-load-identity))))




(defmethod togl-display-using-class ((self nehe06))
  (gl-load-identity)
  (gl-clear (+ gl_color_buffer_bit gl_depth_buffer_bit))

  (gl-line-width 1)
  (gl-color3f 1f0 1f0 1f0)
  (gl-translatef 0 0 -5)
  (gl-enable gl_texture_2d)

  ;--------------------------------------------
  
  (progn
    ;; (gl-translatef 0 0 -5) 
    
    (let ((f 0.2))
      (gl-rotatef (incf xrot (* f 3)) 1 0 0)
      (gl-rotatef (incf yrot (* f 2)) 0 1 0)
      (gl-rotatef (incf zrot (* f 4)) 0 0 1))
    
    (wand-texture-activate *skin6*)
    
    (flet ((v3f (x y z)
             (let ((scale 1))
               (gl-vertex3f (* scale x)(* scale y)(* scale z)))))
      (with-gl-begun (gl_quads)
        ;; Front Face
        (gl-tex-coord2f 0 1)(v3f  1 -1  1)
        (gl-tex-coord2f 0 0)(v3f  1  1  1)
        (gl-tex-coord2f 1 0)(v3f -1  1  1)
        (gl-tex-coord2f 1 1)(v3f -1 -1  1)
;;;        (gl-tex-coord2f 1 0)(v3f  1 -1  1)
;;;        (gl-tex-coord2f 1 1)(v3f  1  1  1)
;;;        (gl-tex-coord2f 0 1)(v3f -1  1  1)
;;;        (gl-tex-coord2f 0 0)(v3f -1 -1  1)
        
        ;; Back Face
        (gl-tex-coord2f 1 0) (v3f -1 -1 -1)
        (gl-tex-coord2f 1 1) (v3f -1  1 -1)
        (gl-tex-coord2f 0 1) (v3f  1  1 -1)
        (gl-tex-coord2f 0 0) (v3f  1 -1 -1)
        ;;;   Top Face
        (gl-tex-coord2f 0 1) (v3f -1  1 -1)
        (gl-tex-coord2f 0 0) (v3f -1  1  1)
        (gl-tex-coord2f 1 0) (v3f  1  1  1)
        (gl-tex-coord2f 1 1) (v3f  1  1 -1)
        ;;;   Bottom Face
        (gl-tex-coord2f 1 1) (v3f -1 -1 -1)
        (gl-tex-coord2f 0 1) (v3f  1 -1 -1)
        (gl-tex-coord2f 0 0) (v3f  1 -1  1)
        (gl-tex-coord2f 1 0) (v3f -1 -1  1)
        ;;;   Right face
        (gl-tex-coord2f 1 0) (v3f  1 -1 -1)
        (gl-tex-coord2f 1 1) (v3f  1  1 -1)
        (gl-tex-coord2f 0 1) (v3f  1  1  1)
        (gl-tex-coord2f 0 0) (v3f  1 -1  1)
        ;;;   Left Face
        (gl-tex-coord2f 0 0) (v3f -1 -1 -1)
        (gl-tex-coord2f 1 0) (v3f -1 -1  1)
        (gl-tex-coord2f 1 1) (v3f -1  1  1)
        (gl-tex-coord2f 0 1) (v3f -1  1 -1)
        ))
    ;;#+ifuwanttoseepixmap
    ;;(wand-render *grace* 0 0 1 -1)

    (progn
      (gl-scalef 0.006  0.006  0.0)
      (gl-disable gl_lighting)
      (gl-translatef -250 -300 -100)
      (gl-enable gl_texture_2d)
      (loop repeat 4 do
            (ftgl-render *jmc-font* "Dr. John McCarthy")
            (gl-rotatef 90 0 0 1))
      (gl-translatef 100 200 100)
      )

    )
  (togl-swap-buffers (togl-ptr self))
  #+shhh (print-frame-rate self))

(defmethod togl-create-using-class ((self nehe06))
  (cello-gl-init)
  (gl-enable gl_texture_2d)
  (gl-shade-model gl_smooth)
  (gl-clear-color 0 0 0 1)
  (gl-clear-depth 1)
  (gl-enable gl_depth_test)
  (gl-depth-func gl_lequal)
  (gl-hint gl_perspective_correction_hint gl_nicest)
  (setf *jmc-font* (ftgl-make :texture 'sylfaen 48 96 18))
  (setf *skin6* (mgk:wand-ensure-typed 'wand-texture
                  (test-image "jmcbw512" "jpg")))
  (setf *grace* (mgk:wand-ensure-typed 'wand-pixels
                  (test-image "grace" "jpg")))) ; "turing" "gif"))))

(defun print-frame-rate (window)
  (with-slots (frame-count t0) window
    (incf frame-count)
    (let ((time (get-internal-real-time)))
      (when (= t0 0)
        (setq t0 time))
      (when (>= (- time t0) (* 5 internal-time-units-per-second))
        (let* ((seconds (/ (- time t0) internal-time-units-per-second))
               (fps (/ frame-count seconds)))
          (declare (ignorable fps))
          #+shh (format *terminal-io* "~D frames in ~3,1F seconds = ~6,3F FPS~%"
                  frame-count seconds fps))
        (setq t0 time)
        (setq frame-count 0)))))

(defun test-image (filename filetype &optional (subdir "shapers"))
  (make-pathname
    :directory `(:absolute "0dev" "user" "graphics" ,subdir)
    :name (string filename)
    :type (string filetype)))
