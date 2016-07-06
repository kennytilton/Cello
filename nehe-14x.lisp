;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; nehe-14.lisp --- Celtk/Togl version of
;;;                  nehe lesson 14 spinning text string
;;;

(defpackage :nehe-06
  (:use :common-lisp :utils-kt :cells :celtk :kt-opengl :cl-ftgl))

(in-package :nehe-06)

(defparameter g_rot 0.0f0)
(defvar *frames*)
(defvar *start*)
(defvar *test-fonts*)

(defun test-font (mode)
  (cdr (assoc mode *test-fonts*)))

#+test
(nehe-14)

(defun nehe-14 () ;; ACL project manager needs a zero-argument function, in project package
  (setf ogl::*gl-begun* nil)
  (setq *test-fonts*
    (mapcar (lambda (mode)
       (cons mode (ftgl-make mode *gui-style-default-face* 48 96 18)))
     '(:texture :pixmap :bitmap :outline :polygon :extruded)))
  (test-window 'nehe-14-demo))

(defmodel nehe-14-demo (window)
  ()
  (:default-initargs
      :title$ "NeHe's OpenGL Framework"
    :kids (c? (the-kids
               (mk-stack (:packing (c?pack-self))
                 (make-instance 'nehe14
                   :fm-parent *parent*
                   :width 400 :height 400
                   :timer-interval 1 #+later (c? (let ((n$ (md-value (fm-other :vtime))))
                                         (format nil "~a" (max 1 (or (parse-integer n$ :junk-allowed t) 0)))))
                   :double 1 ;; "yes"
                   ))))))

(defmodel nehe14 (togl)
  ((frame-count :cell nil :initform 0 :accessor frame-count)
   (t0 :cell nil :initform 0 :accessor t0)
   ;
   (width :initarg :wdith :initform 640 :accessor width)
   (height :initarg :wdith :initform 400 :accessor height)))

(defmethod togl-timer-using-class ((self nehe14))
  (trc nil "enter nehe-14 timer" self (togl-ptr self) (get-internal-real-time))
  (togl-post-redisplay (togl-ptr self)))

(defmethod togl-reshape-using-class ((self nehe14))
  (let ((width (togl-width (togl-ptr self)))
        (height (togl-height (togl-ptr self))))
    (trc "reshape" width height)
    (unless (or (zerop width) (zerop height))
      (trc "reshape" width height)
      (gl-viewport 0 0 width height)
      (gl-matrix-mode gl_projection)
      (gl-load-identity)
      (glu-perspective 70 1 1 1000)
      (glu-look-at 0d0 0d0 5d0 0d0 0d0 0d0 0d0 1d0 0d0)
  
      (gl-matrix-mode gl_modelview)
      (gl-load-identity)
      (gl-clear-depth 1d0))))

(defmethod togl-display-using-class ((self nehe14))
  (incf *frames*)
  (gl-load-identity)      ;; Reset The Current Modelview Matrix
  (gl-clear-color 0 0 0 1)
  (gl-clear (+ gl_color_buffer_bit gl_depth_buffer_bit))
  
  (gl-translatef 0.0f0 0.0f0 2.0f0)   ;; Move Into The Screen
  ;; Pulsing Colors Based On The Rotation
  (gl-color3f (* 1.0f0 (cos (/ g_rot 20.0f0)))
       (* 1.0f0 (sin (/ g_rot 25.0f0)))
       (- 1.0f0 (* 0.5f0 (cos (/ g_rot 17.0f0)))))

  (gl-scalef 0.006  0.006  0.0)
  (gl-disable gl_lighting)
  (gl-translatef -100 -200 0)
  (gl-enable gl_texture_2d)
  (ftgl-render (test-font :texture)
    (format nil "texture ~d" (floor (/ *frames*
                                      (max 1 (- (now) *start*))))))
  (gl-translatef 100 200 0)

  (gl-translatef -100 200 0)
  (gl-line-width  3)
  (ftgl-render (test-font :outline) "un-rotated outline")
  (gl-translatef 100 -200 0)

  (gl-translatef -200 100 0)
  (ftgl-render (test-font :polygon) "un-rotated polygon")
  (gl-translatef 200 -100 0)

  (with-matrix ()
    (gl-polygon-mode gl_front_and_back gl_line)
    (gl-rotatef g_rot 1.0f0 0.5f0 0.0f0)
    (gl-scalef 4 4 4)
    (gl-translatef -70 -20 0)
    (ftgl-render (test-font :extruded) "NeHe")
    (gl-polygon-mode gl_front_and_back gl_fill)
    )

  (gl-rotatef g_rot 1.0f0 0.0f0 0.0f0)   ;; Rotate On The X Axis
  (gl-rotatef (* g_rot 1.5f0) 0.0f0 1.0f0 0.0f0) ;; Rotate On The Y Axis
  (gl-rotatef (* g_rot 1.4f0) 0.0f0 0.0f0 1.0f0) ;; Rotate On The Z Axis

  (gl-push-matrix)
  
  (gl-enable gl_texture_2d)
  (gl-disable gl_lighting)
  (ftgl-render (test-font :texture) "NeHe 14 texture")
  
  (gl-raster-pos3i 10 10 0)
  (ftgl-render (test-font :pixmap) "NeHe 14 pixmap")

  (gl-raster-pos3i 10 -30 0)
  (ftgl-render (test-font :bitmap) "NeHe 14 bitmap")

  (gl-pop-matrix)
  (togl-swap-buffers (togl-ptr self))
  (incf g_rot 0.4f0))


(defmethod togl-create-using-class ((self nehe14))
  (setf *start* (now)
    *frames* 0)
  (gl-matrix-mode gl_projection)
  (gl-load-identity)
  (glu-perspective 70 1 1 1000)
  (glu-look-at 0d0 0d0 5d0 0d0 0d0 0d0 0d0 1d0 0d0)
  
  (gl-matrix-mode gl_modelview)
  (gl-load-identity)
  (gl-clear-depth 1d0))

#+wait
(defmethod togl-display-using-class ((win nehe14))
  (progn ;; flet ((test-font (id) (cdr (assoc id (font-cache win)))))
    (ogl::with-matrix-mode GL_MODELVIEW
      (gl-load-identity)
      (gl-clear-color 0.0 0.0 0.0 1.0)
      
      (gl-clear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      
      ;; --- you are here ---
      (gl-color3f 1.0 1.0 0.0)
      (gl-rectf -0.01 0.01 0.01 -0.01)
      
      ;; --- pulsating color ---
      (gl-color3f (* 1.0f0 (cos (/ (rot win) 20.0f0)))
                  (* 1.0f0 (sin (/ (rot win) 25.0f0)))
                  (- 1.0f0 (* 0.5f0 (cos (/ (rot win) 17.0f0)))))
      (incf (rot win) (rot-delta win))
      
      (gl-scalef 0.003  0.003  0.0)
      (gl-disable gl_lighting)
      
      ;; --- bitmap ---
      (gl-raster-pos3i 10 -30 0)
      (ftgl-render (test-font :bitmap) "un-rotated bitmap")
      
      ;; --- pixmap ---
      (gl-raster-pos3i 10 10 0)
      (ftgl-render (test-font :pixmap) "un-rotated pixmap")
      
      ;; --- pixmap ---
      (gl-raster-pos3i 60 -120 0)
      (ftgl-render (test-font :pixmap)
                   (format nil "fps=~d"
                           (floor (/ (rot win) (rot-delta win))
                                  (max 1 (- (time-now win) (start-time win))))))
      
      ;; --- polygon ---
      (with-matrix ()
        (gl-translatef -100 100 0)
        (ftgl-render (test-font :polygon) "un-rotated polygon"))
      
      ;; --- outline ---
      (with-matrix ()
        (gl-translatef -100 50 0)
        (gl-line-width  3)
        (ftgl-render (test-font :outline) "un-rotated outline"))
      
      ;; --- extruded polygon ---
      (with-matrix ()
        (gl-polygon-mode gl_front_and_back gl_line)
        (gl-rotatef (rot win) 1.0f0 0.5f0 0.0f0)
        (gl-scalef 5 5 5)
        (gl-translatef -70 -20 0)
        (gl-line-width 1)
        (ftgl-render (test-font :extruded) "NeHe")
        (gl-polygon-mode gl_front_and_back gl_fill))
      
    ;;; --- texture ---
    (with-matrix ()
        (gl-rotatef (rot win) 1.0f0 0.0f0 0.0f0)
        (gl-rotatef (* (rot win) 1.5f0) 0.0f0 1.0f0 0.0f0)
        (gl-rotatef (* (rot win) 1.4f0) 0.0f0 0.0f0 1.0f0)
        (gl-enable gl_texture_2d)
        (gl-disable gl_lighting)
        (ftgl-render (test-font :texture) "NeHe Lesson 14"))
      )

    (glut-swap-buffers)
    (glut-post-redisplay)))
