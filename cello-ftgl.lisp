;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
#|

Copyright (C) 2004 by Kenneth William Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cello)

(defmethod font-height ((font ftgl))
  (+ (abs (font-ascent font))
    (abs (font-descent font))))

(defmethod font-ascent ((font ftgl))
  (trc nil "ascender" (ftgl-get-ascender font))
  (scr2log (ftgl-get-ascender font)))

(defmethod font-descent ((font ftgl))
  "Returns a negative measure of display below font origin"
  (trc nil "descender" (ftgl-get-descender font))
  (scr2log (ftgl-get-descender font)))

(defmethod font-string-length ((font ftgl) string &optional start end)
  (when start
    (unless end
      (setf end (length string))))
  (ftgl::dbgftgl :font-string-length
    (ftgl-string-length font (if (or start end)
                                 (subseq string start end)
                               string))))

(defun font-ftgl-ensure (mode face size) ;; ///sorry about the silly naming
  (trc nil "font-ftgl-ensure requesting" mode face size)
  (ftgl-font-ensure mode face size (cs-target-res)))

(defmodel font-id (ct-toggle ix-text)
  ((font-pathname :initarg :font-pathname :accessor font-pathname))
  (:default-initargs
      :style nil
    :pre-layer (with-layers
                   (:rgba (if (^value) +red+ +black+)))
    :text-font (c? (font-ftgl-ensure :texture 
                    (intern (^font-pathname)) 14))
    :text$ (c? (string-capitalize
                (or (^font-pathname) ;; ever not?
                  "Blank")))))

(defobserver mouse-over? ((self font-id))
  (when new-value
    (setf (value (fm-other :ftgl-test)) (^font-pathname))))

(export! gui-style-ftgl)

(defclass gui-style-ftgl (gui-style gui-style-sizable)
  ((mode :initarg :mode :accessor mode :initform :texture)))

(defmethod make-style-font (style)
  (break "no font for style ~a" style))

(defmethod make-style-font ((style gui-style-ftgl))
  (font-ftgl-ensure (mode style) (face style) (gui-style-size style)))

(defun ftgl-debug ()
  (let (*tkw*)
    (with-styles (
                  (make-instance 'gui-style-ftgl
                    :id :button 
                    :face *gui-style-button-face*
                    :sizes '(12 12 12 12 12)
                    :text-color +white+)
                  (make-instance 'gui-style-ftgl
                    :id :label 
                    :face *gui-style-button-face*
                    :sizes '(14 14 14 14 14)
                    :text-color +white+)
                  (make-instance 'gui-style-ftgl
                    :id :unique 
                    :face *gui-style-button-face*
                    :sizes '(24 24 24 24 24)
                    :text-color +white+)
                  (make-instance 'gui-style-ftgl
                    :id :unique2
                    :face *gui-style-button-face*
                    :sizes '(18 18 18 18 18)
                    :text-color +white+)
                  (make-instance 'gui-style-ftgl
                    :id :default 
                    :mode :texture
                    :face *gui-style-button-face*
                    :sizes '(14 9 14 14 14)
                    :text-color +green+))
      (run-cello-window 'ftgl-window
        (lambda ()
          ;;; -- not sure how much of this new reset stuff is necessary ---
          (kt-opengl-init)
          (cl-ftgl-reset)
          (cl-ftgl-init))))))

(defmodel ftgl-window (cello-window)
  ()
  (:default-initargs
    :ll 0 :lt 0
    :lr (c-in (scr2log 900))
    :lb (c-in (scr2log -900))
    :md-name :ftgl-w
    :title$ "Hello, ftgl"
    :skin nil 
    :lighting :off
    :pre-layer (c? (with-layers +blue+ :off))
    :clipped nil
    :kids (c? (the-kids
               (a-stack (:md-name :ftgl-debug :spacing (upts 10) :px 0 :py (downs (uin 1))
                          :justify :left
                          :outset (u8ths 1))
                 (loop for s in (list "hell" ;;"hlwr" ;;"hlwr 1212"
                                  "hi2"
                                  "hello, world 222" "1212"
                                  )
                     for n upfrom 0
                     collecting (mk-part :sample (ix-text)
                                  :lighting :off
                                  :text$ s
                                  :style-id :unique
				  :fm-parent *parent*
                                  :pre-layer (c? (with-layers (:rgba (if (^mouse-over?)
                                                                         +red+ +blue+)))))))))))
#+(or) 
(ftgl-test)

#+vestigial?
(defun ftgl-test ()
  (cl-ftgl-init)
  (let ((fns (mapcar (lambda (p)
                       (pathname-name p))
               (butlast (directory *font-directory-path*) 0)))
        (cols 8))
    (flet ((mk-font-show (col-no row-no)
             (when (nth (+ (* cols row-no) col-no) fns)
               (mk-part :ftest (font-id)
                 :font-pathname (c? (let ((row-no (kid-no self)))
                                      (eko (nil "font show")
                                        (elt fns (+ (* cols row-no) col-no)))))))))
      (a-stack (:md-name :ftgl-test :spacing (upts 10) :px 0 :py (uin 1)
                :value (c-in (car fns))
                :justify :left
                :outset (u8ths 1))
        (a-stack (:lb (downs (upts 64))
                  :justify :center
                  :outset (upts 8)
                  :pre-layer (c? (when (value (fm-other :ftgl-test))
                                   (with-layers
                                       :on +gray+ (:frame-3d :edge-sunken
                                                    :thickness (u96ths 4))
                                     :off +white+ :fill +black+))))
          (loop for line below 2
              collect (mk-part :sample (ix-text)
                        :lighting :off
                        :text$ (nth line
                                 (list "Ah, would that the Gods had this gift to gie us,"
                                   "to see ourselves as others see us"))
                        :style nil
                        :pre-layer (with-layers +black+)
                        :text-font (c? (font-ftgl-ensure
                                        (car (value (fm-other :mode))) 
                                        (intern (value (fm-other :ftgl-test)))
                                        18 ;; (* 12 (1+ (mod x 4)))
                                        )))))
        (mk-part :mode (ct-radio-row)
          :spacing (upts 4)
          :value (c-in (list :texture))
          :clipped nil
          :kids (c? (loop for mode in '(:bitmap :pixmap :texture :outline :polygon :extruded)
                        collect (mk-part :rb (ct-radio-labeled)
                                  :associated-value mode
                                  :title$ (string-capitalize
                                           (format nil "~d" mode))))))
        (mk-part :ftgrow (ix-row)
          :pre-layer (with-layers +white+ :fill)
          :kids (c? (the-kids
                     (loop repeat cols
                           collecting
                           (mk-part :fstk (ix-inline)
                             :orientation :vertical
                             :kids (c? (let ((col-no (kid-no self)))
                                         (loop for row-no below (ceiling (length fns) cols)
                                             when (mk-font-show col-no row-no)
                                             collect it))))))))))))


(defmethod ix-align-text (self (font ftgl-pixmap))
  (ecase (justify-hz self)
    (:left)
    (:center
     (ogl-pen-move (round (- (l-width self) (^text-width)) 2) 0))
    (:right
     (ogl-pen-move (- (l-width self) (v2-h (inset self)) (^text-width)) 0))))

(defmethod ix-align-text (self (font ftgl-bitmap))
  (ecase (justify-hz self)
    (:left)
    (:center
     (ogl-pen-move (round (- (l-width self) (^text-width)) 2) 0))
    (:right
     (ogl-pen-move (- (l-width self) (v2-h (inset self)) (^text-width)) 0))))

(defmethod ix-render-in-font :around ((font ftgl) self)
  (bwhen (t$ (display-text$ self))
    (ix-align-text self font)
    
    (if (ogl-get-boolean gl_current_raster_position_valid)
        (call-next-method)
      (trc "rasterpos offscreen" self :g-offset (g-offset self)))))

(defmethod ix-render-in-font ((font ftgl) self)
  (count-it :render-in-font)
  (ftgl-render font (display-text$ self)))

(defmethod ix-render-in-font ((font ftgl-outline) self)
  (with-attrib (t gl_enable_bit gl_hint_bit gl_line_bit gl_color_buffer_bit)
    (gl-disable gl_texture_2d)
    (gl-enable gl_line_smooth)
    (gl-hint gl_line_smooth_hint gl_dont_care)
    (gl-enable gl_blend)
    (gl-blend-func gl_src_alpha gl_one_minus_src_alpha)
    (ftgl-render font (display-text$ self))))

(defmethod ix-render-in-font ((font ftgl-texture) self)
  (let* ((t$ (display-text$ self)))
    (trc nil "ix-render-in-font ftgl-texture" :pxy (pxy self) (l-rect self) t$)
    
    #+youarehere
    (let ((ll (^ll))(lr (^lr))(lt (^lt))(lb (^lb))) ;; keep outside gl-begun since can kick off FTGL glyph build
      ;(gl-color3f 0 0 0)
      (gl-line-width 1)
      (with-gl-begun (gl_lines)
        (gl-vertex3f 0 0 0)(gl-vertex3f ll 0 0)
        (gl-vertex3f 0 0 0)(gl-vertex3f lr 0 0)
        (gl-vertex3f 0 0 0)(gl-vertex3f 0 lt 0)
        (gl-vertex3f 0 0 0)(gl-vertex3f 0 lb 0)
        ))

    (gl-enable gl_texture_2d)
    (trc nil "(gl-is-enabled gl_texture_2d)!!!!!!!" (gl-is-enabled gl_texture_2d)
      (ogl-get-boolean gl_texture_2d))
    ;;(assert (ogl-get-boolean gl_texture_2d))
    (gl-disable gl_lighting)
    (gl-enable gl_blend)
    (gl-blend-func gl_src_alpha gl_one_minus_src_alpha)
    (gl-polygon-mode gl_front_and_back gl_fill)


    
    (when (zoom self)
      (apply 'gl-scalef (zoom self)))
    
    (when (rotation self)
      (apply 'gl-rotatef (rotation self)))
    
    (ftgl-render font t$)))



;;;(defmethod not-to-be :after ((w window))
;;;  (loop for font-entry in (w-fonts w)
;;;        for ff = (cdr font-entry)
;;;        do (bwhen (cfont (ftgl-ifont ff))
;;;             (trc nil "freeing ff" ff cfont)
;;;             (fgc-free cfont))))

