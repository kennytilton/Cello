;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-magick; -*-
;;;
;;; Copyright (c) 2004 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :cl-magick)

(defclass wand-pixels (wand-image)
  ((pixels :initarg :pixels :accessor pixels :initform nil)))

(defmethod initialize-instance :after ((self wand-pixels) &key)
  (when (and (mgk-wand self) (eql :input (wand-direction self)))
    (magick-flip-image (mgk-wand self))
    (cells::trc "getting pixels for" (image-path self))
    (setf (pixels self) (wand-get-image-pixels self))))

(defmethod wand-release :after ((wand wand-pixels))
  (when (pixels wand)
    (fgn-free (pixels wand))))

(defmethod wand-render ((self wand-pixels) left top right bottom
                        &aux (sz (image-size self)))
  "only works in ortho mode I think; abstract out raster-pos for perspective"
  (declare (ignorable right left))
  (assert (pixels self))
  
  (cells:trc nil "!!!! pixelrender entry rasterpos:"
              (ogl-raster-pos-get) :lrtb (list left right top bottom)
    :image-sz sz)
  (let ((y-move (downs (+ 0 (abs (- top bottom))))))
    (with-bitmap-shifted (0 y-move)
      (cells:trc nil "wand-render pixels move" 0 y-move :top top :bottom bottom)

      (if (ogl-get-boolean gl_current_raster_position_valid)
          (progn
            #+shh (format t "~&rasterpos ~a OK: ~a" 
              (ogl-raster-pos-get) (list left right top bottom) ))
        (format t "~&in wand-render rasterpos ~a invalid, goffset is ???"
          (ogl-raster-pos-get) self ))
      #+wait (gl-pixel-zoom (/ (- right left) (car sz))
               (/ (abs (- top bottom)) (cdr sz)))
      #+not (print (list "draw pixels sz, lbox" left right (image-path self) sz
                     :tby top bottom y-move))
    
      #+shh (unless (zerop (gl-is-enabled gl_scissor_test))
        (print `(scissor-box2 ,(ogl-bounds (ogl-scissor-box)))))
      (gl-disable GL_LIGHTING)
      (gl-disable GL_COLOR_MATERIAL)
      (gl-disable GL_DEPTH_TEST)
      (gl-disable GL_cull_face)
      ;(gl-scalef 1000 1000 1000)
      ;(gl-disable gl_scissor_test) ;; debugging try
      (gl-enable gl_blend) ;; debugging try
      ;(gl-blend-func gl_src_alpha gl_one)
      ;(gl-blend-func gl_dst_alpha gl_one_minus_src_alpha)
      (gl-blend-func gl_src_alpha gl_one_minus_src_alpha)
      ;;(cells:trc "drew pixels " gl_src_alpha gl_zero)
      (gl-polygon-mode gl_front_and_back gl_fill)
      #+not (cells:trc nil "wand-pixelling" (ogl-raster-pos-get))
      (gl-pixel-storei gl_unpack_alignment 1)
      (gl-draw-pixels (+ (car sz) 0) (cdr sz)
        (storage self) gl_unsigned_byte (pixels self))
      (ogl::gl-pixel-transferf gl_alpha_scale 1)
      (ogl::glec :draw-pixels))))




