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

(defclass wand-texture (wand-image ogl-texture)())

(defmethod wand-release :after ((wand wand-texture))
  (when (slot-value wand 'texture-name)
    (ogl-texture-delete (slot-value wand 'texture-name))))
  
(defun best-fit-cons (c1 c2 c3)
  (flet ((bfit (a b c)
	   (if (> (/ c b)(/ b a))
	       a c)))
    (cons (bfit (car c1)(car c2)(car c3))
	  (bfit (cdr c1)(cdr c2)(cdr c3)))))
  
(defmethod texture-name :around ((self wand-texture))
  (or (call-next-method)
    ;;;    (let ((tx (wand-image-to-texture self)))
    ;;;      (if (plusp tx)
    ;;;          (setf (texture-name self) tx)
    ;;;        (break "bad tx name ~a for ~a" tx self)))))
    
    ;;; 
    ;;; this next stuff converts image to 2^n dimensions and may still be necessary
    ;;; on older graphics cards. /// test for this on old or lame PCs
    ;;;
    (let* ((trunc-sz (cons (expt 2 (floor (log (car (image-size self)) 2)))
                       (expt 2 (floor (log (cdr (image-size self)) 2)))))
           (grow-sz (cons (expt 2 (ceiling (log (car (image-size self)) 2)))
                      (expt 2 (ceiling (log (cdr (image-size self)) 2)))))
           (best-fit-sz (best-fit-cons trunc-sz (image-size self) grow-sz)))
      ;;(print `(texture-name> gennning texture ,self)) ;; frgo: debug...
      
      (unless (equal (image-size self) best-fit-sz)
        ;(print `(texture-name> ,(image-path self) tex-refit ,(image-size self) to ,best-fit-sz))
        (magick-scale-image (mgk-wand self) (car best-fit-sz) (cdr best-fit-sz))
        ;;; gaussian-filter 0)
        (setf (image-size self) best-fit-sz))
      
      ;;(print `(texture-name> new image size , self ,(image-size self))) ;; frgo: debug...
      (let ((tx (wand-image-to-texture self)))
        (if (plusp tx)
            (setf (texture-name self) tx)
          (break "bad tx name ~a for ~a" tx self))))))
  
  
(defun wand-texture-activate (wand)
  ;;(print `(wand-texture-activate ,(texture-name wand)))
  (ogl-tex-activate (texture-name wand)))
  
(defparameter *textures-1* (fgn-alloc 'kt-opengl::gluint 1 :ignore))

(defun wand-image-to-texture (self)
  ;;(cells::trcx wand-image-to-texture (image-path self))
  (let ((tx (ogl-texture-gen))
        (pixels (wand-get-image-pixels self)))
    ;;(assert (not *ogl-listing-p*))
    (assert (plusp tx))
    (cells:trc nil "!!!!wand-image-to-texture genning new tx: ~a" tx) ;; frgo: debug...
    (gl-bind-texture gl_texture_2d tx)
      
    (progn ;; useless??
      (gl-tex-parameteri gl_texture_2d gl_texture_wrap_s gl_repeat)
      (gl-tex-parameteri gl_texture_2d gl_texture_wrap_t gl_repeat) ;--
        
      (gl-tex-parameterf gl_texture_2d gl_texture_min_filter gl_linear )
      (gl-tex-parameterf gl_texture_2d gl_texture_mag_filter gl_linear ))
      
    (gl-pixel-storei gl_pack_alignment 1 )
    (gl-pixel-storei gl_unpack_alignment 1 )
    (cells::trc nil "wand-image-to-texture> tex-iage2d-ing" (image-path self)(image-size self))
    (kt-opengl::glec :tex-image-before)
    (gl-tex-image2d  gl_texture_2d 0 gl_rgba (car (image-size self)) (cdr (image-size self))
		     0 (storage self) gl_unsigned_byte pixels)
    (kt-opengl::glec :tex-image)
    
    ;;(print `(wand-image-to-texture loaded texture sized ,(image-size self))) ;; frgo: debug...
      
    (fgn-free pixels)
    tx)) 

#|

To avoid changing the texture, use GL_MODULATE mode (glTexEnv)
and use glColor4f (1.0, 1.0, 1.0, alpha).

This multiplies 'alpha' by the alpha in the RGBA texture map
before blending into the frame buffer. The constants you mentioned
are for that later blending stage. 

|#
  
(defmethod wand-render ((self wand-texture) left top right bottom
                        &aux (sz (image-size self)))
  #+not (cells:trc nil "wand-render tex-name:" (texture-name self) (tilep self) self
          :size sz :bbox (list left top right bottom))
  
  (with-attrib (t gl_texture_bit gl_color_buffer_bit) ;; gl_enable_bit gl_hint_bit gl_line_bit gl_color_buffer_bit) 
    (wand-texture-activate self)
    
    (gl-enable gl_blend)
    (gl-blend-func gl_src_alpha gl_one_minus_src_alpha)
    
    (gl-enable gl_alpha_test)
    (gl-alpha-func gl_greater 0.0)
    
    #+not
    (progn
      (ogl-tex-gen-setup gl_object_linear gl_modulate
        (if (tilep self) gl_repeat gl_clamp)
        (/ 1 (max (car sz)(cdr sz)))
        :s :tee :r)
      (gl-rectf left top right bottom))
    
    (if (tilep self)
        (with-gl-begun (gl_quads)
          (loop for y from top above bottom by (cdr sz)
              for y-rem = (- bottom y)
                
              do (loop for x from left below right by (car sz)
                     for x-rem = (- right x)
                     do ;; (print `(tex tiling ,x ,y))
                       
                       (flet ((vxy (tx ty)
                                (let ((x-fraction (min tx (/ x-rem (car sz))))
                                      (y-fraction (min ty (abs (/ y-rem (cdr sz))))))
                                  (gl-tex-coord2f x-fraction y-fraction)
                                  (gl-vertex3f (+ x (* x-fraction (car sz)))
                                    (+ y (downs (* y-fraction (cdr sz)))) 0))))
                         (vxy 0 0)(vxy 1 0)(vxy 1 1)(vxy 0 1)))))
      (flet ((vxy (tx ty)
               (let ((abs-x (+ left (* tx (- right left))))
                     (abs-y (+ top (downs (* ty (abs (- top bottom)))))))
                 ;(print `(tex full,(cons tx ty) to-vertex ,(cons abs-x abs-y)))
                 (gl-tex-coord2f tx ty)
                 (gl-vertex3f abs-x abs-y 0))))
        
        (with-gl-begun (gl_quads)
          (vxy 0 0)(vxy 0 1)(vxy 1 1)(vxy 1 0)))
      )))