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

(defmethod ix-paint :after ((self family))
  (let ((kids (or (render-order self) (kids self))))
    (declare (ignorable kids))
    #+chyaaaa
    (block chk1
      (dolist (k kids)
        (unless (find k (kids self))
          (trc "1. kid ~a amongst ~a, no longer amongst kids ~a" k kids (kids self))
          (break "1. kid ~a amongst ~a, no longer amongst kids ~a" k kids (kids self))
          (return-from chk1))))
    (loop for k in kids
          when (visible k)
          do
          (trc nil "ixr geo" k (list (px k)(py k)) (list (ll k)(lt k)(lr k)(lb k)))
          (trc nil "render kid pxy" k (px k)(py k)
            :rpos-before (ogl-get-boolean gl_current_raster_position_valid)
            (ogl-raster-pos-get))
          #+chyaaa (assert (find k (kids self))() "kid ~a no longer amongst kids ~a" k (kids self))
          ;(c-assert (px k) () "pX is null in ~a" k)
          ;(c-assert (py k) () "pY is null in ~a" k)    
    
          (if (dsp-list k)
              (progn
                (count-it :call-list)
                ;(trc "ix-paint calling list" (dsp-list k) k)
                (gl-call-list (dsp-list k))) ;; 06/06/29 edit caret presences
            ;; causes INVALID_OP on
            ;; first run only in a session;
            ;; just continue from
            (ix-paint k)))))

(defun rpchk (id pfail psucc &optional self)
  (declare (ignorable pfail psucc))
  (if (not (ogl-get-boolean gl_current_raster_position_valid))
      (trc "rasterpos INVALID" id :self self :rpos (ogl-raster-pos-get))
    #+slow (trc psucc "rasterpos OK" id :self self (ogl-raster-pos-get))))


(defmethod ix-paint (self)
  (declare (ignorable self))
  (trc nil "ix-paint fell through" self (class-of self)))

(let ((ixr-box (mkr 0 0 0 0)))
  (defmethod ix-paint :around ((self ix-view) &aux (n (gl-name self)))
    (trc nil "painting, shifting bitmap" self n (^px)(^py)(^lr)(^lt)(^ll)(^lb))
    
    (with-bitmap-shifted ((px self)(py self))
      (gl-translatef (px self) (py self) 0)
      
      (ogl-echk :ix-paint-ix-view)
      (when n
        (trc nil "pushing gl-name" self n)
        (gl-push-name n))
      
      (rpchk 'ix-paint t nil self)
      (when (and (not (c-stopped))
              (or (not *selecting*)
                (ix-selectable self))
              (visible self)
              (not (collapsed self)))
        (with-clipping (self)
          (progn ;; with.attrib (gl_lighting_bit gl_texture_bit gl_enable_bit gl_hint_bit gl_line_bit gl_color_buffer_bit)
            (count-it :ix-render)
            #+(or) (count-it :ix-paint (type-of self))
            #+(or) (unless (kids self)
                     (count-it :ix-render-atom))
            (trc nil "ix painting 22" self (^px)(^py)(l-box self))
            (with-matrix ()
              (with-ogl-isolation (or (ogl-isolate? self)(lighting self)(pre-layer self))
                  (case (lighting self) ;; default is "same as parent"
                    (:on (gl-enable gl_lighting))
                    (:off (gl-disable gl_lighting)))
                
                (gl-enable gl_color_material) ;; always?!
                
                (bif (pre-layer (pre-layer self))
                  (progn
                    (assert (functionp (ix-layers-fn pre-layer)))
                    (count-it :pre-layer)
                    (nr-make ixr-box (ll self) (lt self) (lr self) (lb self))
                    ;(trc "calling pre-layer" self)
                    (funcall (ix-layers-fn pre-layer) self ixr-box :before)
                    (call-next-method self)
                    (funcall (ix-layers-fn pre-layer) self ixr-box :after)
                    ;(trc "called pre-layer" self)
                    )
                  (call-next-method self)))))))
      (when n
        (gl-pop-name))
      (gl-translatef (- (px self)) (- (py self)) 0))
    
    ))

(defmethod ix-render-layer ((nada null) g-box)
  (break "NIL layer detected" g-box))

(defmethod ix-render-layer :around (key g-box)
  (declare (ignorable key g-box))
  (count-it :render-layer)
  (count-it :render-layer (type-of key))
  (ogl-echk :ix-render-layer)
  (call-next-method))

