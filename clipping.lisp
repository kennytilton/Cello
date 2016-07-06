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
           
(defvar *render-clip-l*)
(defvar *render-clip-r*)
(defvar *render-clip-t*)
(defvar *render-clip-b*)

(defun ogl-is-enabled (key)
  (not (zerop (gl-is-enabled key))))

(defun call-with-clipping (self clipped-fn)
  (declare (ignorable self))
  ;(funcall clipped-fn)
  ;#+not
  (if (not (clipped self))
        (macrolet ((adj-unclipped (g p)
                     `(when ,g (- ,g (,p self)))))
          (let ((*render-clip-l* (adj-unclipped *render-clip-l* px))
                (*render-clip-r* (adj-unclipped *render-clip-r* px))
                (*render-clip-t* (adj-unclipped *render-clip-t* py))
                (*render-clip-b* (adj-unclipped *render-clip-b* py))
                )
            (funcall clipped-fn)))
    (progn ;; wtrc (0 200 "call-with-clipping" self (^ll) (^lr) (^px) (screen-box self) (l-box .tkw))
        (macrolet ((adj-clipped (mm g l p)
                   `(if ,g
                        (,mm (,l self)
                          (- ,g (,p self)))
                      (,l self))))
          ;(trc "*render-clip-l* begins" *render-clip-l*)
          (let ((*render-clip-l* (adj-clipped max *render-clip-l* ll px))
                (*render-clip-r* (adj-clipped min *render-clip-r* lr px))
                (*render-clip-t* (adj-clipped downs-most *render-clip-t* lt py))
                (*render-clip-b* (adj-clipped ups-most *render-clip-b* lb py))
                )
            ;(trc "*render-clip left" *render-clip-l* :right *render-clip-r*)
            (let* ((gr (g-box self))
                   (togr (g-box (u^ ix-togl)))
                   (clip-restore0 (ix-clip self gl_clip_plane0
                                    :xl (- (/ (r-left gr)
                                              (/ (r-width togr) 2)) 1)))
                   (clip-restore1 (ix-clip self gl_clip_plane1
                                    :xr (- (/ (r-right gr)
                                             (/ (r-width togr) 2)) 1)))
                   (clip-restore2 (ix-clip self gl_clip_plane2
                                    :yt (1+ (/ (r-top gr)
                                              (/ (r-height togr) 2)))))
                   (clip-restore3 (ix-clip self gl_clip_plane3
                                    :yb (1+ (/ (r-bottom gr)
                                              (/ (r-height togr) 2)))))
                   #+not (scissor-box (if (ogl-is-enabled gl_scissor_test)
                                          (progn
                                            (trc  "NESTED scissor on" self)
                                            (ogl-scissor-box))
                                        (progn
                                          (trc  "toplevel scissor on" self)
                                          (gl-enable gl_scissor_test)
                                          nil))))
              #+shh (let ((old-eqn (cdr clip-restore2)))
                (trc "just clipped top" self (lt self)
                  :restore (list (eltd old-eqn 0)(eltd old-eqn 1)
                             (eltd old-eqn 2)(eltd old-eqn 3))))
              ;(trc "just clipped right" self (lr self) :restore clip-restore1)
             
              ;(trc "just clipped" self (ll self)(lr self)(lt self)(lb self))
              ;(ix-clip-dump "just clipped dumped")
              (count-it :ix-clipping)
              #+not
              (let* ((wbl (w-bottom-left self))
                     (sx (floor (v2-h wbl)))
                     (sy (floor (v2-v wbl)))
                     (sw (ceiling (l-width self)))
                     (sh (ceiling (l-height self))))
                (gl-scissor sx sy sw sh)
                (ogl::glec :scissor)
                (trc "just  scissored" :wbl wbl :sxy sx sy :swh sw sh)
                (trc "...with rasterpos at" (ogl-raster-pos-get))
                )
              (prog1
                  (funcall clipped-fn)
                (ix-clip-undo self gl_clip_plane0 clip-restore0)
                (ix-clip-undo self gl_clip_plane1 clip-restore1)
                ;(trc "just restored left/right" self (ll self))
                (ix-clip-undo self gl_clip_plane2 clip-restore2)
                (ix-clip-undo self gl_clip_plane3 clip-restore3)
                #+not (if scissor-box
                          (progn
                            (trc " restoring  scissor" self (ogl-bounds scissor-box))
                            (apply 'gl-scissor (ogl-bounds scissor-box))
                            )
                        (gl-disable gl_scissor_test)))))))))

(defun ix-clip-undo (self p was-on?-old-eqn)
  (declare (ignorable self))
  (destructuring-bind (was-on? . old-eqn) was-on?-old-eqn
    (trc nil "restoring clip. me:" (md-name self) (eltd old-eqn 0)(eltd old-eqn 1)
      (eltd old-eqn 2)(eltd old-eqn 3))
    (gl-clip-plane p old-eqn)
    (unless was-on?
      ;(trc "no caller was clipping, disabling clip" p)
      (gl-disable p))))

(defparameter *clipper* (make-ff-array gldouble 0 0 0 0))

(defun ix-clip-dump (msg)
  msg
  (trc nil "clipdump" msg
    (maptimes (pn 4)
      (when (ogl-get-boolean (+ gl_clip_plane0 pn))
        (gl-get-clip-plane (+ gl_clip_plane0 pn) *clipper*)
        (floor (eltd *clipper* 3))))))



(defun ix-clip (self p how eqn)
  (declare (ignorable p self))
  ;;
  ;; A = y1 (z2 - z3) + y2 (z3 - z1) + y3 (z1 - z2)
  ;; B = z1 (x2 - x3) + z2 (x3 - x1) + z3 (x1 - x2)
  ;; C = x1 (y2 - y3) + x2 (y3 - y1) + x3 (y1 - y2)
  ;; - D = x1 (y2 z3 - y3 z2) + x2 (y3 z1 - y1 z3) + x3 (y1 z2 - y2 z1)        
  ;;
  (let ((x1 0)(y1 0)(z1 0)(x2 0)(y2 0)(z2 0)(x3 0)(y3 0)(z3 0)
        (clipping-was-on? (ogl-get-boolean p))
        (old-eqn (make-ff-array gldouble 0 0 0 0)))
    ;
    ; get current clip this plane for restore by caller
    ;
    (gl-get-clip-plane p old-eqn)
    (trc nil "saving anothers clip. me:" self clipping-was-on? (eltd old-eqn 0)(eltd old-eqn 1)
      (eltd old-eqn 2)(eltd old-eqn 3))
    
    (ecase how
      (:xl (setq x1 eqn x2 eqn x3 eqn)
        (setq y2 1 y3 1)
        (setq z3 (nearer 1)))
      (:xr (setq x1 eqn x2 eqn x3 eqn)
        (setq y2 1 y3 1)
        (setq z3 (farther 1)))
      (:yt (setq y1 eqn y2 eqn y3 eqn)
        (setq x2 1 x3 1)
        (setq z3 (nearer 1)))
      (:yb (setq y1 (+ eqn) y2 (+ eqn) y3 (+ eqn))
        (setq x2 1 x3 1)
        (setq z3 (farther 1))))
    
    (setf (eltd *clipper* 0)
      (+ (* y1 (- z2 z3)) (* y2 (- z3 z1)) (* y3 (- z1 z2))))
    
    (setf (eltd *clipper* 1)
      (+ (* z1 (- x2 x3)) (* z2 (- x3 x1)) (* z3 (- x1 x2))))
    
    (setf (eltd *clipper* 2)
      (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2))))
    
    ;; - D = x1 (y2 z3 - y3 z2) + x2 (y3 z1 - y1 z3) + x3 (y1 z2 - y2 z1)     
    ;;;    (trc "clipping :x1" x1 :y1 y1 :z1 z1)
    ;;;    (trc "clipping :x2" x2 :y2 y2 :z2 z2)
    ;;;    (trc "clipping :x3" x3 :y3 y3 :z3 z3)
    (setf (eltd *clipper* 3)
      (- (+ (* x1 (- (* y2 z3) (* y3 z2)))
           (* x2 (- (* y3 z1) (* y1 z3)))
           (* x3 (- (* y1 z2) (* y2 z1))))))
    
    (trc nil "clipping myself:" self :plane (- p gl_clip_plane0)
      (eltd *clipper* 0)(eltd *clipper* 1)
      (eltd *clipper* 2)(eltd *clipper* 3))
    
    (progn
      (gl-clip-plane p *clipper*)
      #+nah (progn
              (gl-get-clip-plane p *clipper*)
              (trc "ix-clip just set/read" test (floor (eltd *clipper* 3))))
      (gl-enable p))
    
    (cons clipping-was-on? old-eqn)))

#+test
(defun ix-clip (self p how eqn)
  (declare (ignorable p how self eqn))
  (let ((clipping-was-on? (ogl-get-boolean p))
        (old-eqn (make-ff-array gldouble 0 0 0 0)))
    (flet ((clip-it ()
             (trc "clipping myself:" how eqn :gbox (screen-box self) :w (screen-box .tkw)
               :plane (- p gl_clip_plane0)
               :bounds (eltd *clipper* 0)(eltd *clipper* 1)(eltd *clipper* 2)(eltd *clipper* 3)
               )
             (gl-clip-plane p *clipper*)
             (gl-enable p)))
      (gl-get-clip-plane p old-eqn)
      (trc nil "saving anothers clip. me:" how clipping-was-on? (eltd old-eqn 0)(eltd old-eqn 1)
        (eltd old-eqn 2)(eltd old-eqn 3))
      
      (case how
        (:xl 
         (setf (eltd *clipper* 0) 1
           (eltd *clipper* 3) (/ 600 700))
         (clip-it))
        (:xr
         (setf (eltd *clipper* 0) -1
           (eltd *clipper* 3) (/ 551 700))
         (clip-it)
         ))
      (cons clipping-was-on? old-eqn))))
