;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
;;;
;;; Copyright © 2004 by Kenneth William Tilton.
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


(in-package :cello)

(defun glut-solid-cylinder (quadric base-radius top-radius height slices stacks)
  (gl-polygon-mode gl_front_and_back gl_fill)
  (glu-quadric-texture quadric 0)
  (glu-cylinder quadric base-radius top-radius height slices stacks))

(defun glut-wire-cylinder (quadric base-radius top-radius height slices stacks)
  (gl-polygon-mode gl_front_and_back gl_line)
  (glu-cylinder quadric base-radius top-radius height slices stacks))

(defun glut-solid-cello (font)
  (glut-ftgl-cello font gl_fill))

(defun glut-wire-cello (font)
  (trc nil "string width"
    (font-string-width 96 font 
      "2Cel2lo"))
  (glut-ftgl-cello font gl_line))

(defun glut-ftgl-cello (font poly-style)
    (gl-polygon-mode gl_front_and_back poly-style)
    ;  (gl-rotatef g_rot 1.0f0 0.5f0 0.0f0)
    (gl-scalef .05 .05 .05)
    ;(gl-Scalef .1 .1 .1)
    ;(gl-Translatef -20 -20 0)
    
    (ftgl-render font "Cello"))

(defun glut-solid-nurb (nurb)
  (glu-nurbs-property nurb glu_display_mode glu_fill)
  (draw-test-nurb nurb))

(defun glut-wire-nurb (nurb)
  (glu-nurbs-property nurb glu_display_mode glu_outline_polygon)
  (draw-test-nurb nurb))

(defparameter *hill* (make-ff-array :float 0 0 0 0 1 1 1 1))
(defparameter *hill-controls*
  (let ((m 3) (d 2))
    (loop with fv = (fgn-alloc 'glfloat 48 :testnurb)
        for u below 4 do
          (loop for v below 4
              for base = (+ (* u 12) (* v 3))
              do (setf (eltf fv (+ base 0)) (- (* m u) d))
                (setf (eltf fv (+ base 1)) (- (* m v) d))
                (setf (eltf fv (+ base 2))
                  (* 3 (if (and (or (eql u 1)(eql u 2))
                             (or (eql v 1)(eql v 2)))
                           d (- d)))))
        finally (return fv))))

(defun draw-test-nurb (nurb)
  (glu-nurbs-property nurb glu_sampling_tolerance 1)
  ;(glu-nurbs-property nurb glu_auto_load_matrix gl_false)

  (gl-enable gl_lighting)
  (gl-enable gl_light0)
  (gl-enable gl_depth_test)
  (gl-enable gl_auto_normal)
  (gl-enable gl_normalize)

  ;(gl-rotatef 330 1 0 0)
  (gl-scalef .25 .25 .25)
  (glu-begin-surface nurb)
  (glu-nurbs-surface nurb 8 *hill* 8 *hill* 12 3 *hill-controls* 4 4 gl_map2_vertex_3)
  (glu-end-surface nurb)

  (gl-point-size 5)
  (gl-disable gl_lighting)
  (gl-color3f 1 1 0)
  (gl-begin gl_points)
  (loop for u below 4 do
        (loop for v below 4
            for base = (+ (* u 12) (* v 3))
            do (gl-vertex3f (eltf *hill-controls* (+ base 0))
                 (eltf *hill-controls* (+ base 1))
                 (eltf *hill-controls* (+ base 2)))))
  (gl-end))

(defparameter *sponge-offset* (loop with fv = (fgn-alloc 'gldouble 3 :sponge)
                                    for n below 3
                                    do (setf (eltd fv n) 0)
                                    finally (return fv)))

(defmethod ix-paint ((self hedron))
  ;(return-from ix-render)
  (let ((w .w.))
    (declare (ignorable w))
    (gl-matrix-mode gl_projection)
    (with-matrix (t)
      (trc nil "ix-paint > hedron ortho" (ll self) (lr self) (lb self) (lt self))
      (gl-ortho (ll w) (lr w) (lb w) (lt w) 10000 -10000) ;*mgw-near* *mgw-far*) ;; was -+ 10k
      
      (gl-matrix-mode gl_modelview)
      (with-matrix (nil)
        (let ((shape (car (value (fm^ :shape))))
              (wireframe-p (value (fm^ :wireframe)))
              (tex-gen (or (car (value (fm^ :tex-gen)))
                         gl_sphere_map))
              (tex-wrap (or (car (value (fm^ :tex-wrap)))
                          gl_sphere_map))
              (line-width (or (value (fm^ :line-width))
                            (mkv2 4 0)))
              (scalex (or (value (fm^ :scalex))
                        (mkv2 0 0)))
              (scaley (or (value (fm^ :scaley))
                        (mkv2 0 0)))
              (scalez (or (value (fm^ :scalez))
                        (mkv2 0 0)))
              (size (or (value (fm^ :size))
                      1))
              (height (or (value (fm^ :height))
                        1))
              (base-r (or (value (fm^ :base-r))
                        1))
              (top-r (or (value (fm^ :top-r))
                       1))
              (inner-r (or (value (fm^ :inner-r))
                         0.5))
              (outer-r (or (value (fm^ :outer-r))
                         0.5))
              (sides (or (value (fm^ :sides))
                      1))
              (rings (or (value (fm^ :rings))
                      1))
              (slices (or (value (fm^ :slices))
                      1))
              (stacks (or (value (fm^ :stacks))
                      1))
              (levels (or (value (fm^ :levels))
                      1))
              )
          (if (skin self)
              (progn
                (trc nil "setting up texture" self (skin self))
                (wand-texture-activate (skin self))
                (ogl-tex-gen-setup tex-gen gl_modulate tex-wrap ;; radio group value
                  (case shape
                    (cone .5)
                    (cylinder .5)
                    (cube .5)
                    (cello ;(gl-translatef -100 0 0) ;;-1440)
                     (rpchk 'hedron t nil self)
                     ;;(trc "evaluating value" self)
                     
                     .5)
                    (torus .5)
                    (teapot .25)
                    (otherwise .5))
                  :s :tee :r
                  ))
            (gl-disable gl_texture_2d))
          
          (let ((ox 100)(oy 00) (oz 0))
            (case shape
              (cello (gl-translatef ox oy oz)))
            
            (destructuring-bind (rx ry rz) (rotation self)
              (gl-rotatef rx 1 0 0)
              (gl-rotatef ry 0 1 0)
              (gl-rotatef rz 0 0 1))
            
            (case shape
              (cello (gl-translatef (- (* 3 ox)) (- oy) (- oz)))))
          
          (let ((sc (floor (l-width self) 2)))
            (gl-scalef sc sc sc))
          
          (trc nil "lwinf" lw-p line-width)
          (gl-line-width (max 1 (* (v2-h line-width) 32)))
          
          (let ((sc 1))
            (gl-scalef (* sc (v2-h scalex)) (* sc (v2-h scaley)) (* sc (v2-h scalez))))
          
          (gl-disable gl_color_material)
          ;;(trc "hedron render stack depth" (ogl::cello-matrix-mode) (ogl::get-stack-depth))
          (apply (intern (string-upcase
                          (conc$ "glut-"
                            (if wireframe-p "wire-" "solid-")
                            (case shape
                              (4 "tetrahedron")
                              (8 "octahedron")
                              (12 "dodecahedron")
                              (20 "icosahedron")
                              (otherwise (string shape))))) :cello)
            (case shape
              (cello (list (^text-font)))
              (nurb (list (^nurb)))
              (cone (list base-r height (round slices) (round stacks)))
              (cylinder (list (quadric self) base-r top-r height (round slices) (round stacks)))
              ((cube teapot) (list size))
              (sierpinski-sponge (list (min 10 (round levels)) *sponge-offset* 5))
              (torus (list inner-r outer-r (round sides) (round rings)))
              (sphere (list size (round slices) (round stacks)))))
          
          )))
    
    (gl-disable gl_texture_gen_s)
    (gl-disable gl_texture_gen_t)
    (gl-disable gl_texture_gen_r)
    (gl-disable gl_texture_gen_q)
    
    #+hunh (gl-matrix-mode gl_projection))
  (gl-matrix-mode gl_modelview))

