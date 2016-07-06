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

(defgeneric ix-layer-expand  (key &rest args))

(defmethod ix-layer-expand ((key (eql :rgba)) &rest args)
  `(ix-render-rgba ,(car args)))

(export! ix-render-rgba)

(defun ix-render-rgba (rgba)
  (when rgba (gl-color4fv (rgba-fo rgba))))

(defmacro def-layer-rgba-expansion (color)
  `(defmethod ix-layer-expand ((key (eql ',color)) &rest args)
     (declare (ignore args))
     `(ix-render-rgba ,',color)))


(def-layer-rgba-expansion +white+)
(def-layer-rgba-expansion +red+)
(def-layer-rgba-expansion +dark-green+)
(def-layer-rgba-expansion +green+)
(def-layer-rgba-expansion +turquoise+)
(def-layer-rgba-expansion +dark-blue+)
(def-layer-rgba-expansion +blue+)
(def-layer-rgba-expansion +light-blue+)
(def-layer-rgba-expansion +black+)
(def-layer-rgba-expansion +yellow+)
(def-layer-rgba-expansion +light-yellow+)
(def-layer-rgba-expansion +purple+)
(def-layer-rgba-expansion +gray+)
(def-layer-rgba-expansion +light-gray+)
(def-layer-rgba-expansion +dark-gray+)


(defmethod ix-layer-expand ((key (eql :fill)) &rest args)
  (declare (ignore args))
  `(ix-render-fill l-box))

(defun ix-render-fill (l-box)
  (gl-polygon-mode gl_front_and_back gl_fill)
  (gl-disable gl_blend)
  (gl-disable gl_texture_2d)
  (gl-normal3i 0 0 1)
  
  (gl-rectf (r-left l-box) (r-top l-box) (r-right l-box)(r-bottom l-box))
  )

(defmethod ix-layer-expand ((key (eql :normal-out)) &rest args)
  (declare (ignore args))
  `(gl-normal3i 0 0 1))

(defmethod ix-layer-expand ((key (eql :on)) &rest args)
  (declare (ignore args))
  `(gl-enable gl_lighting))

(defmethod ix-layer-expand ((key (eql :off)) &rest args)
  (declare (ignore args))
  `(gl-disable gl_lighting))

(defmethod ix-layer-expand ((key (eql :out)) &rest layer-params)
  `(gl-translatef 0 0 (xlout ,(car layer-params))))

(defmethod ix-layer-expand ((key (eql :in)) &rest layer-params)
  `(gl-translatef 0 0 (xlin ,(car layer-params))))

(defmethod ix-layer-expand ((self (eql :line-frame)) &rest layer-params)
  (declare (ignore layer-params))
  `(ix-render-line-frame l-box))

(defun ix-render-line-frame (l-box)
  (gl-polygon-mode gl_front gl_line)
  (gl-disable gl_blend)
  (gl-rectf (r-left l-box)(r-bottom l-box)  (r-right l-box) (r-top l-box)))

(defmethod ix-layer-expand ((ztran number) &rest layer-params)
  (declare (ignore layer-params))
  ;; (trc "render N translating Z" ztran)
  `(gl-translatef 0 0 ,ztran))
       
        
(defmethod ix-layer-expand ((key (eql :v3f)) &rest layer-params)
  (destructuring-bind (x y z) layer-params
    `(gl-translatef ,x ,y ,z)))

(defmethod ix-layer-expand ((key (eql :line-width)) &rest layer-params)
  `(gl-line-width ,(car layer-params)))

(defmethod ix-layer-expand ((self (eql :enable)) &rest layer-params)
  `(progn
     ,@(loop for gl in layer-params
             collecting
             `(gl-enable ,gl))))

(defmethod ix-layer-expand ((self (eql :disable)) &rest layer-params)
  `(progn
     ,@(loop for gl in layer-params
             collecting
             `(gl-disable ,gl))))

(defmethod ix-layer-expand ((self (eql :poly-mode)) &rest args)
  `(gl-polygon-mode ,(car args) ,(cadr args)))


(defmethod ix-layer-expand ((self (eql :nice-lines)) &rest args)
  `(progn
     (gl-disable gl_texture_2d)
     (gl-enable gl_line_smooth)
     (gl-hint gl_line_smooth_hint gl_dont_care)
     (gl-enable gl_blend)
     (gl-blend-func gl_src_alpha gl_one_minus_src_alpha)
     ,(when args
        `(gl-line-width ,(or (car args) 1)))))



(defmethod ix-layer-expand ((self (eql :rect)) &rest args)
  `(gl-rectf ,@args))

(defmethod ix-layer-expand ((self (eql :mat-ambi-diffuse)) &rest args)
  `(progn
     (gl-disable gl_color_material)
     (gl-materialfv gl_front_and_back gl_ambient_and_diffuse
       (rgba-fo ,(car args)))))

(defmethod ix-layer-expand ((self (eql :mat-specular)) &rest values)
  `(progn
     (gl-disable gl_color_material)
     (gl-materialfv gl_front gl_specular (rgba-fo ,(car values)))
     ))

(defmethod ix-layer-expand ((self (eql :mat-shiny)) &rest values)
  `(progn
     (assert (numberp ,(car values)))
     (gl-disable gl_color_material)
     (gl-materialf gl_front gl_shininess (* 128.0f0 ,(car values)))))

(defmethod ix-layer-expand ((self (eql :mat-emission)) &rest values)
  (let ((emission (gensym)))
    `(bwhen (,emission ,(car values))
       (gl-disable gl_color_material)
       (gl-materialfv gl_front gl_emission (rgba-fo ,emission)))))

(defmethod ix-layer-expand ((key (eql :texturing)) &rest args)
  (destructuring-bind (status &key name tx-size) args
    `(ix-render-texturing l-box ,status ,name ,tx-size)))

(defun ix-render-texturing (lbox status name tx-size)
  (declare (ignorable lbox))
  (ecase status
    (:on
     (ogl-tex-activate name)
     (ogl-tex-gen-setup gl_object_linear gl_modulate gl_repeat
       (/ 1 (max (car tx-size)(cdr tx-size)))
         ;;(/ 1 (max (r-width lbox)(r-height lbox)))
       :s :tee :r))
    (:off
     (gl-disable gl_texture_2d))))


(defmethod ix-layer-expand ((key (eql :sphere)) &rest args)
  (destructuring-bind (&key slices stacks wireframe) args
    `(ix-render-sphere l-box ,slices ,stacks ,wireframe)))

(defun ix-render-sphere (lbox slices stacks wireframep)
  (gl-disable gl_texture_2d)
  (with-matrix ()
    (gl-rotatef 45 0 0 1)
    (funcall (if wireframep 'glut-wire-sphere 'glut-solid-sphere)
      (round (hypotenuse (r-width lbox)(r-height lbox)) 2)
      slices stacks)))

(defun hypotenuse (a b)
  (sqrt (+ (* a a)(* b b))))

(defun ogl-vertex-normaling (e xyn x y z)
    (multiple-value-bind (xn yn zn)
        (ogl::xgl-normalize-v3f
         (* (cos e)(cos xyn))
         (* (cos e)(sin xyn))
         (sin e))
      (trc nil "e xyn normal" (radian-degrees e)(radian-degrees xyn)
        (mapcar (lambda (v) (round (* v 10)))(list xn yn zn)))
      (gl-normal3f xn yn zn)
      (gl-vertex3f x y z)))



(defmethod ix-layer-expand ((key (eql :oblong)) &rest args)
  (destructuring-bind (thickness baser
                        &key slices stacks) args
    `(ix-render-oblong l-box ,thickness ,baser ,slices ,stacks)))

(defun ix-render-oblong (lbox thickness baser slices stacks)
  (unless slices (setq slices 0))
  (unless stacks (setq stacks (if (zerop thickness)
                                  0 (min 10
                                      (max 1  ;; force 3d if nonzero thickness
                                        (round (abs thickness) 2))))))
  (when (eql (abs thickness) (abs baser))
    (setf thickness (* .99 thickness)))
  (trc nil "oblong" baser thickness etages)
      
  (loop
    with theta = (/ pi 2 slices)
    with etages = stacks ;; french floors (etages) zero = ground floor
    with lw/2 = (/ (r-width lbox) 2)
    with lh/2 = (/ (r-height lbox) 2)
    with bx = (- lw/2 baser)
    with by = (- lh/2 baser)
    for etage upto etages
    for oe = 0 then ie
    for ie = (unless (= etage etages)
               (* (/ (1+ etage) etages)
                 (/ pi 2)))
    for ii = (if (not ie)
                 0 ;; throwaway value to avoid forever testing if nil
               (+ (* (abs thickness)
                    (- 1 (cos ie)))))
        
    for ox = lw/2 then ix
    for oy = lh/2 then iy
    for oz = 0 then iz
    for oc = (cornering baser slices) then ic
    for ic = (when ie
               (cornering (- baser ii) slices))
    for ix = (- lw/2 ii)
    for iy = (- lh/2 ii)
    for iz = (when ie
               (* thickness (sin ie)))
    
    do (trc nil "staging" etage ie)
        
        
    (gl-translatef (+ (r-left lbox) lw/2)(+ (r-bottom lbox) lh/2) 0)

    (with-gl-begun ((if ie
                        gl_quad_strip
                      gl_polygon))
      
      (loop for (dx dy no-turn-p)
          in '((1 1)(-1 1)(-1 -1)(1 -1)(1 1 t))
            ;;for dbg = (and (eql dx 1)(eql dy 1)(not no-turn-p))
            do (destructuring-bind (xyn0 ix0 iy0 ox0 oy0) 
                   (cons (+ (if oc (/ theta 2) 0)
                           (ecase dx (1 (ecase dy (1 0)(-1 (/ pi -2))))
                             (-1 (ecase dy (1 (/ pi 2))(-1 pi)))))
                     (if oc
                         (case (* dx dy)
                           (1 (list (* dx ix)(* dy by)(* dx ox)(* dy by)))
                           (-1 (list (* dx bx)(* dy iy)(* dx bx)(* dy oy))))
                        (list (* dx ix)(* dy iy)(* dx ox)(* dy oy))))
                  
                 ;; --- lay-down start/only -------------
                 (when ie
                   (ogl-vertex-normaling ie xyn0 ix0 iy0 iz))
                 (ogl-vertex-normaling  oe xyn0 ox0 oy0 oz)
                 
                 (trc nil "cornering!!!!!!----------------" dx dy)
                 ;; --- corner if slices and not just finishing strip
                 
                 (unless no-turn-p
                   (trc nil "------ start ------------------" (length oc)(length ic))
                   (loop for (oxn . oyn) in oc
                       for icrem = ic then (cdr icrem)
                       for (ixn . iyn) = (car icrem)
                       for xyn upfrom (+ xyn0 theta) by theta
                          do (macrolet
                                 ((vtx (elev gx sx gy sy gz)
                                    `(progn
                                       (when (minusp (* dx dy))
                                         (rotatef ,sx ,sy))
                                       (ogl-vertex-normaling ,elev xyn
                                         (incf ,gx (* dx ,sx))
                                         (incf ,gy (* dy ,sy))
                                         ,gz))))
                               (trc nil "ocn icn" oxn oyn (car icrem))
                               (when icrem
                                 (vtx ie ix0 ixn iy0 iyn iz))
                               (vtx oe ox0 oxn oy0 oyn oz)))))))
    (gl-translatef (- (+ (r-left lbox) lw/2))
      (- (+ (r-bottom lbox) lh/2)) 0)))

(defun cornering (radius slices)
  (assert (not (minusp slices)))
  (case slices
    (0)
    (1 (list (cons (- radius) radius)))
    (otherwise
     (loop with theta = (/ pi 2 slices)
         and r^2 = (* radius radius)
         repeat slices
         for angle upfrom theta by theta
         for x1 = radius then x2
         for y1 = 0 then y2
         for x2 = (* radius (cos angle))
         for y2 = (sqrt (- r^2 (* x2 x2)))
         collect (cons (- x2 x1)(- y2 y1))))))

(defmethod ix-layer-expand ((key (eql :tri-arrow)) &rest values)
  (destructuring-bind (direction &key inset) values
    `(ix-render-tri-arrow l-box ,direction ,inset)))

(let (triangle-rect)
  (defun ix-render-tri-arrow (lbox direction inset)
    (with-r-bounds (l top r b)
      (if inset
          (setf triangle-rect (r-inset (ncopy-rect lbox triangle-rect) inset))
        lbox)
      (trc nil "triangle bounds" l top r b)
      (xlout 50)
      (gl-disable gl_texture_2d)
      (gl-polygon-mode gl_front_and_back gl_fill)
      (gl-color4fv (rgba-fo +black+))
          
      (with-gl-begun (gl_polygon)
        (ecase direction
          (:left
           (gl-vertex3f r b 0)
           (gl-vertex3f r top 0)
           (gl-vertex3f l (round (+ top b) 2) 0)
           )
          (:up
           (gl-vertex3f l b 0)
           (gl-vertex3f r b 0)
           (gl-vertex3f (round (+ l r) 2) top 0)
           )
          (:right
           (gl-vertex3f l top 0)
           (gl-vertex3f l b 0)
           (gl-vertex3f r (round (+ top b) 2) 0)
           )
          (:down
           (gl-vertex3f l top 0)
           (gl-vertex3f r top 0)
           (gl-vertex3f (round (+ l r) 2) b 0)
           )))
      (xlin 50))))
