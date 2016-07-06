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

(defobserver rgba-value ()
  (when old-value
    (fgn-free (rgba-fo old-value))))

(defmodel hedron (ix-styled ix-view)
  ((quadric :initform (c? (glu-new-quadric)) :reader quadric)
   (nurb :reader nurb :initform (c? (let ((nurb (glu-new-nurbs-renderer)))
                                      (assert (not (zerop nurb)))
                                      (trc "hedron got new nurbs renderer" self nurb)
                                      (glu-nurbs-property nurb glu_sampling_tolerance 25)
                                      nurb)))
   (mat-ambi-diffuse :initform nil :initarg :mat-ambi-diffuse :reader mat-ambi-diffuse)
   (mat-specular :initform nil :initarg :mat-specular :reader mat-specular)
   (mat-shiny :initform nil :initarg :mat-shiny :reader mat-shiny)
   (mat-emission :initform nil :initarg :mat-emission :reader mat-emission)
   (backdrop :reader backdrop :initarg :backdrop :initform nil))
  (:default-initargs
      :lighting :on
    :text-font (ftgl-make :extruded *gui-style-default-face* 18 96 9)
    :rotation (let ((rx 0)(ry 0)(rz 0))
                (c? (bIf (spinning (value (fm-other :spinning)))
                      (macrolet ((radj (axis ixid)
                                   `(incf ,axis
                                      (if spinning
                                          (* 10 (v2-h (value (fm-other ,ixid))))
                                        0))))
                        (when (frame-ct .togl)
                          (list (radj rx :rotx)
                            (radj ry :roty)
                            (radj rz :rotz))))
                      (list rx ry rz))))))

(defmethod ogl-dsp-list-prep progn ((self hedron))
  (trc nil "ogl-dsp-list-prep> doing hedron" self)
  (^nurb)
  (ogl-dsp-list-prep (backdrop self)))

(defmethod not-to-be :after ((self hedron))
  (bwhen (q (^quadric))
    (glu-delete-quadric q))
  (bwhen (n (^nurb))
    (glu-delete-nurbs-renderer n)))

(defmethod display-text$ ((self Hedron))
  "quick dirty ugly hack to satisfy ix-styled ogl-disp-list-prep"
  "Cello")

(defmodel rgba-mixer (ix-stack)
  ((red :cell nil :initarg :red :initform nil)
   (green :cell nil :initarg :green :initform nil)
   (blue :cell nil :initarg :blue :initform nil)
   (alpha :cell nil :initarg :alpha :initform nil)
   (init-all :cell nil :initarg :init-all :initform nil :reader init-all)
   (rgba-value :initarg :rgba-value :initform nil :reader rgba-value)
   )
  (:default-initargs
      :justify :right
    :sound `((:click . ,(lambda (self)
                          (declare (ignore self))
                          (make-sound :paths '("click") :gain .5 :source :default))))
    :value (c? (^rgba-value))
    :rgba-value (c? (make-rgba :fo (apply 'make-floatv
                                     (mapcar (lambda (k)
                                               (v2-h (value k))) (^kids)))))
    :kids (c? (mapcar (lambda (c)
                        (make-slider c
                          :initial-pcts (list (mkv2 (or (slot-value self c)
                                                      (init-all self) .5) 0))))
                '(red green blue alpha)))))

(defun make-rgba-mixer (md-name &rest iargs)
  (apply 'make-part md-name 'rgba-mixer iargs))

(defun light-panel (self)
  (a-row (:md-name 'light-panel ;; :px (u8ths 4) :py (u8ths (downs 4))
          :lb (c? (^fill-parent-down))
          :spacing (u8ths 2) :justify :top
          :outset (u8ths 1))
    
    (a-stack (:spacing (u8ths 1) :justify :right)
      (a-stack ( :justify :right)
        (a-label "Light model")
        (mk-part :light-model (rgba-mixer)
          :red .20
          :value (c? (cons gl_light_model_ambient (rgba-fo (^rgba-value))))))
      (a-label "World Color")
      (make-rgba-mixer :world-color)
      (a-row ()
        (make-lighting :light0 gl_light0 *light-pos-tl*)
        ;(make-lighting :light1 GL_LIGHT1 *LightPosTR*)
        ;(make-lighting :light2 GL_LIGHT2 *LightPosTR*)
        ;(make-lighting :light3 GL_LIGHT3 *LightPosTR*)
        ))
    
    (starter-hedron self)))

(defun make-lighting (md-name id pos)
  (make-instance 'ix-light
    :md-name md-name
    :id id
    :initial-pos pos))

(defun starter-hedron (self)
  (a-row (:outset (u8ths 1) :spacing (u8ths 1)
          :lb (c? (^fill-parent-down)))
    (hedron-options)
    (a-stack (:spacing (u8ths 1)
              :justify :left)
      (hedron-tex-options self)
      (mk-part :hedron (hedron)
        :ll (u96ths -300) :lt (ups (u96ths 300))
        :lr (u96ths 300) :lb (downs (u96ths 300))
        :clipped t
        :lighting :on
        :mat-ambi-diffuse (c? (value (fm-other :hedro-color)))
        :mat-specular (c? (value (fm-other :hedro-specular)))
        :mat-shiny (c? (v2-h (value (fm-other :hedro-shiny))))
        :mat-emission (c? (when (value (fm-other :lights-on))
                            (value (fm-other :hedro-emission))))
        :backdrop (c? (assert (not *ogl-listing-p*))
                    (wand-ensure-typed 'wand-texture
                      (car (value (fm-other :shape-backer)))
                      :tile-p nil))
        :pre-layer (with-layers
                       (:in 300)
                     +white+
                     :off (:wand (backdrop self)) :on
                     (:in 20)
                     +gray+
                     (:out 20)
                     (:mat-ambi-diffuse (^mat-ambi-diffuse))
                     (:mat-specular (^mat-specular))
                     (:mat-shiny (^mat-shiny))
                     (:mat-emission (^mat-emission))
                     +white+)
                  
        :skin (c? (wand-ensure-typed 'wand-texture
                    (car (value (fm^ :shape-skin)))))))))



(defun shape-options (self)
  (a-stack (:justify :right)
    (loop for spec in '((:size  5)(:height  5)
                        (:base-r  5) (:top-r 5)
                        (:inner-r  2)(:outer-r  5)
                        (:slices  60) (:stacks  60) (:levels 10)
                        (:sides  60) (:rings  60))
        collecting (destructuring-bind (id max) spec
                     (a-row (:collapsed (c? (not (^visible)))
                             :spacing (upts 2) :justify :center
                             :visible (c? (find id
                                            (shape-ids
                                             (car (value (without-c-dependency
                                                             (fm^ :shape))))))))
                       (a-label (string-downcase id))
                       (make-slider id
                         :value-fn (lambda (drag-pct)
                                        (* (expt (v2-h drag-pct) 2) max))))))))

(defmethod shape-ids ((shape (eql 'cone)))
  '(:base-r :height :slices :stacks))

(defmethod shape-ids ((shape (eql 'sierpinski-sponge)))
  '(:levels))

(defmethod shape-ids ((shape (eql 'cylinder)))
  '(:base-r :top-r :height :slices :stacks))

(defmethod shape-ids ((shape (eql 'torus)))
  '(:inner-r :outer-r :sides :rings))

(defmethod shape-ids ((shape (eql 'sphere)))
  '(:size :stacks :slices))

(defmethod shape-ids (other)
  (declare (ignore other))
  '(:size))

(defun find-radio (self)
  (or (upper self ct-radio)
    (break "no upper radio ~a" self)))

(defmodel ix-light (light ix-stack)
  ((initial-pos :initarg :initial-pos :initform nil :accessor initial-pos))
  (:default-initargs
    :value nil #+(or) (c? (when (value (fm-other :enabled))
                    (make-instance 'light
                      :id id)))
    :enabled (c? (value (fm-other :enabled)))
    :pos  (c? (value (fm-other :xyz-pos)))
    :ambient (c? (rgba-fo (value (fm-other :ambient))))
    :diffuse (c? (rgba-fo (value (fm-other :diffuse))))
    :specular (c? (rgba-fo (value (fm-other :specular))))
    :cutoff (c? (round (* 180 (v2-h (value (fm-other :cutoff))))))
    :spot-exp (c? (round (* 128 (v2-h (value (fm-other :spot-exponent))))))
    :justify :right
    :spacing (u16ths 1)
    :kids (c? (the-kids
               (mk-part :enabled (ct-check-text)
                 :value (c-in t)
                 :title$ "on/off";;(c? (string-downcase (string (md-name (upper self ix-light)))))
                 :clipped nil
                 :enabled t)
               (mk-part :readout (ix-text)
                 :style-id :label
                 :lr (uin 1)
                 ;;:justify-hz :right
                 :text-font (font-ftgl-ensure :texture 'arialn 10)
                 :pre-layer (with-layers +black+)
                 :text$ (c? (let ((fpos (value (fm-other :xyz-pos))))
                             (format nil "~6,,,d ~6,,,d ~6,,,d" (round (eltf fpos 0))
                                 (round (eltf fpos 1))(round (eltf fpos 2))))))
               (a-row (:md-name :xyz-pos
                       :value (c? (eko (nil "xyz c?")
                                       (let* ((ks (^kids))
                                              (xy (value (car ks))))
                                         (make-ff-array :float
                                           (pct-xlate (v2-h xy) (ll .w.) (lr .w.) .30)
                                           (pct-xlate (v2-v xy) (lb .w.) (lt .w.) .50)
                                           (eko (nil "light pos z" (v2-v (value (second ks))))
                                             (pct-xlate (v2-v (value (second ks)))
                                               *mgw-near* *mgw-far* 1.5))
                                           1)))))
                 (make-slider :xy-pos
                   :width (u8ths 5) 
                   :height (u8ths 5))
                 (make-slider :z-pos
                   :initial-pcts (list (mkv2 0 .4))
                   :width (u8ths 1) 
                   :height (u8ths 5)))
               (a-stack (:justify :right)
                 (a-label "cutoff/spot")
                 (make-slider :cutoff
                   :initial-pcts (list (mkv2 .75 0))
                   :width (u8ths 4) 
                   :height (u8ths 1))
                 (make-slider :spot-exponent
                   :initial-pcts (list (mkv2 0 0))
                   :width (u8ths 4) 
                   :height (u8ths 1)))
               (a-stack (:justify :right)
                 (a-label "ambient")
                 (make-rgba-mixer :ambient :init-all 0.1))
               (a-stack (:justify :right)
                 (a-label "diffusion")
                 (make-rgba-mixer :diffuse))
               (a-stack (:justify :right :visible nil :collapsed t)
                 (a-label "specular")
                 (make-rgba-mixer :specular))))))
