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


(defun hedron-options ()  
  (mk-part :options (ix-inline)
          :orientation :vertical
      :spacing (upts 4)
    :justify :right
      :kids (c? (the-kids
                 (a-stack (:justify :left)
                   (mk-part :spinning (ct-check-text)
                     :title$ "spinning")
                   (mk-part :wireframe (ct-check-text)
                     :value (c-in t)
                     :title$ "wireframe"
                     :clipped nil
                     :enabled t))
                 
                 (a-stack ()
                   (a-label "line width")
                   (make-slider :line-width :initial-pcts (list (mkv2 .05 .05))))
                 
                 (a-stack ()
                   (a-label "spin")
                   (make-slider :rotx :initial-pcts (list (mkv2 .15 .15)))
                   (make-slider :roty :initial-pcts (list (mkv2 .15 .15)))
                   (make-slider :rotz :initial-pcts (list (mkv2 .15 .15))))
                 
                 (a-stack ()
                   (a-label "scale")
                   (make-slider :scalex)
                   (make-slider :scaley)
                   (make-slider :scalez))
                 
                 (a-stack (:spacing (upts 8)
                               :justify :right)
                   
                   (a-stack ()
                     (a-label "color")
                     (make-rgba-mixer :hedro-color :alpha 1 :init-all .5))
                   
                   (a-stack (:collapsed t)
                     (a-label "specular")
                     (make-rgba-mixer :hedro-specular :init-all .8))
                   
                   (a-stack ()
                     (a-label "shiny")
                     (make-slider :hedro-shiny)))
                 
                 (a-stack ()
                   (mk-part :lights-on (ct-check-text)
                     :value (c-in t)
                     :title$ "glowing")
                   (make-rgba-mixer :hedro-emission :init-all 0.3))
                 
                 
                 (shape-options self)
                 ))))

(defun hedron-tex-options (self)
  (mk-part :tex-options (ix-inline)
          :orientation :vertical
    :justify :left
    :kids (c? (the-kids
               (a-row ()
                 (hedron-shapes self)
                 (test-image-group :shape-backer "window-bkgs" "hedron-bkgs")
                 (test-image-group :shape-skin "Skin" "shapers" "cloudy"))
               (hedron-texxing self)))))

(defun hedron-shapes (self)
  (a-stack ()
    (a-label "Shape/Sides")
    (mk-part :scroller (ix-scroller)
      :mac-p t
      :scroll-bars '(:vertical)
      :start-size (mkv2 (uin 2)(u96ths (downs 96)))
      :resizeable nil
      :content (c? (mk-part :shape (ix-inline)
                     :orientation :vertical
                     :pre-layer (with-layers +white+ :fill)
                     :value (c-in (list 'nurb))
                     :kids (c? (loop for shape in '(nurb cube 4 8 12 rhombic-dodecahedron 20
                                                     cylinder cone sphere torus
                                                     sierpinski-sponge teapot cello)
                                   collecting (mk-part :rb (ct-text-radio-item)
                                                :radio self
                                                :associated-value shape
                                                :already-on-do nil
                                                :text-color (c? (if (^value)
                                                                    +red+ +black+))
                                                :pre-layer (c? (with-layers
                                                                   (:rgba (^text-color))))
                                                ;:lr (u8ths 6)
                                                ;;:inset (mkv2 2 4)
                                                :text$ (string-downcase
                                                        (format nil "~d" shape))))))))))

(defun hedron-texxing (self)
  (a-row (:spacing (u8ths 2))
    (a-row ()
      (let ((styles `((object . ,gl_object_linear)
                      (eye . ,gl_eye_linear)
                      (sphere . ,gl_sphere_map))))
        (mk-part :tex-gen (ct-radio-row)
          :spacing (upts 4)
          :value (c-in (list gl_object_linear))
          :clipped nil
          :kids (c? (mapcar (lambda (s)
                              (mk-part :rb (ct-radio-push-button)
                                ;;:value (c? (see-if-on self))
                                :associated-value (cdr s)
                                ;;:radio (c? (find-radio self))
                                :inset (mkv2 2 2)
                                :lr (u8ths 5)
                                :title$ (string-downcase (car s))))
                      styles)))))
    (a-row ()
      (let ((styles `((repeat . ,gl_repeat)(clamp . ,gl_clamp))))
        (mk-part :tex-wrap (ct-radio-row)
          :spacing (upts 4)
          :value (c-in (list gl_repeat))
          :clipped nil
          :kids (c? (mapcar (lambda (s)
                              (mk-part :rb (ct-radio-push-button)
                                :associated-value (cdr s)
                                :inset (mkv2 2 2)
                                :lr (u8ths 6)
                                :title$ (string-downcase (car s))))
                      styles)))))))



(defun hedron-backers (self)
  (test-image-group self :shape-backer "window-bkgs" "hedron-bkgs"))

(defun test-image-group (self md-name label$ dir-name$ &optional start$)
  (let ((jpegs (mapcan (lambda (type)
                         (directory (merge-pathnames
                                     (make-pathname :type type)
                                     (demo-image-subdir dir-name$))))
                 '("jpg" "bmp" "gif" "tif"))))
    (a-stack ()
      (a-label label$)
      (mk-part :scroller (ix-scroller)
        :mac-p t
        :scroll-bars '(:vertical)
        :start-size (mkv2 (uin 2)(u96ths (downs 96)))
        :resizeable nil  
        :content (c? (make-part md-name 'ix-inline
                             :orientation :vertical
                       :pre-layer (with-layers +white+ :fill)
                       :value (c-in (list (or (when start$
                                                 (find-if (lambda (jpeg)
                                                            (search start$ (namestring jpeg)))
                                                   jpegs))
                                             (car jpegs))))
                       :kids (c? (loop for p in jpegs
                                       collecting
                                       (mk-part :rb (ct-text-radio-item)
                                         :radio self
                                         :associated-value p
                                         :already-on-do :off
                                         :text-color (c? (if (^value)
                                                         +red+ +black+))
                                         :pre-layer (c? (with-layers
                                                            (:rgba (^text-color))))
                                         ;:lr (u8ths 6)
                                         ;;:inset (mkv2 2 4)
                                         :text$ (pathname-name p))))))))))

